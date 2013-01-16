-module(auth_http_backend).
-export([verify/3]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([http_get/1]).

http_get(URL) ->
  T = flu_session:timeout(),
  case httpc:request(get, {URL, []}, [{connect_timeout, T},{timeout, T},{autoredirect,false}],[], auth) of
    {ok, {{_,Code,_}, Headers, _Body}} ->
      {ok, {Code, Headers}};
    {error, _} = Error ->
      Error
  end.

merge(List1, List2) ->
  lists:ukeymerge(1, lists:ukeysort(1,List1), lists:ukeysort(1, List2) ).

to_s(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_s(List) when is_list(List) -> List;
to_s(Int) when is_integer(Int) -> integer_to_list(Int);
to_s(undefined) -> undefined;
to_s(Atom) when is_atom(Atom) -> atom_to_list(Atom).

to_b(Bin) when is_binary(Bin) -> Bin;
to_b(List) when is_list(List) -> list_to_binary(List);
to_b(Int) when is_integer(Int) -> to_b(integer_to_list(Int));
to_b(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).


to_i(undefined) -> undefined;
to_i(List) when is_list(List) -> list_to_integer(List);
to_i(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin));
to_i(Int) when is_integer(Int) -> Int.

verify(URL, Identity, Options) when is_list(URL) ->
  verify(list_to_binary(URL), Identity, Options);

verify(URL, Identity, Options) when is_binary(URL) ->
  Query = lists:flatmap(fun
    ({K,V}) when is_atom(K),is_atom(V) -> [to_s(K), "=", cowboy_http:urlencode(to_b(V)), "&"];
    ({K,V}) when is_atom(K),is_binary(V) -> [to_s(K), "=", cowboy_http:urlencode(to_b(V)), "&"];
    ({K,V}) when is_atom(K),is_integer(V) -> [to_s(K), "=", cowboy_http:urlencode(to_b(V)), "&"];
    ({K,V}) when is_atom(K),is_list(V) -> try list_to_binary(V) of
      V_ -> [to_s(K), "=", cowboy_http:urlencode(V_), "&"]
    catch
      _:_ -> []
    end;
    (_) -> []
  end, Identity ++ Options),
  RequestURL = binary_to_list(iolist_to_binary([URL, "?", Query])),
  case whereis(httpc_auth) of
    undefined ->
      inets:start(),
      inets:start(httpc, [{profile,auth}]),
      httpc:set_options([{max_sessions,20},{max_keep_alive_length,100}]);
    _ ->
      ok
  end,
  case ?MODULE:http_get(RequestURL) of
    {ok, {Code, Headers}} ->
      AuthDuration = to_i(proplists:get_value("x-authduration", Headers, 30))*1000,
      DeleteTime = lists:max([AuthDuration, 10000]),
      UniqueUid = case proplists:get_value("x-unique", Headers) of
        "true" -> [{unique,true}];
        _ -> []
      end,
      UserId = to_i(proplists:get_value("x-userid", Headers)),
      Opts0_ = [{auth_time,AuthDuration},{delete_time,DeleteTime},
        {user_id,UserId}] ++ UniqueUid,
      lager:warning("Backend auth request \"~s\": ~B code, duration: ~B, user_id: ~p, unique: ~p", 
        [RequestURL, Code, AuthDuration, UserId, proplists:get_value(unique,UniqueUid, false)]),
      Opts0 = merge([{K,V} || {K,V} <- Opts0_, V =/= undefined], Options),
      % Name = to_b(proplists:get_value("x-name", Headers, proplists:get_value(name, Identity))),
      case Code of
        200 -> {ok,    merge([{access, granted}],Opts0)};
        302 -> {ok,    merge([{access, granted}],Opts0)};
        403 -> {error, {403, "backend_denied"},  merge([{access, denied}], Opts0)};
        _ ->   {error, {403, io_lib:format("backend_code: ~B", [Code])},  merge([{access, denied}], Opts0)}
      end;
    {error, _Error} ->
      lager:warning("Backend auth request \"~s\": failed: ~p", [RequestURL, _Error]),
      {error, {404, "backend_http_error"}, merge([{access, denied}], Options)}
  end.


