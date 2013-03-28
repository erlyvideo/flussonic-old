-module(auth_http_backend).
-export([verify/3]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([http_get/1]).

http_get(URL) ->
  T = flu_session:timeout(),
  case lhttpc:request(URL, "GET", [], <<>>, T, [{connect_timeout, T},{pool,auth_backend},{pool_ensure,true}]) of
    {ok, {{Code,_}, Headers, _Body}} ->
      {ok, {Code, [{string:to_lower(K),V} || {K,V} <- Headers]}};
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
  T1 = os:timestamp(),
  case ?MODULE:http_get(RequestURL) of
    {ok, {Code, Headers}} ->
      ResponseTime = timer:now_diff(os:timestamp(),T1) div 1000,
      AuthDuration = to_i(proplists:get_value("x-authduration", Headers, 30))*1000,
      DeleteTime = lists:max([AuthDuration, 10000]),
      UniqueUid = case proplists:get_value("x-unique", Headers) of
        "true" -> [{unique,true}];
        _ -> []
      end,
      UserId = to_i(proplists:get_value("x-userid", Headers)),
      Opts0_ = [{auth_time,AuthDuration},{delete_time,DeleteTime},
        {user_id,UserId}] ++ UniqueUid,
      LogLevel = case proplists:get_value(request_type, Options) of
        update_session -> debug;
        _ -> info
      end,
      lager:log(LogLevel, [{pid,self()},{module,?MODULE},{line,?LINE}], "Backend auth request \"~s\": ~B code, duration: ~B, user_id: ~p, unique: ~p, time: ~Bms", 
        [RequestURL, Code, AuthDuration, UserId, proplists:get_value(unique,UniqueUid, false), ResponseTime]),
      Opts0 = merge([{K,V} || {K,V} <- Opts0_, V =/= undefined], Options),
      % Name = to_b(proplists:get_value("x-name", Headers, proplists:get_value(name, Identity))),
      case Code of
        200 -> {ok,    merge([],Opts0)};
        302 -> {ok,    merge([],Opts0)};
        403 -> {error, merge([{code,403}],Opts0)};
        500 -> undefined; %% Return stale cache for 500 response
        _ ->   {error, merge([{code,Code}], Opts0)}
      end;
    {error, {failed_connect, _}} -> %% Return stale cache for broken backend
      lager:warning("Auth backend is down on url ~s", [URL]),
      undefined;
    {error, econnrefused} ->
      lager:warning("Auth backend is down on url ~s", [URL]),
      undefined;
    {error, _Error} ->
      lager:warning("Backend auth request \"~s\": failed: ~p", [RequestURL, _Error]),
      {error, merge([{http_error,_Error}], Options)}
  end.


