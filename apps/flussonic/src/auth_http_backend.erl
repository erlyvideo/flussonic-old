-module(auth_http_backend).
-export([verify/3]).
-include("log.hrl").


merge(List1, List2) ->
  lists:ukeymerge(1, lists:ukeysort(1,List1), lists:ukeysort(1, List2) ).

to_s(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_s(List) when is_list(List) -> List;
to_s(Int) when is_integer(Int) -> integer_to_list(Int);
to_s(undefined) -> undefined;
to_s(Atom) when is_atom(Atom) -> atom_to_list(Atom).


to_i(undefined) -> undefined;
to_i(List) when is_list(List) -> list_to_integer(List);
to_i(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin));
to_i(Int) when is_integer(Int) -> Int.


verify(URL, Identity, Options) ->
  Query = [ [to_s(K), "=", to_s(V), "&"] || {K,V} <- Identity ++ Options, is_binary(V) orelse is_list(V) orelse is_atom(V) orelse is_integer(V)],
  RequestURL = lists:flatten([URL, "?", Query]),
  case httpc:request(get, {RequestURL, []}, [{connect_timeout, flu_session:timeout()},{timeout, flu_session:timeout()},{autoredirect,false}],
    [], auth) of
    {ok, {{_,Code,_}, Headers, _Body}} ->
      ?DBG("Backend auth request \"~s\": ~B code", [RequestURL, Code]),
      AuthDuration = to_i(proplists:get_value("x-authduration", Headers, 30))*1000,
      DeleteTime = lists:max([AuthDuration, 10000]),
      Opts0_ = [{auth_time,AuthDuration},{delete_time,DeleteTime},
        {user_id,to_i(proplists:get_value("x-userid", Headers))}],
      Opts0 = merge([{K,V} || {K,V} <- Opts0_, V =/= undefined], Options),
      % Name = to_b(proplists:get_value("x-name", Headers, proplists:get_value(name, Identity))),
      Name = proplists:get_value(name, Identity),
      case Code of
        200 -> {ok,    Name, merge([{access, granted}],Opts0)};
        302 -> {ok,    Name, merge([{access, granted}],Opts0)};
        403 -> {error, {403, "backend_denied"},  merge([{access, denied}], Opts0)};
        _ ->   {error, {403, io_lib:format("backend_code: ~B", [Code])},  merge([{access, denied}], Opts0)}
      end;
    {error, _Error} ->
      ?DBG("Backend auth request \"~s\": failed: ~p", [RequestURL, _Error]),
      {error, {404, "backend_http_error"}, merge([{access, denied}], Options)}
  end.


