#!/usr/bin/env escript


main([]) ->
  Path1 = "apps/flussonic/src/flussonic.app.src",
  Path2 = "apps/flussonic/ebin/flussonic.app",
  case file:consult(Path1) of
    {ok, [{application, flussonic, Env}]} -> io:format("~s~n", [proplists:get_value(vsn,Env)]);
    {error, enoent} ->
       {ok, [{application, flussonic, Env}]} = file:consult(Path2),
       io:format("~s~n", [proplists:get_value(vsn,Env)])
  end.

