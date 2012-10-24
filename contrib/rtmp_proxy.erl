#!/usr/bin/env escript
%%! 


main([ListenPort, Forward]) ->
  Root = filename:join(filename:dirname(escript:script_name()), ".."),
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/apps/*/ebin")],
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/deps/*/ebin")],
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  lager:start(),
  application:start(rtmp),
  {ok, Pid} = rtmp_proxy:run(list_to_integer(ListenPort), Forward),
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _, process, Pid, normal} -> ok;
    {'DOWN', _, process, Pid, Reason} -> io:format("Died: ~p~n", [Reason]), ok
  end,
  ok;
  
main(_) ->
  io:format("ListenPort ForwardHost ~n").
