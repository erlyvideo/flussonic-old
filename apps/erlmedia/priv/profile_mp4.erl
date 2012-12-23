#!/usr/bin/env ERL_LIBS=../../deps:.. escript

-mode(compile).

main([Path]) ->
  make:all([load]),
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  application:set_env(lager,error_logger_redirect,true),
  application:set_env(lager,crash_log,undefined),
  lager:start(),

  {ok, M} = mmap:open(Path, [binary]),

  {T, R} = timer:tc(fun() -> mp4:open({mmap, M}, []) end),
  io:format("Time ~B, mem ~B Kb~n", [T div 1000, size(term_to_binary(R)) div 1024]),
  ok.

