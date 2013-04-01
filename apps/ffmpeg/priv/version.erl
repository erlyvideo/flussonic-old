#!/usr/bin/env escript


main([]) ->
  Path1 = "src/ffmpeg.app.src",
  Path2 = "ebin/ffmpeg.app",
  case file:consult(Path1) of
    {ok, [{application, ffmpeg, Env}]} -> io:format("~s~n", [proplists:get_value(vsn,Env)]);
    {error, enoent} ->
       {ok, [{application, ffmpeg, Env}]} = file:consult(Path2),
       io:format("~s~n", [proplists:get_value(vsn,Env)])
  end.

