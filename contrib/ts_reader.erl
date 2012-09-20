#!/usr/bin/env ERL_LIBS=apps:deps escript

-include_lib("erlmedia/include/video_frame.hrl").

-mode(compile).


-define(D(X), io:format("ts_reader:~p ~p~n", [?LINE, X])).

main([Path]) ->
  Root = filename:join(filename:dirname(escript:script_name()), ".."),
  [code:add_pathz(P) || P <- filelib:wildcard(Root ++ "/apps/*/ebin")],
  
  {ok, Bin} = file:read_file(Path),
  
  T1 = erlang:now(),
  {ok, Frames1} = decode_file(Bin),
  T2 = erlang:now(),
  io:format("~B frames (~B)~n", [length(Frames1), timer:now_diff(T2,T1)]),
  
  {ok, Reader} = mpegts_reader:init([]),
  T3 = erlang:now(),
  {ok, _Reader1, Frames2} = mpegts_reader:decode(Bin, Reader),
  T4 = erlang:now(),
  io:format("~B frames (~B)~n", [length(Frames2), timer:now_diff(T4,T3)]),
  % io:format("        new   ~30s    old    ~n", [""]),
  % dump(Frames1, Frames2),
  
  % {ok,Tracer} = fprof:profile(start),
  % fprof:trace([start,{tracer,Tracer}]),
  % decode_file(Bin),
  % fprof:trace(stop),
  % fprof:analyse([{cols,120}]),
  
  ok.


dump([#video_frame{codec = C1, dts = D1, flavor = F1,body=B1}|Frames1], [#video_frame{codec = C2, dts = D2, flavor = F2, body = B2}|Frames2]) ->
  io:format("~4s ~8s ~8B(~6B)    ~4s ~8s ~8B(~6B)~n", [C1,F1,round(D1),size(B1),C2,F2,round(D2),size(B2)]),
  dump(Frames1, Frames2);

dump([], [#video_frame{codec = C2, dts = D2, flavor = F2,body=B2}|Frames2]) ->
  io:format("~30s    ~4s ~8s ~8B(~6B)~n", ["", C2,F2,round(D2),size(B2)]),
  dump([], Frames2);

dump([#video_frame{codec = C1, dts = D1, flavor = F1,body=B1}|Frames1], []) ->
  io:format("~4s ~8s ~8B(~6B)~n", [C1,F1,round(D1),size(B1)]),
  dump(Frames1, []);

dump([], []) ->
  ok.


