#!/usr/bin/env ERL_LIBS=../../deps:.. escript

-mode(compile).
-include("../include/mp4.hrl").
-include("../include/video_frame.hrl").

main([Path]) ->
  main([Path, "mp4_new"]);

main([Path|Args]) ->
  make:all([load]),
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  application:set_env(lager,error_logger_redirect,true),
  application:set_env(lager,crash_log,undefined),
  lager:start(),

  code:add_pathz("../hls/ebin"),

  T1 = erlang:now(),
  {OpenTime, {ok, M}} = timer:tc(fun() -> 
    file:open(Path, [read,raw,binary])
  end),
  io:format("open time ~B us\n", [OpenTime]),

  Tracks = [1,2],

  T2 = erlang:now(),
  io:format("T2: ~B\n", [timer:now_diff(T2,T1)]),

  {T, R} = timer:tc(fun() -> 
    {ok, R1} = mp4:open({file, M}, []),
    % Module:keyframes(R1, [1,2]),
    % Module:keyframes(R1, [6,7]),
    R1#mp4_media{handler = mp4}
  end),
  io:format("Time ~B ms, mem ~B MB~n", [T div 1000, erlang:external_size(R) div 1048576]),
  T3 = erlang:now(),
  io:format("T3: ~B\n", [timer:now_diff(T3,T1)]),

  {ok, Manifest} = hds:file_manifest(mp4_reader, R),
  io:format("Manifest ~p bytes~n", [iolist_size(Manifest)]),

  % io:format("MI: ~240p~n", [mp4_reader:media_info(R)]),

  T4 = erlang:now(),
  io:format("T4: ~B\n", [timer:now_diff(T4,T1)]),

  io:format("Keyframes length: ~240p, ~p~n", [length(video_frame:reduce_keyframes(mp4:keyframes(R, Tracks))), length(mp4:keyframes(R, Tracks))]),

  T5 = erlang:now(),
  io:format("T5: ~B\n", [timer:now_diff(T5,T1)]),

  N = case Args of
    [GopNum] -> list_to_integer(GopNum);
    [] -> length(video_frame:reduce_keyframes(mp4:keyframes(R, Tracks))) * 5 div 6
  end,

  KF = mp4:keyframes(R,Tracks),
  RKF = video_frame:reduce_keyframes(KF),
  {DTS,_} = lists:nth(N, RKF),
  Left = [K || {T,_} = K <- KF, T > DTS],

  io:format("Reading segment ~p, left ~p keyframes after~n", [N, Left]),

  T6 = erlang:now(),
  io:format("T6: ~B\n", [timer:now_diff(T6,T1)]),

  {ReadGopTime, {ok, Gop}} = timer:tc(fun() -> mp4:read_gop(R, N, Tracks) end),
  io:format("Gop number ~B, frames ~B: ~B us~n", [N, length(Gop), ReadGopTime]),
  T7 = erlang:now(),
  io:format("T7: ~B\n", [timer:now_diff(T7,T1)]),

  hls:segment(Gop, mp4_reader:media_info(R), []),

  T8 = erlang:now(),
  io:format("T8: ~B\n", [timer:now_diff(T8,T1)]),

  % [io:format("~4.. s ~8.. s ~8.. B ~4.. B ~8.16.0B ~B~n", [Codec, Flavor, round(DTS), round(PTS - DTS), erlang:crc32(Body), size(Body)]) || 
  % #video_frame{codec = Codec, flavor = Flavor, dts = DTS, pts = PTS, body = Body} <- Gop],
  ok.

