-module(mp4_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").





keyframes_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  [io:format("{~B,~B},~n", [round(T),I]) || {T,I} <- mp4:keyframes(R, [1,2])],
  ?assertMatch([
{0,_},    {2000,_}, {4000,_}, {6000,_}, {8000,_}, {10000,_},
{11875,_},{13875,_},{15750,_},{17750,_},{19750,_},{21750,_},
{23042,_},{25042,_},{27042,_},{29042,_},{31042,_},{33042,_},
{35042,_},{37042,_},{39042,_},{41042,_},{43042,_},{45042,_},
{47042,_},{49042,_},{51042,_},{53042,_},{55042,_},{56083,_},{58083,_}], 
[{round(T), I} || {T,I} <- mp4:keyframes(R, [1,2])]),
  file:close(F),
  ok.

last_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),

  ?assertEqual({error, no_segment}, mp4:read_gop(R,0,[1,2])),


  ?assertEqual(16, length(video_frame:reduce_keyframes(mp4:keyframes(R, [1])))),
  ?assertMatch({ok, [#video_frame{}|_]}, mp4:read_gop(R,16,[1])),

  ?assertEqual(16, length(video_frame:reduce_keyframes(mp4:keyframes(R, [1,2])))),
  ?assertMatch({ok, [#video_frame{}|_]}, mp4:read_gop(R,16,[1,2])),

  % ?assertEqual(16, length(video_frame:reduce_keyframes(mp4:keyframes(R, [2])))),
  % ?assertMatch({ok, [#video_frame{}|_]}, mp4:read_gop(R,16,[2])),

  ?assertEqual({error, no_segment}, mp4:read_gop(R,17,[1,2])),
  ok.



missing_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  ?assertEqual({error, no_segment}, mp4:read_gop(R, 400, [1,2])),
  file:close(F).

missing_audio_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  ?assertEqual({error, no_segment}, mp4:read_gop(R, 400, [2])),
  file:close(F).

read_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  Result = mp4:read_gop(R, 4, [1,2]),
  ?assertMatch({ok, Frames} when length(Frames) > 40, Result),

  {ok, Frames} = Result,
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames],
  Vts = [round(DTS) || #video_frame{dts = DTS, content = video} <- Frames],

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),

  file:close(F),
  ok.


proper_keyframes_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),

  {ok, Frames1} = mp4:read_gop(R, 1, [1,2]),
  ?assertMatch(#video_frame{flavor = keyframe}, lists:keyfind(2000.0, #video_frame.dts, Frames1)),
  ?assertMatch(#video_frame{flavor = keyframe, dts = 0.0}, hd(Frames1)),


  {ok, Frames2} = mp4:read_gop(R, 1, [1]),
  ?assertMatch(#video_frame{flavor = keyframe}, lists:keyfind(2000.0, #video_frame.dts, Frames2)),
  ?assertMatch(#video_frame{flavor = keyframe, dts = 0.0}, hd(Frames2)),

  file:close(F),
  ok.



read_av_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),

  %%% This is the difference. Inverted order of tracks
  Result = mp4:read_gop(R, 4, [2,1]), 
  ?assertMatch({ok, Frames} when length(Frames) > 40, Result),

  {ok, Frames} = Result,
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames],
  Vts = [round(DTS) || #video_frame{dts = DTS, content = video} <- Frames],

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),

  file:close(F),
  ok.


read_audio_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  Result = mp4:read_gop(R, 4, [2]),
  ?assertMatch({ok, Frames} when length(Frames) > 40, Result),

  {ok, Frames} = Result,
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames],
  ?assertEqual([], [Fr || #video_frame{content = video} = Fr <- Frames]),

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),

  file:close(F),
  ok.


read_video_only_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),

  %%% This is the difference. Inverted order of tracks
  Result = mp4:read_gop(R, 4, [1]), 
  ?assertMatch({ok, Frames} when length(Frames) > 40, Result),

  {ok, Frames} = Result,
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames],
  Vts = [round(DTS) || #video_frame{dts = DTS, content = video} <- Frames],

  ?assertEqual([], Ats),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),

  file:close(F),
  ok.


guess_tracks_on_good_file_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  {ok, Frames} = mp4:read_gop(R, 4, undefined),

  ?assertMatch(#video_frame{}, lists:keyfind(audio, #video_frame.content, Frames)),
  ?assertMatch(#video_frame{}, lists:keyfind(video, #video_frame.content, Frames)),
  file:close(F),
  ok.


guess_tracks_on_video_only_test() ->
  {ok, F} = file:open("../../../priv/video_only.mp4", [read,binary,raw]),
  {ok, R} = mp4:open({file,F}, []),
  {ok, Frames} = mp4:read_gop(R, 4, undefined),

  ?assertMatch(false, lists:keyfind(audio, #video_frame.content, Frames)),
  ?assertMatch(#video_frame{}, lists:keyfind(video, #video_frame.content, Frames)),
  file:close(F),
  ok.











