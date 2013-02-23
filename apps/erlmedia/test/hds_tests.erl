-module(hds_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/mp4.hrl").



read_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F}, []),
  {ok, Frames} = mp4_reader:read_gop(R, 4, [1,2]),
  file:close(F),

  {ok, HDS} = hds:segment(Frames, mp4_reader:media_info(R), [{tracks, [1,2]}]),
  Frames1 = flv:read_all_frames(iolist_to_binary(HDS)),
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames1],
  Vts = [round(DTS) || #video_frame{dts = DTS, content = video} <- Frames1],

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),

  ok.



read_lang_gop_test() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F}, []),
  {ok, Frames} = mp4_reader:read_gop(R, 4, [2]),
  file:close(F),

  {ok, HDS} = hds:segment(Frames, mp4_reader:media_info(R), [{tracks, [2]},{no_metadata,true}]),
  Frames1 = flv:read_all_frames(iolist_to_binary(HDS)),
  Ats = [round(DTS) || #video_frame{dts = DTS, content = audio, flavor = frame} <- Frames1],
  ?assertEqual([], [Frame || #video_frame{content = video} = Frame <- Frames1]),
  ?assertEqual([], [Frame || #video_frame{content = metadata} = Frame <- Frames1]),

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),

  ok.


flv_manifest_test() ->
  {ok, F} = file:open("../../../priv/bunny.flv", [read,binary,raw]),
  {ok, R} = flv_reader:init({file,F}, []),
  {ok, Manifest} = hds:file_manifest(flv_reader, R),
  ?assertMatch(_ when is_binary(Manifest), Manifest),
  file:close(F).
