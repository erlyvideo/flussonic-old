-module(mpegts_decoder_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


bench() ->

  Count = 100,
  {T1, _} = timer:tc(fun() ->
    run_bench(mpegts_decoder, undefined, Count)
  end),
  {T2, _} = timer:tc(fun() ->
    run_bench(mpegts_reader, undefined, Count)
  end),

  {T3, _} = timer:tc(fun() ->
    run_bench(mpegts_decoder1, undefined, Count)
  end),

  io:format("mpegts_decoder: ~p, mpegts_reader: ~p, mpegts_decoder1: ~p~n", [T1 div Count, T2 div Count, T3 div Count]).




run_bench(_M, _, 0) -> ok;
run_bench(M, _, Count) ->
  {ok, F1} = M:read_file("../test/fixtures/fileSequence0.ts"),
  {ok, F2} = M:read_file("../test/fixtures/41-08000.ts"),
  {ok, F3} = M:read_file("../test/fixtures/media_7946.ts"),
  run_bench(M, {F1,F2,F3}, Count - 1).




-define(assertFloatEq(X,Y), ?assert(abs((X-Y)/(X+Y)) < 0.01)).

check_frames(VideoStart,VideoEnd,VideoTrackId,VideoCount,AudioStart,_AudioEnd,AudioTrackId,AudioCount,Frames) ->
  Video = [Frame || #video_frame{content = video, flavor = F} = Frame <- Frames, lists:member(F,[keyframe,frame])],
  Audio = [Frame || #video_frame{content = audio, flavor = frame} = Frame <- Frames],

  CheckMonotonic = fun(Frames1,Tag) ->
    lists:foldl(fun(#video_frame{dts = DTS}, PrevDTS) ->
      DTS >= PrevDTS orelse error({non_monotonic_frame_dts,Tag,PrevDTS,DTS}),
      DTS;
    (#media_info{}, PrevDTS) -> PrevDTS
    end, 0, Frames1)
  end,

  CheckMonotonic(Video, video_frames),
  CheckMonotonic(Audio, audio_frames),
  CheckMonotonic(Frames, all_frames),

  ?assertEqual(VideoStart, round((hd(Video))#video_frame.dts*90)),
  ?assertEqual(VideoEnd, round((lists:last(Video))#video_frame.dts*90)),
  [?assertEqual(VideoTrackId, TrackId) || #video_frame{track_id = TrackId} <- Video],
  % is_number(VideoCount) andalso ?assertEqual(VideoCount,length(Video)),
  if length(Video) == VideoCount -> ok;
    VideoCount == undefined -> ok;
    true -> ?debugFmt("requested video_count = ~B, real = ~B",[VideoCount, length(Video)])
  end,


  ?assertEqual(AudioStart, round((hd(Audio))#video_frame.dts*90)),
  % ?assertEqual(AudioEnd, round((lists:last(Audio))#video_frame.dts*90)),
  [?assertEqual(AudioTrackId, TrackId) || #video_frame{track_id = TrackId} <- Audio],
  % is_number(AudioCount) andalso ?assertEqual(AudioCount,length(Audio)),
  if length(Audio) == AudioCount -> ok;
    AudioCount == undefined -> ok;
    true -> ?debugFmt("requested audio_count = ~B, real = ~B",[AudioCount, length(Audio)])
  end,

  ok.



% track_id_shift_test() ->
%   {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/fileSequence0.ts"),
%   #video_frame{track_id = VideoId} = hd([F || #video_frame{content = video} = F <- Frames]),
%   #video_frame{track_id = AudioId} = hd([F || #video_frame{content = audio} = F <- Frames]),
%   ?assertMatch(_ when VideoId >= 1 andalso VideoId =< 3, VideoId),
%   ?assertMatch(_ when AudioId >= 1 andalso AudioId =< 3, AudioId),
%   ok.

mpegts_test_() ->
  Tests = case file:read_file_info("../test/fixtures") of
    {ok, _} ->
      TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
      lists:prefix("readtest_", atom_to_list(F))],
      [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions];
    {error, _} ->
      []
  end,
  Tests.


readtest_cupertino() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/fileSequence0.ts"),
  check_frames(900000,1796250,1,241,900000,1787040,2,15,Frames),
  ok.


readtest_flu() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/41-08000.ts"),
  check_frames(6083042256,6083758656,1,200,6083050323,6083768403,2,375,Frames),
  ok.


readtest_flubad() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/10-06800.ts"),
  check_frames(8427325595,8427933998,1,173,8427327828,8427936491,2,318,Frames),
  ok.



readtest_more() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/media_7946.ts"),
  check_frames(7155146430,7155891630,1,210,7155146160,7155889200,2,130,Frames),
  ok.



readtest_fltv() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/fltv-01245629.ts"),
  check_frames(6256905057,6257653857,1,210,6256905651,6257656371,2,394,Frames),
  ok.


readtest_flunew1() ->
  {ok, Frames} = mpegts_decoder:read_file("../../../test/files/livestream/2012/09/27/12/24/36-07292.ts"),
  check_frames(1597500,2250000,1,175,1599360,2252160,2,341,Frames),
  ok.



precise_compare_frames1_test() ->
  precise_compare("../../../test/files/livestream/2012/09/27/12/24/23-05875.ts", "example.txt").

precise_compare_frames2_test() ->
  precise_compare("../../../test/files/livestream/2012/09/27/12/24/57-06000.ts", "example2.txt").


precise_compare(Path, ExamplePath) ->
  {ok, Frames} = mpegts_decoder:read_file(Path),
  {ok, [Example]} = file:consult("../test/fixtures/"++ExamplePath),
  zipwith(fun({Codec1,Flavor1,DTS1,PTS1,Size1,CRC1,_}, 
    #video_frame{codec = Codec2, flavor = Flavor2, dts = DTS2, pts = PTS2, body = Body}) ->
    Size2 = size(Body),
    CRC2 = erlang:crc32(Body),
    % {Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1} == {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2} orelse
    % ?debugFmt("~240p =/= ~240p", [{Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1}, {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2}]),
    ?assertEqual({Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1}, {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2}),
    ok
  end, Example, Frames),
  ok.


zipwith(Fun, [X1|X], [Y1|Y]) ->
  Fun(X1,Y1),
  zipwith(Fun, X, Y);

zipwith(_, [], _) -> ok;
zipwith(_, Left, []) -> error(Left).






