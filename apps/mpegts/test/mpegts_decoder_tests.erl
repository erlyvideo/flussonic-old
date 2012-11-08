-module(mpegts_decoder_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").


bench() ->

  Count = 100,
  {T1, _} = timer:tc(fun() ->
    run_bench(mpegts_decoder, undefined, Count)
  end),
  {T2, _} = timer:tc(fun() ->
    run_bench(mpegts_reader, undefined, Count)
  end),

  io:format("mpegts_decoder: ~p, mpegts_reader: ~p~n", [T1 div Count, T2 div Count]).




run_bench(_M, _, 0) -> ok;
run_bench(M, _, Count) ->
  {ok, F1} = M:read_file("../test/fixtures/fileSequence0.ts"),
  {ok, F2} = M:read_file("../test/fixtures/41-08000.ts"),
  {ok, F3} = M:read_file("../test/fixtures/media_7946.ts"),
  run_bench(M, {F1,F2,F3}, Count - 1).




-define(assertFloatEq(X,Y), ?assert(abs((X-Y)/(X+Y)) < 0.01)).

check_frames(VideoStart,VideoEnd,VideoTrackId,VideoCount,AudioStart,AudioEnd,AudioTrackId,AudioCount,Frames) ->
  Video = [Frame || #video_frame{content = video} = Frame <- Frames],
  Audio = [Frame || #video_frame{content = audio} = Frame <- Frames],

  ?assertFloatEq(VideoStart, (hd(Video))#video_frame.dts),
  ?assertFloatEq(VideoEnd, (hd(lists:reverse(Video)))#video_frame.dts),
  [?assertEqual(VideoTrackId, TrackId) || #video_frame{track_id = TrackId} <- Video],
  is_number(VideoCount) andalso ?assertEqual(VideoCount,length(Video)),


  ?assertFloatEq(AudioStart, (hd(Audio))#video_frame.dts),
  ?assertFloatEq(AudioEnd, (hd(lists:reverse(Audio)))#video_frame.dts),
  [?assertEqual(AudioTrackId, TrackId) || #video_frame{track_id = TrackId} <- Audio],
  is_number(AudioCount) andalso ?assertEqual(AudioCount,length(Audio)),

  ok.



% track_id_shift_test() ->
%   {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/fileSequence0.ts"),
%   #video_frame{track_id = VideoId} = hd([F || #video_frame{content = video} = F <- Frames]),
%   #video_frame{track_id = AudioId} = hd([F || #video_frame{content = audio} = F <- Frames]),
%   ?assertMatch(_ when VideoId >= 1 andalso VideoId =< 3, VideoId),
%   ?assertMatch(_ when AudioId >= 1 andalso AudioId =< 3, AudioId),
%   ok.



cupertino_test() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/fileSequence0.ts"),
  % {ok, MP4} = mp4_writer:write_frame_list(Frames, []),
  % file:write_file("a.mp4",MP4),
  % file:write_file("a.flv", [flv:header(), [flv_video_frame:to_tag(Frame) || Frame <- Frames]]),
  check_frames(10000, 19958.33, 257, 241, 10000, 19856, 258, 469, Frames),
  ok.

cupertino1_test() ->
  {ok, Frames} = mpegts_reader:read_file("../test/fixtures/fileSequence0.ts"),
  % mpegts_reader is known to loose last audio frames
  check_frames(10000, 19958.33, 257, 241, 10000, 19856, 258, undefined, Frames),
  ok.

flu_test() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/41-08000.ts"),
  check_frames(67589358.4, 67597318.4, 101, 201, 67589448.0, 67597426.7, 100, 376, Frames),
  ok.

flu1_test() ->
  {ok, Frames} = mpegts_reader:read_file("../test/fixtures/41-08000.ts"),
  check_frames(67589358.4, 67597318.4, 101, undefined, 67589448.0, 67597426.7, 100, undefined, Frames),
  ok.

more_test() ->
  {ok, Frames} = mpegts_decoder:read_file("../test/fixtures/media_7946.ts"),
  check_frames(79501707.0, 79509907.0, 256, 209, 79501624.0, 79509880.0, 257, 391, Frames),
  ok.


more1_test() ->
  {ok, Frames} = mpegts_reader:read_file("../test/fixtures/media_7946.ts"),
  check_frames(79501707.0, 79509907.0, 256, undefined, 79501624.0, 79509880.0, 257, undefined, Frames),
  ok.
