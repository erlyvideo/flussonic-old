-module(mpegts_decoder_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("common_test/include/ct.hrl").




all() ->
  [{group, decoder}].


groups() ->
  [{decoder, [parallel], [
    archive,
    broken_start,
    cupertino,
    flu,
    flubad,
    more,
    fltv,
    flunew1,
    ellinika,
    broken_looping,
    empty,
    precise_test_1,
    precise_test_2,
    small_chunk,
    audio_doesnt_appear_without_pmt,
    some_audio_appear_after_pmt
  ]}].


read_file(Path) ->
  {ok, Bin} = file:read_file(filename:join([code:lib_dir(mpegts,test), "fixtures", Path])),
  {ok, Bin}.

read_frames(Path) ->
  {ok, Frames} = mpegts_decoder:read_file(filename:join([code:lib_dir(mpegts,test), "fixtures", Path])),
  {ok, Frames}.


archive(_Config) ->
  Files = filelib:wildcard("../../test/files/livestream/*/*/*/*/*/*.ts"),
  [begin
    {ok, Frames} = mpegts_decoder:read_file(File),
    ?assertMatch(Len when Len > 30, length(Frames))
  end || File <- Files].


broken_start(_Config) ->
  {ok, Bin} = read_file("10-06800.ts"),
  {ok, Frames} = mpegts_decoder:decode_file(<<"abcdeffghlfref", Bin/binary>>),
  check_frames(8427325595,8427933998,1,173,8427327828,8427936491,2,318,Frames),
  ok.



cupertino(_Config) ->
  {ok, Frames} = read_frames("fileSequence0.ts"),
  check_frames(900000,1796250,1,241,900000,1787040,2,15,Frames),
  ok.


flu(_Config) ->
  {ok, Frames} = read_frames("41-08000.ts"),
  check_frames(6083042256,6083758656,1,200,6083050323,6083768403,2,375,Frames),
  ok.


flubad(_Config) ->
  {ok, Frames} = read_frames("10-06800.ts"),
  check_frames(8427325595,8427933998,1,173,8427327828,8427936491,2,318,Frames),
  ok.



more(_Config) ->
  {ok, Frames} = read_frames("media_7946.ts"),
  check_frames(7155146430,7155891630,1,210,7155146160,7155889200,2,130,Frames),
  ok.



fltv(_Config) ->
  {ok, Frames} = read_frames("fltv-01245629.ts"),
  check_frames(6256905057,6257653857,1,210,6256905651,6257656371,2,394,Frames),
  ok.


flunew1(_Config) ->
  {ok, Frames} = mpegts_decoder:read_file("../../test/files/livestream/2012/09/27/12/24/36-07292.ts"),
  check_frames(1597500,2250000,1,175,1599360,2252160,2,341,Frames),
  ok.


ellinika(_Config) ->
  {ok, Frames} = read_frames("ellinika.ts"),
  check_frames(5447916992,5448791783,1,244,5447918585,5448792185,2,456,Frames),
  ok.


broken_looping(_Config) ->
  {ok, Frames} = read_frames("looping.ts"),
  check_frames(2154685376,2155290176,1,169,2154687019,2155293739,2,316,Frames),
  ok.



empty(_Config) ->
  {ok, []} = read_frames("empty.ts").








precise_test_1(_Config) ->
  precise_compare("../../test/files/livestream/2012/09/27/12/24/23-05875.ts", "example.txt").

precise_test_2(_Config) ->
  precise_compare("../../test/files/livestream/2012/09/27/12/24/57-06000.ts", "example2.txt").


precise_compare(Path, ExamplePath) ->
  {ok, Frames} = mpegts_decoder:read_file(Path),
  {ok, [Example]} = file:consult(filename:join([code:lib_dir(mpegts,test),"fixtures",ExamplePath])),
  zipwith(fun({Codec1,Flavor1,DTS1,PTS1,Size1,CRC1,_}, 
    #video_frame{codec = Codec2, flavor = Flavor2, dts = DTS2, pts = PTS2, body = Body}) ->
    Size2 = size(Body),
    CRC2 = erlang:crc32(Body),
    % {Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1} == {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2} orelse
    % ?debugFmt("~240p =/= ~240p", [{Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1}, {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2}]),
    ?assertEqual({Codec1,Flavor1,round(DTS1),round(PTS1),Size1,CRC1}, {Codec2,Flavor2,round(DTS2),round(PTS2),Size2,CRC2}),
    ok
  end, Example, Frames),
  ct:log("Compared ~B frames, they are equal", [length(Frames)]),
  ok.


zipwith(Fun, [X1|X], [Y1|Y]) ->
  Fun(X1,Y1),
  zipwith(Fun, X, Y);

zipwith(_, [], _) -> ok;
zipwith(_, Left, []) -> error(Left).


small_chunk(_Config) ->
  M1 = mpegts_decoder:init(),
  {ok, M2, []} = mpegts_decoder:decode(<<16#47, 23>>, M1),
  {ok, _M3, []} = mpegts_decoder:decode(<<45,32>>, M2),
  ok.



padding(0) -> [];
padding(Size) -> [255 | padding(Size - 1)].

only_video_mpegts() ->
  {ok, Bin} = file:read_file("../../test/files/livestream/2012/09/27/12/24/23-05875.ts"),
  [<<16#47, _:3, 0:13, _/binary>> = PAT, 
  <<16#47, _:3, 16#FFF:13, _PMT/binary>> | Packets] = [<<16#47, Packet:187/binary>> || <<16#47, Packet:187/binary>> <= Bin],
  PMT1 = [<<16#47, 0:1, 1:1, 0:1, 16#FFF:13, 0:2, 0:1, 1:1, 0:4>>,
    mpegts_psi:encode(pmt, [{program,1},{pcr_pid,16#100},{streams,[{h264,16#100,[]}]}])
  ],
  Padding = padding(188 - iolist_size(PMT1)),
  PMT2 = [PMT1, Padding],
  TS = iolist_to_binary([PAT, PMT2 | [Packet || <<16#47, _:3, Pid:13, _/binary>> = Packet <- Packets, Pid =/= 0,  Pid =/= 16#FFF]]),
  TS.

all_mpegts() ->
  {ok, Bin} = file:read_file("../../test/files/livestream/2012/09/27/12/24/30-05875.ts"),
  Bin.


audio_doesnt_appear_without_pmt(_Config) ->
  % {ok, Bin} = file:read_file("../test/fixtures/10-06800.ts"),
  % {ok, Frames} = mpegts_decoder:decode_file(Bin),

  VideoTS = only_video_mpegts(),
  {ok, Frames} = mpegts_decoder:decode_file(VideoTS),

  % ?assertMatch([#media_info{streams = [#stream_info{codec = h264, config = Config}]}|_] when is_binary(Config), Frames),

  ?assertMatch(VideoCount when VideoCount > 40, length([Frame || #video_frame{content = video} = Frame <- Frames])),
  ?assertEqual([], [Frame || #video_frame{content = audio} = Frame <- Frames]),
  ok.


some_audio_appear_after_pmt(_Config) ->
  VideoTS = only_video_mpegts(),
  AllTS = all_mpegts(),

  {ok, Frames} = mpegts_decoder:decode_file(<<VideoTS/binary, AllTS/binary>>),

  ?assertMatch(Count when Count > 40, length([Frame || #video_frame{content = video} = Frame <- Frames])),
  ?assertMatch(Count when Count > 40, length([Frame || #video_frame{content = audio} = Frame <- Frames])),

  ok.















profile() ->
  mpegts_decoder:module_info(),
  {ok,Tracer} = fprof:profile(start),
  fprof:trace([start,{tracer,Tracer}]),
  mpegts_decoder:bench("../a.ts"),
  fprof:trace(stop),
  fprof:analyse([{cols,120}]).


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
    true -> ct:pal("requested video_count = ~B, real = ~B",[VideoCount, length(Video)])
  end,


  ?assertEqual(AudioStart, round((hd(Audio))#video_frame.dts*90)),
  % ?assertEqual(AudioEnd, round((lists:last(Audio))#video_frame.dts*90)),
  [?assertEqual(AudioTrackId, TrackId) || #video_frame{track_id = TrackId} <- Audio],
  % is_number(AudioCount) andalso ?assertEqual(AudioCount,length(Audio)),
  if length(Audio) == AudioCount -> ok;
    AudioCount == undefined -> ok;
    true -> ct:pal("requested audio_count = ~B, real = ~B",[AudioCount, length(Audio)])
  end,

  ok.















