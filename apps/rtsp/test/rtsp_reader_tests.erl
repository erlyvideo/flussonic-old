-module(rtsp_reader_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).




play_stream_test_() ->
  {setup, flu_test:setup_([{apps,[rtsp]}], fun() ->
    ok
  end),
  flu_test:teardown_(),
  flu_test:tests(?MODULE)}.


gop(N) ->
  [F#video_frame{pts = DTS + case Content of video -> 40; _ -> 0 end} || #video_frame{dts = DTS, content = Content} = F <- flu_test:gop(N)].

test_start_new_stream() ->
  flu_test:set_config([{rtsp,1556},{stream,"stream1","passive://"}]),
  {ok,S1} = flu_stream:autostart(<<"stream1">>),
  S1 ! flu_test:media_info(),
  [S1 ! F || F <- gop(1)],
  [S1 ! F || F <- gop(2)],
  [S1 ! F || F <- gop(3)],

  {ok,R} = rtsp_reader:start_link("rtsp://localhost:1556/stream1", [{consumer,self()}]),
  {ok, #media_info{streams = Streams}} = rtsp_reader:media_info(R),
  2 = length(Streams),
  {messages, []} = process_info(self(),messages),
  [S1 ! F || F <- gop(6)],

  ?assertMatch(#gop{format = mpegts}, receive M1 -> M1 end),
  ?assertMatch(#gop{format = mpegts}, receive M2 -> M2 end),
  ?assertMatch(#gop{format = mpegts}, receive M3 -> M3 end),

  Frame = receive {'$gen_call', From, #video_frame{} = F} -> gen_server:reply(From, ok), F end,
  #video_frame{codec = h264, dts = DTS, pts = PTS} = Frame,
  ?assertEqual(DTS + 40, PTS),
  flush_frames(),
  erlang:exit(S1,shutdown),
  ok.

test_start_stream_no_hls() ->
  flu_test:set_config([{rtsp,1556},{stream,"stream1","passive://", [{hls,false}]}]),
  {ok,S1} = flu_stream:autostart(<<"stream1">>),
  S1 ! flu_test:media_info(),
  [S1 ! F || F <- gop(1)],
  [S1 ! F || F <- gop(2)],
  ?assertEqual(undefined, gen_tracker:getattr(flu_streams,<<"stream1">>,hls_playlist)),

  {ok,R} = rtsp_reader:start_link("rtsp://localhost:1556/stream1", [{consumer,self()}]),
  {ok, #media_info{streams = Streams}} = rtsp_reader:media_info(R),
  2 = length(Streams),
  receive #gop{} -> error(gop_send) after 0 -> ok end,
  flush_frames(),
  erlang:exit(S1,shutdown),
  ok.

flush_frames() ->
  receive
    {'$gen_call', From, _} -> gen_server:reply(From, ok), flush_frames()
  after
    10 -> ok
  end.

read_frames() ->
  receive
    {'$gen_call', From, Frame} -> gen_server:reply(From, ok), [Frame|read_frames()]
  after
    10 -> []
  end.




test_frames_are_equal() ->
  flu_test:set_config([{rtsp,1556},{stream,"stream1","passive://"}]),
  {ok,S1} = flu_stream:autostart(<<"stream1">>),
  {ok, M} = gen_server:call(S1, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  MediaInfo1 = (flu_test:media_info())#media_info{flow_type = stream, duration = undefined},
  S1 ! MediaInfo1,
  Gop = gop(1),
  [S1 ! F || F <- Gop],
  {ok,R} = rtsp_reader:start_link("rtsp://localhost:1556/stream1", [{consumer,self()}]),
  {ok, #media_info{streams = Streams2_} = MediaInfo2_} = rtsp_reader:media_info(R),
  MediaInfo2 = MediaInfo2_#media_info{options = [], streams = [S#stream_info{options = []} || S <- Streams2_]},
  ?assertEqual(MediaInfo1, MediaInfo2),

  Frames = read_frames(),
  % ?debugFmt("\n~p\n~p", [
  %   [{C,F,round(D),round(P)} || #video_frame{codec = C, flavor = F, dts = D, pts = P} <- lists:sublist(Gop,135,10)],
  %   [{C,F,round(D),round(P)} || #video_frame{codec = C, flavor = F, dts = D, pts = P} <- lists:sublist(Frames,135,10)]
  % ]),
  ?assertEqual(lists:nth(1,Gop), lists:nth(1,Frames)),
  ?assertEqual(lists:nth(2,Gop), lists:nth(2,Frames)),
  ?assertEqual(lists:nth(3,Gop), lists:nth(3,Frames)),
  lists:zipwith3(fun(N,G1,F1) ->
    ?assertEqual({N,G1},{N,F1})
  end, lists:seq(1,length(Frames)),lists:sublist(Gop, 1, length(Frames)), Frames),
  erlang:exit(S1,shutdown),
  ok.


