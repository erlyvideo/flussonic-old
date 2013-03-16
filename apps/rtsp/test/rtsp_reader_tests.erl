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
  [F#video_frame{pts = DTS + 40} || #video_frame{dts = DTS} = F <- flu_test:gop(N)].

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

  ok.


