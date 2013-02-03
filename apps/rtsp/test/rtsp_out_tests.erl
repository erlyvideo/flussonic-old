-module(rtsp_out_tests).
-compile(export_all).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").


rtsp_out_test_() ->
  {foreach, fun() ->
    application:start(crypto),
    application:start(ranch),
    rtsp:start_server(8854, fake_rtsp, fake_rtsp_callback)
  end, fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(ranch),
    error_logger:add_report_handler(error_logger_tty_h),
  ok
  end, [
    {"test_read", fun test_read/0}
  ]}.


test_read() ->
  {ok, RTSP} = rtsp_reader:start_link(<<"rtsp://localhost:8854/stream1">>, [{consumer,self()}]),
  {ok, MI} = rtsp_reader:media_info(RTSP),
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264}, #stream_info{codec = aac}]}, MI),

  _Frames = get_all_frames(),

  ok.


get_all_frames() ->
  case get_frame() of
    undefined -> [];
    Frame -> [Frame|get_all_frames()]
  end.

get_frame() ->
  receive 
    {'$gen_call', From, #video_frame{} = Frame} ->
      gen:reply(From, ok),
      Frame
  after 100 -> undefined end.


  