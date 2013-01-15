-module(flu_monotone_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


big_gop_test() ->
  meck:new([flu_stream], [{passthrough,true}]),
  Self = self(),
  meck:expect(flu_stream, find, fun(<<"chan1">>) -> {ok, Self} end),
  {ok, Pid} = flu_monotone:start_link(<<"chan1">>),
  Frames = flu_rtmp_tests:h264_aac_frames(),
  [?assertEqual(ok, flu_monotone:send_frame(Pid, F)) || F <- Frames],
  flu_monotone:stop(Pid),
  meck:unload([flu_stream]),
  ok.


% FIXME
% Add test when two or more media_info in queue


double_media_info_test() ->
  meck:new([flu_stream], [{passthrough,true}]),
  Self = self(),
  meck:expect(flu_stream, find, fun(<<"chan1">>) -> {ok, Self} end),
  {ok, Pid} = flu_monotone:start_link(<<"chan1">>),
  Frames = flu_rtmp_tests:h264_aac_frames(),
  MI = video_frame:define_media_info(undefined, Frames),
  flu_monotone:send_media_info(Pid, MI),
  flu_monotone:send_media_info(Pid, MI),
  flu_monotone:send_media_info(Pid, MI),
  flu_monotone:send_media_info(Pid, MI),
  [?assertEqual(ok, flu_monotone:send_frame(Pid, F)) || F <- Frames],
  
  flu_monotone:stop(Pid),
  meck:unload([flu_stream]),
  ok.
