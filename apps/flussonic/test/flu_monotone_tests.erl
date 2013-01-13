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
