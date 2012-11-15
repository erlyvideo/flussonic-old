-module(mpegts_decoder1_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").


read_test() ->
  {ok, Bin} = file:read_file("../test/fixtures/10-06800.ts"),
  {ok, _Frames} = mpegts_decoder1:decode(Bin),
  ok.
