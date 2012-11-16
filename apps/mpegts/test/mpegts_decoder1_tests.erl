-module(mpegts_decoder1_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").


read_test_() ->
  Tests = case file:read_file_info("../test/fixtures") of
    {ok, _} ->
      TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
      lists:prefix("readtest_", atom_to_list(F))],
      [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions];
    {error, _} ->
      []
  end,
  Tests.


readtest_flu() ->
  {ok, Bin} = file:read_file("../test/fixtures/10-06800.ts"),
  {ok, _Frames} = mpegts_decoder1:decode(Bin),
  ok.
