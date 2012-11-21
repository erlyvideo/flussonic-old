-module(mbr_packetizer_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


launchedFFmpeg() ->
  {links,Links} = process_info(self(),links),
  [P || P <- Links, is_port(P) andalso string:str(proplists:get_value(name, erlang:port_info(P)),"flussonic_ffmpeg") > 0].

assertFFmpeg() ->
  ?assertMatch(Ports when length(Ports) == 1, launchedFFmpeg()).

assertNotFFmpeg() ->
  ?assertMatch(Ports when length(Ports) == 0, launchedFFmpeg()).


init_test() ->
  {ok, MBR} = mbr_packetizer:init([]),
  assertFFmpeg(),
  ok = mbr_packetizer:terminate(normal, MBR),
  assertNotFFmpeg(),
  ok.
