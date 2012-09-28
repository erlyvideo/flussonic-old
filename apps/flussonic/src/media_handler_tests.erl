-module(media_handler_tests).

-include_lib("eunit/include/eunit.hrl").



media_handler_test_() ->
  {foreach, fun() ->
    meck:new([flu_config,media_handler], [{passthrough,true}]),
    [flu_config,media_handler]
  end, 
  fun meck:unload/1,
  [
    fun test_offline_stream_playlist/0,
    fun test_offline_rewrite_playlist/0,
    fun test_live_dvr_playlist/0
  ]}.



test_offline_stream_playlist() ->
  meck:expect(flu_config, get_config, fun() -> [{stream, <<"livestream">>, <<"fake://url">>, [{dvr, "test/files"}]}] end),
  meck:expect(media_handler, check_sessions, fun(_, Name, _) -> Name end),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>}, 
    media_handler:lookup_name([<<"livestream">>, <<"index-1234567-3600.m3u8">>], [{dvr,"test/files"}], req, [])).


test_offline_rewrite_playlist() ->
  meck:expect(flu_config, get_config, fun() -> [{rewrite, <<"livestream">>, <<"fake://url">>, [{dvr, "test/files"}]}] end),
  meck:expect(media_handler, check_sessions, fun(_, Name, _) -> Name end),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>}, 
    media_handler:lookup_name([<<"livestream">>, <<"index-1234567-3600.m3u8">>], [{dvr,"test/files"}], req, [])).


test_live_dvr_playlist() ->
  meck:expect(flu_config, get_config, fun() -> [{live, <<"live">>, [{dvr, "test/files"}]}] end),
  meck:expect(media_handler, check_sessions, fun(_, Name, _) -> Name end),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>}, 
    media_handler:lookup_name([<<"livestream">>, <<"index-1234567-3600.m3u8">>], [{dvr,"test/files"}], req, [])).

