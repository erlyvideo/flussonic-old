-module(flu_media_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").



find_or_open_test_() ->
  {foreach,
  fun setup/0,
  fun teardown/1,
  [
    fun test_file/0,
    fun test_live/0
  ]
  }.


setup() ->
  application:stop(ranch),
  application:stop(gen_tracker),
  application:stop(flussonic),
  ok = application:start(ranch),
  ok = application:start(gen_tracker),
  ok = application:start(flussonic),
  gen_tracker_sup:start_tracker(flu_streams),
  gen_tracker_sup:start_tracker(flu_files),
  ok.


teardown(_) ->
  application:stop(ranch),
  application:stop(gen_tracker),
  application:stop(flussonic),
  ok.

set_config(Env) ->
  {ok, Conf} = flu_config:parse_config(Env,undefined),
  application:set_env(flussonic, config, Conf),
  ok.


test_file() ->
  set_config([{file,"vod","../../priv"}]),
  ?assertMatch({ok, {file, _}}, flu_media:find_or_open("vod/bunny.mp4")),
  ok.


test_live() ->
  set_config([{live, "live"}]),
  ?assertMatch({ok, {stream, <<"ustream">>}}, flu_media:find_or_open("live/ustream")),
  ok.
