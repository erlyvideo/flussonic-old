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
  error_logger:delete_report_handler(error_logger_tty_h),
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
  set_config([{file,"vod","../../../priv"}]),
  ?assertMatch({ok, {file, _}}, flu_media:find_or_open("vod/bunny.mp4")),
  ok.


test_live() ->
  set_config([{live, "live"}]),
  ?assertMatch({ok, {stream, Pid}} when is_pid(Pid), flu_media:find_or_open("live/ustream")),
  ok.


lookup_test_() -> 
  Tests = [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("lookup_", atom_to_list(F))],
  {setup,
  fun() ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:start(flussonic)
  end,
  fun(_) ->
    application:stop(flussonic)
  end,
  Tests}.

lookup_vod() ->
  set_config([{file, "vod", "priv"}]),
  ?assertEqual({ok, {file, <<"bunny.mp4">>, [{root, <<"priv">>}]}}, flu_media:lookup("vod/bunny.mp4")),
  ?assertEqual({error, enoent}, flu_media:lookup("securevod/bunny.mp4")),
  ok.

lookup_securevod() ->
  set_config([{file, "securevod", "priv", [{sessions, "url"}]}]),
  ?assertEqual({ok, {file, <<"bunny.mp4">>, [{root, <<"priv">>},{sessions, "url"}]}}, flu_media:lookup("securevod/bunny.mp4")),
  ?assertEqual({error, enoent}, flu_media:lookup("vod/bunny.mp4")),
  ok.

lookup_stream() ->
  set_config([{stream, "channel", "url://source", [{sessions, "url"}]}]),
  ?assertEqual({ok, {stream, <<"channel">>, [{url, <<"url://source">>},{sessions, "url"},{static,true}]}}, flu_media:lookup("channel")),
  ok.

lookup_live() ->
  set_config([{live, "prefix"}]),
  ?assertEqual({ok, {live, <<"ustream">>, [{prefix,<<"prefix">>}]}}, flu_media:lookup("prefix/ustream")),
  ok.

lookup_live_secure() ->
  set_config([{live, "prefix", [{sessions, "url"}]}]),
  ?assertEqual({ok, {live, <<"ustream">>, [{prefix,<<"prefix">>},{sessions, "url"}]}}, flu_media:lookup("prefix/ustream")),
  ok.







