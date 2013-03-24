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
  application:stop(flussonic),
  ok = application:start(ranch),
  ok = application:start(flussonic),
  ok.


teardown(_) ->
  error_logger:delete_report_handler(error_logger_tty_h),
  application:stop(ranch),
  application:stop(flussonic),
  error_logger:add_report_handler(error_logger_tty_h),
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
  ?assertEqual({ok, {file, <<"vod/bunny.mp4">>, [{path,<<"priv/bunny.mp4">>}]}}, flu_media:lookup("vod/bunny.mp4")),
  ?assertEqual({error, enoent}, flu_media:lookup("securevod/bunny.mp4")),
  ok.

lookup_securevod() ->
  set_config([{file, "securevod", "priv", [{sessions, "url"}]}]),
  ?assertEqual({ok, {file, <<"securevod/bunny.mp4">>, [{path, <<"priv/bunny.mp4">>},{sessions, "url"}]}}, flu_media:lookup("securevod/bunny.mp4")),
  ?assertEqual({error, enoent}, flu_media:lookup("vod/bunny.mp4")),
  ok.

lookup_stream() ->
  set_config([{stream, "channel", "url://source", [{sessions, "url"}]}]),
  ?assertEqual({ok, {stream, <<"channel">>, [{url, <<"url://source">>},{sessions, "url"},{static,true}]}}, flu_media:lookup("channel")),
  ok.

lookup_live() ->
  set_config([{live, "prefix"}]),
  ?assertEqual({ok, {live, <<"prefix/ustream">>, [{prefix,<<"prefix">>},{clients_timeout,false}]}}, flu_media:lookup("prefix/ustream")),
  ok.

lookup_live_secure() ->
  set_config([{live, "prefix", [{sessions, "url"}]}]),
  ?assertEqual({ok, {live, <<"prefix/ustream">>, [{prefix,<<"prefix">>},{clients_timeout,false},{sessions, "url"}]}}, flu_media:lookup("prefix/ustream")),
  ok.







