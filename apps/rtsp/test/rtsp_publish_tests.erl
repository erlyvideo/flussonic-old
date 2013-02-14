-module(rtsp_publish_tests).
-compile(export_all).


-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").


rtsp_publish_test_() ->
  Tests = [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))],

  {foreach, fun() ->
    application:start(crypto),
    application:start(ranch),

    application:load(lager),
    application:set_env(lager,handlers,[{lager_console_backend,info}]),
    application:set_env(lager,error_logger_redirect,true),
    application:set_env(lager,crash_log,undefined),
    lager:start(),

    rtsp:start_server(8854, fake_rtsp, fake_rtsp_callback)
  end, fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(ranch),
    application:stop(lager),
    error_logger:add_report_handler(error_logger_tty_h),
  ok
  end, Tests
  }.


test_raw_publisher() ->
  {ok, S} = gen_tcp:connect("localhost", 8854, [binary,{active,false}]),
  
  % gen_tcp:send(S, "OPTIONS /publish RTSP/1.0\r\nCSeq: 1\r\n\r\n"),
  % {ok, R1} = gen_tcp:recv(S, 0),
  % ?assertMatch({ok, {rtsp,response, {200, <<"OK">>}, _, undefined}, <<>>}, rtsp:read(R1)),

  gen_tcp:send(S, rtsp_tests:announce_request()),
  {ok, R2} = gen_tcp:recv(S, 0),

  ?assertMatch({ok, {rtsp,response, {200, <<"OK">>}, _, undefined}, <<>>}, rtsp:read(R2)),

  % TODO add further publishing ideas
  ok.
