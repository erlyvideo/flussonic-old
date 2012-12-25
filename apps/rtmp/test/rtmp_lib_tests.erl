-module(rtmp_lib_tests).

-compile(export_all).
-include("../include/rtmp.hrl").

-include_lib("eunit/include/eunit.hrl").



rtmp_lib_test_() ->
  {setup, 
  fun() ->
    application:start(crypto),
    application:start(ranch),
    application:start(rtmp),
    ok
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(rtmp),
    ok
  end,
  [{foreach, fun() -> ok end, 
  fun(_) ->
    rtmp_socket:stop_server(test_rtmp)
  end,
    [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports), lists:prefix("test_", atom_to_list(F))]
  }]
  }.



test_connect_ok() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:4555/rtmpapp"),
  rtmp_socket:close(RTMP),
  ok.

test_connect_failed() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  ?assertMatch({error, _}, rtmp_lib:connect("rtmp://localhost:4555/app")),
  ok.



test_publish() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:4555/rtmpapp"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"stream0">>),
  {ok, RTMP}.


test_play_ok_stream() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, RTMP, _StreamId} = rtmp_lib:play("rtmp://localhost:4555/rtmpapp/stream"),
  {ok, RTMP}.

test_play_ok_file() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, RTMP, _StreamId} = rtmp_lib:play("rtmp://localhost:4555/rtmpapp/file.mp4"),
  {ok, RTMP}.

test_play_failed() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  ?assertMatch({error, _}, rtmp_lib:play("rtmp://localhost:4555/rtmpapp/cam0")),
  ok.

test_play_rejected() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  ?assertMatch({error, _}, rtmp_lib:play("rtmp://localhost:4555/rtmpapp/rejected")),
  ok.

