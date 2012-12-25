-module(rtmp_socket_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


rtmp_server_test_() ->
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



test_connect() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, _Socket} = rtmp_socket:connect("rtmp://localhost:4555/"),
  ok.


test_funcall() ->
  rtmp_socket:start_server(4555, test_rtmp, rtmp_test_client, [[]]),
  {ok, Socket} = rtmp_socket:connect("rtmp://localhost:4555/"),
  ?assertEqual([6.0], rtmp_lib:sync_call(Socket, 0, test_call, [5])),
  ok.

