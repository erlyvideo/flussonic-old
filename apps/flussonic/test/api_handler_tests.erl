-module(api_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


api_handler_test_() ->
  {foreach,
  fun() ->
      Apps = [ranch, gen_tracker, cowboy, flussonic],
      [application:stop(App) || App <- Apps],
      [ok = application:start(App) || App <- Apps],
      Config = [{api,[]}],
      application:set_env(flussonic, config, Config),
      error_logger:delete_report_handler(error_logger_tty_h),
      error_logger:delete_report_handler(error_logger),
      inets:start(),
      cowboy:start_http(fake_http, 3, 
        [{port,5555}],
        [{dispatch,[{'_',flu_config:parse_routes(Config)}]}]
      ), 
      ok
  end,
  fun(_) ->
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(gen_tracker)
  end,
  [
    fun test_reconf/0
    ,fun test_streams/0
  ] 
  }.



test_reconf() ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /erlyvideo/api/reload HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  gen_tcp:close(Sock),
  ?assertEqual(200, Code),
  ok.


test_streams() ->
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/streams"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ok.
