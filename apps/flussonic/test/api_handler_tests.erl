-module(api_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


api_handler_test_() ->
  {foreach,
  fun() ->
      Apps = [ranch, gen_tracker, cowboy, flussonic],
      [application:stop(App) || App <- Apps],
      [ok = application:start(App) || App <- Apps],
      error_logger:delete_report_handler(error_logger_tty_h),
      error_logger:delete_report_handler(error_logger),
      meck:new([flu], [{passthrough,true}]),
      inets:start(),
      ok
  end,
  fun(_) ->
    meck:unload([flu]),
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(gen_tracker)
  end,
  [
    fun test_reconf/0
    ,fun test_streams/0
    ,fun test_protected_reconf_rejected/0
    ,fun test_protected_reconf_passed/0
  ] 
  }.


set_config(Conf) ->
  {ok, Config} = flu_config:parse_config(Conf, []),
  application:set_env(flussonic, config, Config),
  cowboy:start_http(fake_http, 3, 
    [{port,5555}],
    [{dispatch,[{'_',flu_config:parse_routes(Config)}]}]
  ),
  ok.


test_protected_reconf_rejected() ->
  set_config([{api, [{admin, "admin", "pass0"}]}]),
  {ok, Reply} = httpc:request("http://127.0.0.1:5555/erlyvideo/api/reload"),
  ?assertMatch({{_,401,_},_,_}, Reply),
  ok.

test_protected_reconf_passed() ->
  set_config([{api, [{admin, "admin", "pass0"}]}]),
  meck:expect(flu, reconf, fun() -> ok end),
  {ok, Reply} = httpc:request(get, {"http://127.0.0.1:5555/erlyvideo/api/reload", 
    [{"Authorization", "Basic "++base64:encode_to_string("admin:pass0")}]}, [], []),
  ?assertMatch({{_,200,_},_,_}, Reply),
  ok.

test_reconf() ->
  set_config([api]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /erlyvideo/api/reload HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  gen_tcp:close(Sock),
  ?assertEqual(200, Code),
  ok.


test_streams() ->
  set_config([api]),
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/streams"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ok.
