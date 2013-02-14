-module(api_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


api_handler_test_() ->
  {foreach,
  fun() ->
      Apps = [crypto, ranch, gen_tracker, cowboy, flussonic],
      [application:start(App) || App <- Apps],
      meck:new([flu], [{passthrough,true}]),
      inets:start(),
      ok
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    meck:unload([flu]),
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(gen_tracker),
    application:stop(inets),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end,
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))]
  }.


set_config(Conf) ->
  {ok, Config} = flu_config:parse_config(Conf, []),
  application:set_env(flussonic, config, Config),
  cowboy:start_http(fake_http, 3, 
    [{port,5555}],
    [{env,[{dispatch, cowboy_router:compile([{'_',flu_config:parse_routes(Config)}])}]}]
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



test_media_info() ->
  set_config([{file, "vod", "../../../priv"}, api]),
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/media_info/vod/bunny.mp4"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ok.


test_server_info() ->
  set_config([api]),
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/server"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ok.


test_sessions() ->
  set_config([api]),
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/sessions"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ok.



test_stream_restart() ->
  set_config([{stream, "chan0", "passive://"}, api]),
  ?assertEqual([], flu_stream:list()),
  {ok, _Pid1} = flu_stream:autostart(<<"chan0">>),
  ?assertMatch([_], flu_stream:list()),
  Reply = httpc:request("http://127.0.0.1:5555/erlyvideo/api/stream_restart/chan0"),
  ?assertMatch({ok, {{_,200,_}, _, _}}, Reply),
  ?assertEqual([], flu_stream:list()),
  ok.





