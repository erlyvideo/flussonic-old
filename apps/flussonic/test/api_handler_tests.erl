-module(api_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


api_route_test_() ->
[

  % Now goes API

  % {"root", ?_assertMatch({api_handler, mainpage, [],_}, r(<<"/">>))}
  {"root", ?_assertMatch({api_handler, mainpage, [],_}, r(<<"/admin">>))}

  ,{"api_sendlogs", ?_assertMatch({api_handler, sendlogs, [],_}, r(<<"/erlyvideo/api/sendlogs">>))}
  ,{"api_reload", ?_assertMatch({api_handler, reload, [],_}, r(<<"/erlyvideo/api/reload">>))}
  ,{"api_events", ?_assertMatch(undefined, r(<<"/erlyvideo/api/events">>))}
  ,{"api_streams", ?_assertMatch({api_handler, streams, [],_}, r(<<"/erlyvideo/api/streams">>))}
  ,{"api_files", ?_assertMatch({api_handler, files, [],_}, r(<<"/erlyvideo/api/files">>))}
  ,{"api_server", ?_assertMatch({api_handler, server, [],_}, r(<<"/erlyvideo/api/server">>))}
  ,{"api_sessions", ?_assertMatch({api_handler, sessions, [req],_}, r(<<"/erlyvideo/api/sessions">>))}
  ,{"api_pulse", ?_assertMatch({api_handler, pulse, [],_}, r(<<"/erlyvideo/api/pulse">>))}
  ,{"api_stream_health", ?_assertMatch({api_handler, health, [<<"ort/good">>],_}, r(<<"/erlyvideo/api/stream_health/ort/good">>))}
  ,{"api_stream_restart", ?_assertMatch({api_handler, stream_restart, [<<"ort/good">>],_}, r(<<"/erlyvideo/api/stream_restart/ort/good">>))}
  ,{"api_media_info", ?_assertMatch({api_handler, media_info, [<<"ort/good">>],_}, r(<<"/erlyvideo/api/media_info/ort/good">>))}
  ,{"api_dvr_status", ?_assertMatch({api_handler, dvr_status, [2012,9,15,<<"ort/good">>],_}, 
    r(<<"/erlyvideo/api/dvr_status/2012/9/15/ort/good">>))}


].

r(Path) -> 
  case api_handler:route(Path, []) of
    {ok, {M,F,A,Meta}} -> {M,F,A,proplists:get_value(tag,Meta)};
    undefined -> undefined
  end.





api_handler_test_() ->
  {setup,
  flu_test:setup_([log, {meck, [flu]}], fun() ->
    inets:start()
  end),
  flu_test:teardown_(fun() ->
    application:stop(inets)
  end),
  flu_test:tests(?MODULE)
  }.


set_config(Conf) ->
  {ok, Config} = flu_config:parse_config(Conf, []),
  application:set_env(flussonic, config, Config),
  flu:start_webserver([{http,5555}|Config]).


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
  set_config([{stream, "chan0", "passive://"}, {api, [{admin,"admin","pass0"}]}]),
  ?assertEqual([], flu_stream:list()),
  {ok, _Pid1} = flu_stream:autostart(<<"chan0">>),
  ?assertMatch([_], flu_stream:list()),
  Reply1 = lhttpc:request("http://127.0.0.1:5555/erlyvideo/api/stream_restart/chan0", get, [], 1000),
  ?assertMatch({ok, {{401,_}, _, _}}, Reply1),
  Reply2 = lhttpc:request("http://admin:pass0@127.0.0.1:5555/erlyvideo/api/stream_restart/chan0", get, [], 1000),
  ?assertMatch({ok, {{200,_}, _, _}}, Reply2),
  ?assertEqual([], flu_stream:list()),
  ok.





