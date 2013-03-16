-module(api_handler_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


api_route_test_() ->
[

  % Now goes API

  % {"root", ?_assertMatch({api_handler, mainpage, [],_}, r(<<"/">>))}
  {"root", ?_assertMatch({api_handler, mainpage, [],_}, r(<<"/admin">>))}

  ,{"api_sendlogs", ?_assertMatch({api_handler, sendlogs, [req],_}, r(<<"/erlyvideo/api/sendlogs">>))}
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
  flu_test:setup_([{meck, [flu]}]),
  flu_test:teardown_(),
  flu_test:tests(?MODULE)
  }.


set_config(Conf) ->
  flu_test:set_config(Conf).
  % {ok, Config} = flu_config:parse_config(Conf, []),
  % application:set_env(flussonic, config, Config),
  % flu:start_webserver([{http,5555}|Config]).


test_protected_reconf_rejected() ->
  set_config([{api, [{admin, "admin", "pass0"}]}]),
  {ok, Reply} = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/reload", get, [], 1000),
  ?assertMatch({{401,_},_,_}, Reply),
  ok.

test_protected_reconf_passed() ->
  set_config([{api, [{admin, "admin", "pass0"}]}]),
  meck:expect(flu, reconf, fun() -> ok end),
  {ok, Reply} = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/reload", get,
    [{"Authorization", "Basic "++base64:encode_to_string("admin:pass0")}], 1000),
  ?assertMatch({{200,_},_,_}, Reply),
  ok.

test_reconf() ->
  set_config([api]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /erlyvideo/api/reload HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  gen_tcp:close(Sock),
  ?assertEqual(200, Code),
  ok.


test_streams() ->
  set_config([api]),
  Reply = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/streams", get, [], 1000),
  ?assertMatch({ok, {{200,_}, _, _}}, Reply),
  ok.



test_media_info() ->
  set_config([{file, "vod", "../../../priv"}, api]),
  Reply = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/media_info/vod/bunny.mp4", get, [], 1000),
  ?assertMatch({ok, {{200,_}, _, _}}, Reply),
  ok.

test_stream_health() ->
  ?assertMatch({ok, {{424,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/stream_health/ustream", get, [], 1000)),
  set_config([{stream, "ustream", "passive://"}]),
  ?assertMatch({ok, {{424,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/stream_health/ustream", get, [], 1000)),
  {ok,Pid} = flu_stream:autostart(<<"ustream">>),
  Pid ! flu_test:media_info(),
  [Pid ! F || F <- flu_test:gop(1)],
  gen_server:call(Pid, {get,media_info}),
  ?assertMatch({ok, {{200,_}, _, <<"true\n">>}}, lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/stream_health/ustream", get, [], 1000)),
  erlang:exit(Pid,shutdown),
  ok.




test_server_info() ->
  set_config([api]),
  Reply = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/server", get, [], 1000),
  ?assertMatch({ok, {{200,_}, _, _}}, Reply),
  ok.


test_sessions() ->
  set_config([api]),
  {ok, {{200,_}, _, Json1}} = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {struct, Sess1} = mochijson2:decode(Json1),
  ?assertEqual([], proplists:get_value(<<"sessions">>, Sess1)),
  ?assertEqual(<<"user.list">>, proplists:get_value(<<"event">>, Sess1)),


  set_config([{stream, "ustream", "passive://"}]),
  {ok,Pid} = flu_stream:autostart(<<"ustream">>),
  {ok, M} = gen_server:call(Pid, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  Pid ! flu_test:media_info(),
  [Pid ! F || F <- flu_test:gop(1)],
  gen_server:call(Pid, {get, media_info}),


  {ok, Sock} = gen_tcp:connect("localhost", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /ustream/mpegts HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, 200, _}} = gen_tcp:recv(Sock, 0, 1000),


  {ok, {{200,_}, _, Json2}} = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {struct, Sess2} = mochijson2:decode(Json2),
  [{struct,Session2}] = proplists:get_value(<<"sessions">>, Sess2),

  ?assertEqual(<<"mpegts">>, proplists:get_value(<<"type">>,Session2)),
  ?assertEqual(<<"ustream">>, proplists:get_value(<<"name">>,Session2)),

  erlang:exit(Pid, shutdown),
  ok.



test_stream_restart() ->
  set_config([{stream, "chan0", "passive://"}, {api, [{admin,"admin","pass0"}]}]),
  ?assertEqual([], flu_stream:list()),
  {ok, _Pid1} = flu_stream:autostart(<<"chan0">>),
  ?assertMatch([_], flu_stream:list()),
  Reply1 = lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/stream_restart/chan0", get, [], 1000),
  ?assertMatch({ok, {{401,_}, _, _}}, Reply1),
  Reply2 = lhttpc:request("http://admin:pass0@127.0.0.1:5670/erlyvideo/api/stream_restart/chan0", get, [], 1000),
  ?assertMatch({ok, {{200,_}, _, _}}, Reply2),
  ?assertEqual([], flu_stream:list()),
  ok.


test_mainpage_admin() ->
  filelib:ensure_dir("priv/index.html"),
  file:write_file("priv/index.html", "good"),
  set_config([api, {http_auth, "login", "pass"}]),
  ?assertMatch({ok, {{401,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/admin", get, [], 1000)),
  ?assertMatch({ok, {{200,_}, _, _}}, lhttpc:request("http://login:pass@127.0.0.1:5670/admin", get, [], 1000)),
  ok.

test_mainpage() ->
  filelib:ensure_dir("priv/index.html"),
  file:write_file("priv/index.html", "good"),
  set_config([api, {http_auth, "login", "pass"}]),
  ?assertMatch({ok, {{401,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/", get, [], 1000)),
  ?assertMatch({ok, {{200,_}, _, _}}, lhttpc:request("http://login:pass@127.0.0.1:5670/", get, [], 1000)),
  ok.


test_events_sse() ->
  set_config([api, {http_auth, "login", "pass"}]),
  ?assertMatch({ok, {{401,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/events", get, [{"accept", "text/event-stream"}], 1000)),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{active,false},{packet,http}]),
  gen_tcp:send(Sock, ["GET /erlyvideo/api/events HTTP/1.1\r\n" "Host: localhost:8080\r\n" "Accept: text/event-stream\r\n"
    "Authorization: Basic ",base64:encode("login:pass"), "\r\n"  "\r\n"]),
  {ok, Reply} = gen_tcp:recv(Sock, 0, 500),
  ?assertMatch({http_response,_,200,_}, Reply),
  gen_tcp:close(Sock),
  ok.


dvr_status_test_() ->
  Spec = {setup, flu_test:setup_(),
  flu_test:teardown_(),
  [{"dvrtest_bad_status", fun dvrtest_bad_status/0}]},

  case file:read_file_info("../../dvr") of
    {ok, _} -> Spec;
    {error, _} -> []
  end.


dvrtest_bad_status() ->
  flu_test:set_config([{rewrite, "chan0", "passive://"}]),
  ?assertMatch({ok, {{424,_}, _, _}}, lhttpc:request("http://127.0.0.1:5670/erlyvideo/api/dvr_status/2012/09/15/chan0", get, [], 1000)),
  ok.











