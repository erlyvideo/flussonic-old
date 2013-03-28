-module(api_handler_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").



all() ->
  [
    {group, route},
    {group, http}
  ].


groups() ->
  [
    {route, [], [route]},
    {http, [parallel], [
      reload,
      streams,
      media_info,
      stream_health,
      server_info,
      sessions,
      stream_restart,
      mainpage_admin,
      mainpage,
      events_sse,
      dvr_status
    ]}
  ].



init_per_group(http, Config) ->
  R = flu_test:setup([log]),
  flu_test:set_config([
    {stream, "stream1", "passive://"},
    {stream, "stream_to_restart", "passive://"},
    {rewrite, "ustream", "passive://"},
    {rewrite, "stream_with_health", "passive://"},
    {rewrite, "livestream", "passive://", [{dvr, "dvr2:../../test/files"}]},
    {file, "vod", "../../priv"},
    {http_auth, "user", "pass1"},
    {api, [{admin, "admin", "pass0"}]}
  ]),
  {ok, S} = flu_stream:autostart(<<"stream1">>),
  S ! flu_test:media_info(),
  [S ! F || F <- flu_test:gop(1)],
  [{r,R}|Config];
init_per_group(_, Config) ->
  Config.


end_per_group(http, Config) ->
  {value, {r,R}, Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1;
end_per_group(_, Config) ->
  Config.



init_per_testcase(reload, Config) ->
  meck:new(flussonic_app, [{passthrough,true}]),
  Config;
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(reload, Config) ->
  meck:unload([flussonic_app]),
  Config;
end_per_testcase(_, Config) ->
  Config.



route(_Config) ->
  {api_handler, mainpage, [],_} = r(<<"/admin">>),
  {api_handler, sendlogs, [req],_} = r(<<"/erlyvideo/api/sendlogs">>),
  {api_handler, reload, [],_} = r(<<"/erlyvideo/api/reload">>),
  undefined = r(<<"/erlyvideo/api/events">>),
  {api_handler, streams, [],_} = r(<<"/erlyvideo/api/streams">>),
  {api_handler, files, [],_} = r(<<"/erlyvideo/api/files">>),
  {api_handler, server, [],_} = r(<<"/erlyvideo/api/server">>),
  {api_handler, sessions, [req],_} = r(<<"/erlyvideo/api/sessions">>),
  {api_handler, pulse, [],_} = r(<<"/erlyvideo/api/pulse">>),
  {api_handler, health, [<<"ort/good">>],_} = r(<<"/erlyvideo/api/stream_health/ort/good">>),
  {api_handler, stream_restart, [<<"ort/good">>],_} = r(<<"/erlyvideo/api/stream_restart/ort/good">>),
  {api_handler, media_info, [<<"ort/good">>],_} = r(<<"/erlyvideo/api/media_info/ort/good">>),
  {api_handler, dvr_status, [2012,9,15,<<"ort/good">>],_} =
    r(<<"/erlyvideo/api/dvr_status/2012/9/15/ort/good">>),
  ok.

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


reload(_Config) ->
  {ok, {{401,_},_,_}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/reload", get, [], 1000),

  meck:expect(flussonic_app, load_config, fun() -> ok end),
  {ok, {{200,_},_,_}} = flu_test:request("http://admin:pass0@127.0.0.1:5670/erlyvideo/api/reload", get,
    [], 1000),
  ok.


streams(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/streams", get, [], 1000),
  {ok, {{200,_}, Headers, JSON}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/streams", get, [], 1000),
  "application/json" = proplists:get_value("content-type", Headers),
  {struct, _} = mochijson2:decode(JSON),
  ok.



media_info(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/media_info/vod/bunny.mp4", get, [], 1000),
  {ok, {{200,_}, Headers, JSON}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/media_info/vod/bunny.mp4", get, [], 1000),
  "application/json" = proplists:get_value("content-type", Headers),
  {struct, Info} = mochijson2:decode(JSON),
  240 = proplists:get_value(<<"width">>,Info),
  160 = proplists:get_value(<<"height">>,Info),
  ok.



stream_health(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/stream_health/stream_with_health", get, [], 1000),
  {ok, {{424,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/stream_health/stream_with_health", get, [], 1000),

  {ok,Pid} = flu_stream:autostart(<<"stream_with_health">>),
  Pid ! flu_test:media_info(),

  {ok, {{424,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/stream_health/stream_with_health", get, [], 1000),

  [Pid ! F || F <- flu_test:gop(1)],
  gen_server:call(Pid, {get,media_info}),
  % io:fwrite(user, "Stream info: ~p, ~p", [process_info(Pid,dictionary), ]),
  {ok, {{200,_}, _, <<"true\n">>}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/stream_health/stream_with_health", get, [], 1000),
  flu_stream:stop(<<"stream_with_health">>),
  ok.




server_info(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/server", get, [], 1000),
  {ok, {{200,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/server", get, [], 1000),
  ok.


sessions(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {ok, {{200,_}, _, Json1}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {struct, Sess1} = mochijson2:decode(Json1),
  [], proplists:get_value(<<"sessions">>, Sess1),
  <<"user.list">> = proplists:get_value(<<"event">>, Sess1),


  % {ok,Pid} = flu_stream:find(<<"stream1">>),
  % {ok, M} = gen_server:call(Pid, start_monotone),
  % gen_server:call(M, {set_start_at,{0,0,0}}),

  % gen_server:call(Pid, {get, media_info}),


  {ok, Sock} = gen_tcp:connect("localhost", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /stream1/mpegts HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, 200, _}} = gen_tcp:recv(Sock, 0, 1000),


  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {ok, {{200,_}, _, Json2}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/sessions", get, [], 1000),
  {struct, Sess2} = mochijson2:decode(Json2),
  [{struct,Session2}] = proplists:get_value(<<"sessions">>, Sess2),

  <<"mpegts">> = proplists:get_value(<<"type">>,Session2),
  <<"stream1">> = proplists:get_value(<<"name">>,Session2),

  ok.



stream_restart(_Config) ->
  {ok, Pid1} = flu_stream:autostart(<<"stream_to_restart">>),
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/stream_restart/stream_to_restart", get, [], 1000),
  {ok, {{200,_}, _, <<"true\n">>}} = flu_test:request("http://admin:pass0@127.0.0.1:5670/erlyvideo/api/stream_restart/stream_to_restart", get, [], 1000),
  case flu_stream:find(<<"stream_to_restart">>) of
    {ok, Pid2} when Pid2 =/= Pid1 -> ok;
    undefined -> ok;
    {ok, Pid1} -> error({stream_not_restarted,Pid1})
  end,
  ok.


mainpage_admin(_Config) ->
  filelib:ensure_dir("priv/index.html"),
  file:write_file("priv/index.html", "good"),
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/admin", get, [], 1000),
  {ok, {{200,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/admin", get, [], 1000),
  ok.

mainpage(_Config) ->
  filelib:ensure_dir("priv/index.html"),
  file:write_file("priv/index.html", "good"),
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/", get, [], 1000),
  {ok, {{200,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/", get, [], 1000),
  ok.


events_sse(_Config) ->
  {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/events", get, [{"accept", "text/event-stream"}], 1000),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{active,false},{packet,http}]),
  gen_tcp:send(Sock, ["GET /erlyvideo/api/events HTTP/1.1\r\n" "Host: localhost:8080\r\n" "Accept: text/event-stream\r\n"
    "Authorization: Basic ",base64:encode("user:pass1"), "\r\n"  "\r\n"]),
  {ok, Reply} = gen_tcp:recv(Sock, 0, 500),
  ?assertMatch({http_response,_,200,_}, Reply),
  gen_tcp:close(Sock),
  ok.



dvr_status(_Config) ->
  case code:is_loaded(dvr) of
    false -> code:load_file(dvr);
    _ -> ok
  end,
  case code:is_loaded(dvr) of
    false ->
      {skip, "No dvr application"};
    _ ->
      {ok, {{401,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/erlyvideo/api/dvr_status/2012/09/15/ustream", get, [], 1000),
      {ok, {{424,_}, _, _}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/dvr_status/2012/09/15/ustream", get, [], 1000),
      {ok, {{200,_}, _, JSON}} = flu_test:request("http://user:pass1@127.0.0.1:5670/erlyvideo/api/dvr_status/2012/09/27/livestream", get, [], 1000),
      Minutes = mochijson2:decode(JSON),
      length(Minutes) >= 10 orelse error({too_few_minutes,length(Minutes)}),
      ok
  end.











