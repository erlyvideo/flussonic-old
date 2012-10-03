-module(flu_session_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).



setup_flu_session() ->
  meck:new([flu_config,flu_session,http_stream], [{passthrough,true}]),
  Table = ets:new(test_sessions, [{keypos,2},public]),
  meck:expect(flu_session,table, fun() -> Table end),
  meck:new(fake_auth),
  meck:expect(fake_auth, init, fun(_, Req, _) -> {ok, Req, state} end),
  meck:expect(fake_auth, handle, fun(Req, _) -> 
    {Code, Headers, Body} = fake_auth:reply(),
    {ok, R1} = cowboy_http_req:reply(Code, Headers, Body, Req),
    {ok, R1, undefined}
  end),
  meck:expect(fake_auth, terminate, fun(_,_) -> ok end),
  Dispatch = [{'_', [{['...'], fake_auth, []}]}],
  cowboy:stop_listener(fake_http),
  {ok, _} = cowboy:start_listener(fake_http, 1, cowboy_tcp_transport, [{port, 6070}],
    cowboy_http_protocol, [{dispatch, Dispatch}]),
  [flu_config,flu_session,http_stream, fake_auth].

teardown_flu_session(Modules) ->
  ets:delete(flu_session:table()),
  cowboy:stop_listener(fake_http),
  meck:unload(Modules),
  ok.  


flu_session_test_() ->
  {foreach, 
  fun setup_flu_session/0,
  fun teardown_flu_session/1,
  [
  fun test_backend_request1/0,
  fun test_backend_request2/0,
  fun test_backend_request3/0,
  fun test_backend_request4/0,
  fun test_backend_request5/0,
  fun test_backend_request6/0,
  fun test_backend_request7/0,

  fun test_new_session1/0,
  fun test_new_session2/0,

  fun test_remember_positive/0,
  fun test_remember_negative/0,

  fun test_monitor_session/0,

  fun test_session_info/0
  ]}.

http_mock_url() -> "http://127.0.0.1:6070/auth".

test_backend_request1() ->
  meck:expect(fake_auth, reply, fun() -> {200,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{referer,<<"http://ya.ru/">>}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request2() ->
  meck:expect(fake_auth, reply, fun() -> {200,[{<<"X-AuthDuration">>, <<"600">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{expire,600},{referer,<<"http://ya.ru/">>}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request3() ->
  meck:expect(fake_auth, reply, fun() -> {302,[{<<"X-Name">>, <<"cam1">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam1">>, [{access,granted}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request4() ->
  meck:expect(fake_auth, reply, fun() -> {403,[], <<"">>} end),
  ?assertEqual({error, 403, [{access,denied}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request5() ->
  cowboy:stop_listener(fake_http),
  ?assertEqual({error, 404, [{access,denied}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request6() ->
  meck:expect(fake_auth, reply, fun() -> {200,[{<<"X-AuthDuration">>, <<"600">>},{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{expire,600},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request7() ->
  meck:expect(fake_auth, reply, fun() -> {200,[{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    flu_session:backend_request(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).


test_new_session1() ->
  Session = flu_session:new_session([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam0">>, flu_session:url(Session)).


test_new_session2() ->
  Session = flu_session:new_session([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted},{name,<<"cam1">>}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam1">>, flu_session:url(Session)).




test_remember_positive() ->
  meck:expect(fake_auth, reply, fun() -> {200,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(fake_auth, reply, fun() -> {403,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).



test_remember_negative() ->
  meck:expect(fake_auth, reply, fun() -> {403,[], <<"">>} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(fake_auth, reply, fun() -> {200,[], <<"">>} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  ok.


test_monitor_session() ->
  meck:expect(fake_auth, reply, fun() -> {200,[], <<"">>} end),
  Identity = [{ip,<<"127.0.0.5">>},{token,<<"123">>},{name,<<"cam0">>}],

  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [{type,<<"rtmp">>},{pid,self()}])),
  ?assertMatch([_], flu_session:list()),
  [Info] = flu_session:list(),
  ?assertEqual(<<"127.0.0.5">>, proplists:get_value(ip, Info)),
  Session = flu_session:find_session(Identity),
  flu_session ! {'DOWN', flu_session:ref(Session), undefined, self(), undefined},
  gen_server:call(flu_session, {unregister, flu_session:ref(Session)}),
  ?assertEqual([], flu_session:list()),
  ok.




test_session_info() ->
  meck:expect(fake_auth, reply, fun() -> {302,[{<<"X-UserId">>,<<"15">>},{<<"X-Name">>,<<"cam5">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam5">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  Info = flu_session:info([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}]),
  ?assertEqual(<<"cam5">>, proplists:get_value(name, Info)),
  ?assertEqual(15, proplists:get_value(user_id, Info)),
  ?assertEqual(granted, proplists:get_value(flag, Info)),
  ok.














