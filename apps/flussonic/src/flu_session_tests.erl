-module(flu_session_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").




flu_session_test_() ->
  {foreach, fun() ->
    meck:new([flu_config,flu_session,http_stream], [{passthrough,true}]),
    Table = ets:new(test_sessions, [{keypos,2},public]),
    meck:expect(flu_session,table, fun() -> Table end),
    [flu_config,flu_session,http_stream]
  end, 
  fun(Modules) ->
    ets:delete(flu_session:table()),
    meck:unload(Modules)
  end,
  [
  fun test_backend_request1/0,
  fun test_backend_request2/0,
  fun test_backend_request3/0,
  fun test_backend_request4/0,
  fun test_backend_request5/0,

  fun test_new_session1/0,
  fun test_new_session2/0,

  fun test_remember_positive/0,
  fun test_remember_negative/0
  ]}.



test_backend_request1() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,200,[], <<"">>}} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{referer,<<"http://ya.ru/">>}]},
    flu_session:backend_request(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request2() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,200,[{<<"X-AuthDuration">>, <<"600">>}], <<"">>}} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{expire,600},{referer,<<"http://ya.ru/">>}]},
    flu_session:backend_request(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request3() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,302,[{<<"X-Name">>, <<"cam1">>}], <<"">>}} end),
  ?assertEqual({ok, <<"cam1">>, [{access,granted}]},
    flu_session:backend_request(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request4() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,403,[], <<"">>}} end),
  ?assertEqual({error, 403, [{access,denied}]},
    flu_session:backend_request(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request5() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {error, econnrefused} end),
  ?assertEqual({error, 404, [{access,denied}]},
    flu_session:backend_request(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).


test_new_session1() ->
  Session = flu_session:new_session([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam0">>, flu_session:url(Session)).


test_new_session2() ->
  Session = flu_session:new_session([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted},{name,<<"cam1">>}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam1">>, flu_session:url(Session)).




test_remember_positive() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,200,[], <<"">>}} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,403,[], <<"">>}} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).



test_remember_negative() ->
  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,403,[], <<"">>}} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(http_stream, request_body, fun(_, _) -> {ok, {socket,200,[], <<"">>}} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url, [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  ok.

















