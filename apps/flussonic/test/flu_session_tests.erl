-module(flu_session_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include("../src/flu_session.hrl").
-compile(export_all).



setup_flu_session() ->
  error_logger:delete_report_handler(error_logger_tty_h),
  Apps = [ranch, gen_tracker, cowboy, flussonic],
  [application:stop(App) || App <- lists:reverse(Apps)],
  [ok = application:start(App) || App <- Apps],
  % cowboy:stop_listener(fake_http),
  gen_tracker_sup:start_tracker(flu_files),
  gen_tracker_sup:start_tracker(flu_streams),


  Modules = [flu_session, fake_auth],
  meck:new(Modules, [{passthrough,true}]),
  Table = ets:new(test_sessions, [{keypos,2},public]),
  meck:expect(flu_session,table, fun() -> Table end),
  meck:expect(flu_session, timeout, fun() -> 100 end),
  meck:expect(fake_auth, init, fun(_, Req, _) -> {ok, Req, state} end),
  meck:expect(fake_auth, handle, fun(Req, _) ->
    {Code, Headers, Body} = fake_auth:reply(Req),
    {ok, R1} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, R1, undefined}
  end),
  meck:expect(fake_auth, terminate, fun(_,_) -> ok end),
  Dispatch = [{'_', [{['...'], fake_auth, []}]}],
  {ok, _} = cowboy:start_http(fake_http, 1, [{port, 6070}],
    [{dispatch, Dispatch}]),
  
  ServerConf = [{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6070/"}]}],
  {ok, ServerConfig} = flu_config:parse_config(ServerConf, undefined),
  {ok, _} = cowboy:start_http(our_http, 1, [{port, 5555}],
    [{dispatch, [{'_', flu_config:parse_routes(ServerConfig)}]}]
  ),
  {Modules}.

teardown_flu_session({Modules}) ->
  ets:delete(flu_session:table()),
  % cowboy:stop_listener(fake_http),
  application:stop(cowboy),
  application:stop(flussonic),
  application:stop(gen_tracker),
  application:stop(ranch),
  meck:unload(Modules),
  ok.  


flu_session_test_() ->
  TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))],
  {foreach, 
    fun setup_flu_session/0,
    fun teardown_flu_session/1,
    [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions]
  }.

http_mock_url() -> "http://127.0.0.1:6070/auth".

test_backend_request1() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request2() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-AuthDuration">>, <<"600">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

% test_backend_request3() ->
%   meck:expect(fake_auth, reply, fun() -> {302,[{<<"X-Name">>, <<"cam1">>}], <<"">>} end),
%   ?assertEqual({ok, <<"cam1">>, [{access,granted}]},
%     auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request4() ->
  meck:expect(fake_auth, reply, fun(_) -> {403,[], <<"">>} end),
  ?assertEqual({error, {403, "backend_denied"}, [{access,denied},{auth_time,30000},{delete_time,30000}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request5() ->
  cowboy:stop_listener(fake_http),
  ?assertEqual({error, {404, "backend_http_error"}, [{access,denied}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_request6() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-AuthDuration">>, <<"600">>},{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request7() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>, [{access,granted},{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).


test_new_session1() ->
  Session = flu_session:new_or_update([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam0">>, flu_session:url(Session)).


test_new_session2() ->
  Session = flu_session:new_or_update([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted},{name,<<"cam1">>}]),
  ?assertEqual(granted, flu_session:update_session(Session)),
  ?assertEqual(<<"cam1">>, flu_session:url(Session)).




test_remember_positive_with_changing_reply() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(fake_auth, reply, fun(_) -> {403,[], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).



test_cached_positive() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(_) ->
    Self ! backend_request,
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"10">>}], <<"">>} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),

  receive
    backend_request -> error(backend_was_requested_twice)
  after
    100 -> ok
  end,

  ok.



test_remember_negative() ->
  meck:expect(fake_auth, reply, fun(_) -> {403,[], <<"">>} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(fake_auth, reply, fun(_) -> {200,[], <<"">>} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  ok.


test_monitor_session() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[], <<"">>} end),
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


test_backend_down() ->
  ?assertMatch({error,403,_}, flu_session:verify("http://127.0.0.5/", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).

test_session_info() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  Info = flu_session:info([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}]),
  ?assertEqual(<<"cam0">>, proplists:get_value(name, Info)),
  ?assertEqual(15, proplists:get_value(user_id, Info)),
  ?assertEqual(granted, proplists:get_value(flag, Info)),
  ok.

% test_session_with_rewrite_info() ->
%   meck:expect(fake_auth, reply, fun() -> {302,[{<<"X-UserId">>,<<"15">>},{<<"X-Name">>,<<"cam5">>}], <<"">>} end),
%   ?assertEqual({ok, <<"cam5">>},
%     flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
%   Info = flu_session:info([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}]),
%   ?assertEqual(<<"cam5">>, proplists:get_value(name, Info)),
%   ?assertEqual(15, proplists:get_value(user_id, Info)),
%   ?assertEqual(granted, proplists:get_value(flag, Info)),
%   ok.

assertBackendRequested(Msg) ->
  receive
    backend_request -> ok
  after
    100 -> error(Msg)
  end.



test_backend_arguments() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(Req) ->
    {QsVals, _} = cowboy_req:qs_vals(Req),
    % ?debugFmt("qs_vals: ~p", [QsVals]),
    Self ! {backend_request, QsVals},
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/vod/bunny.mp4/manifest.f4m?token=123", [
    {"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,200,_}, _, _}, Reply),

  Qs = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,
  ?assertEqual(<<"123">>, proplists:get_value(<<"token">>, Qs)),
  ?assertEqual(<<"bunny.mp4">>, proplists:get_value(<<"name">>, Qs)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(<<"ip">>, Qs)),
  ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(<<"referer">>, Qs)),
  ok.


test_backend_is_working_without_options() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(Req) ->
    {QsVals, _} = cowboy_req:qs_vals(Req),
    Self ! {backend_request, QsVals},
    {200,[], <<"">>} 
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/vod/bunny.mp4/manifest.f4m?token=123", [
    {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,200,_}, _, _}, Reply),

  Qs = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,
  ?assertEqual(<<"123">>, proplists:get_value(<<"token">>, Qs)),
  ?assertEqual(<<"bunny.mp4">>, proplists:get_value(<<"name">>, Qs)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(<<"ip">>, Qs)),
  ?assertEqual(false, lists:keyfind(<<"referer">>, 1, Qs)),
  ok.


test_expire_and_delete_session() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(_) ->
    Self ! backend_request,
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),
  ets:delete_all_objects(flu_session:table()),

  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),

  assertBackendRequested(backend_wasnt_requested_second_time),
  ok.




test_rerequest_expiring_session() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(_) ->
    Self ! backend_request,
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),
  Now = flu:now_ms(),
  #session{} = Session = flu_session:find_session(Identity),
  Session1 = Session#session{last_access_time = Now - 6000}, % a bit more than auth duration
  ets:insert(flu_session:table(), Session1),

  ?assertEqual({ok, <<"cam0">>},
    flu_session:verify(http_mock_url(), Identity, [])),

  assertBackendRequested(backend_wasnt_requested_second_time),
  ok.


test_session_is_not_destroyed_after_rerequest() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(_) ->
    Self ! backend_request,
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertEqual({ok, <<"cam0">>}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  #session{} = Session = flu_session:find_session(Identity),
  Session1 = Session#session{last_access_time = flu:now_ms() - 6000, bytes_sent = 5254}, % a bit more than auth duration
  ets:insert(flu_session:table(), Session1),

  ?assertEqual({ok, <<"cam0">>}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested_second_time),

  #session{bytes_sent = BytesSent} = flu_session:find_session(Identity),
  ?assertEqual(5254, BytesSent),
  ok.










