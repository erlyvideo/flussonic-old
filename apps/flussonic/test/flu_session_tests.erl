-module(flu_session_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("../src/flu_session.hrl").
-compile(export_all).



setup_flu_session() ->
  Apps = [crypto, ranch, cowboy, public_key, ssl, lhttpc, flussonic, inets],
  [application:start(App) || App <- Apps],


  Modules = [flu_session, flu],
  meck:new(Modules, [{passthrough,true}]),
  Table = ets:new(test_sessions, [{keypos,2},public]),
  meck:expect(flu_session,table, fun() -> Table end),
  meck:expect(flu_session, timeout, fun() -> 100 end),
  
  ServerConf = [
    {file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6070/vodauth"}]},
    {rewrite, "cam2", "passive://localhost/", [{sessions, "http://127.0.0.1:6070/streamauth"}]},
    {live, "live", [{sessions, "http://127.0.0.1:6070/liveauth"}]}
  ],
  {ok, ServerConfig} = flu_config:parse_config(ServerConf, undefined),
  flu:start_webserver([{http,5555}|ServerConfig]),
  {Modules, Apps}.

teardown_flu_session({Modules, Apps}) ->
  error_logger:delete_report_handler(error_logger_tty_h),
  ets:delete(flu_session:table()),
  % cowboy:stop_listener(fake_http),
  [application:stop(App) || App <- lists:reverse(Apps)],
  meck:unload(Modules),
  error_logger:add_report_handler(error_logger_tty_h),
  ok.  


flu_session_test_() ->
  TestFunctions = [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))],
  {foreach, 
    fun setup_flu_session/0,
    fun teardown_flu_session/1,
    TestFunctions
  }.

http_mock_url() -> "http://127.0.0.1:6070/auth".



test_new_session1() ->
  Session = flu_session:new_or_update([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{access,granted}]),
  ?assertMatch(#session{access = granted}, Session).






test_remember_positive_with_changing_reply() ->
  meck:expect(flu_session, backend_request, fun(_,_,_) -> {ok, []} end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(flu_session, backend_request, fun(_,_,_) -> {error, [{code,403}]} end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).



test_cached_positive() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(_,_,_) ->
    Self ! backend_request,
    {ok, [{user_id,15},{auth_time, 10000}]}
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [])),

  receive
    backend_request -> error(backend_was_requested_twice)
  after
    100 -> ok
  end,

  ok.



test_remember_negative() ->
  meck:expect(flu_session, backend_request, fun(_,_,_) -> {error, [{code,403}]} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),

  meck:expect(flu_session, backend_request, fun(_,_,_) -> {ok, []} end),
  ?assertMatch({error, 403, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  ok.


test_monitor_session() ->
  meck:expect(flu_session, backend_request, fun(_,_,_) -> {ok, []} end),
  Identity = [{ip,<<"127.0.0.5">>},{token,<<"123">>},{name,<<"cam0">>}],

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [{type,<<"rtmp">>},{pid,self()}])),
  ?assertMatch([_], flu_session:list()),
  [Info] = flu_session:list(),
  ?assertEqual(<<"127.0.0.5">>, proplists:get_value(ip, Info)),
  Session = flu_session:find_session(Identity),
  flu_session ! {'DOWN', flu_session:ref(Session), undefined, self(), undefined},
  gen_server:call(flu_session, sync_call),
  ?assertEqual([], flu_session:list()),

  gen_server:call(flu_session, {unregister, flu_session:ref(Session)}),
  ok.


test_backend_down_when_no_session() ->
  ?assertMatch({error,403,_}, flu_session:verify("http://127.0.0.5/", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])).


test_backend_down_when_already_granted() ->
  meck:expect(flu_session, backend_request, fun(_,_,_) -> {ok, []} end),
  Identity = [{ip,<<"127.0.0.5">>},{token,<<"123">>},{name,<<"cam0">>}],

  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [{type,<<"rtmp">>}])),

  meck:expect(flu_session, backend_request, fun(_,_,_) -> undefined end),

  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [{type,<<"rtmp">>}])),
  ok.

test_session_info() ->
  meck:expect(flu_session, backend_request, fun(_,_,_) -> {ok, [{user_id,15}]} end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [])),
  Info = flu_session:info([{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}]),
  ?assertEqual(<<"cam0">>, proplists:get_value(name, Info)),
  ?assertEqual(15, proplists:get_value(user_id, Info)),
  ?assertEqual(granted, proplists:get_value(access, Info)),
  ok.


assertBackendRequested(Msg) ->
  receive backend_request -> ok after 100 -> error(Msg) end.

assertBackendNotRequested(Msg) ->
  receive backend_request -> error(Msg) after 50 -> ok end.


test_backend_arguments_on_file() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/vod/bunny.mp4/manifest.f4m?token=123", [
    {"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,403,_}, _, _}, Reply),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  ?assertEqual(<<"http://127.0.0.1:6070/vodauth">>, URL),
  ?assertEqual(<<"123">>, proplists:get_value(token, Identity)),
  ?assertEqual(<<"vod/bunny.mp4">>, proplists:get_value(name, Identity)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(ip, Identity)),
  ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(referer, Options)),

  ok.



test_backend_arguments_on_stream() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/cam2/manifest.f4m?token=123", [
    {"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,403,_}, _, _}, Reply),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  ?assertEqual(<<"http://127.0.0.1:6070/streamauth">>, URL),
  ?assertEqual(<<"123">>, proplists:get_value(token, Identity)),
  ?assertEqual(<<"cam2">>, proplists:get_value(name, Identity)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(ip, Identity)),
  ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(referer, Options)),

  ok.



test_backend_arguments_on_live() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/live/ustream/manifest.f4m?token=123", [
    {"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,403,_}, _, _}, Reply),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  ?assertEqual(<<"http://127.0.0.1:6070/liveauth">>, URL),
  ?assertEqual(<<"123">>, proplists:get_value(token, Identity)),
  ?assertEqual(<<"live/ustream">>, proplists:get_value(name, Identity)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(ip, Identity)),
  ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(referer, Options)),

  ok.



test_backend_is_working_without_options() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {ok, []}
  end),
  {ok, Reply} = httpc:request(get, 
    {"http://127.0.0.1:5555/vod/bunny.mp4/manifest.f4m?token=123", [
    {"X-Forwarded-For", "94.95.96.97"}]},[],[]),
  ?assertMatch({{_,200,_}, _, _}, Reply),

  {_URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,
  ?assertEqual(<<"123">>, proplists:get_value(token, Identity)),
  ?assertEqual(<<"vod/bunny.mp4">>, proplists:get_value(name, Identity)),
  ?assertEqual(<<"94.95.96.97">>, proplists:get_value(ip, Identity)),
  ?assertEqual(false, lists:keyfind(referer, 1, Options)),
  ok.


test_expire_and_delete_session() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    Self ! backend_request,
    {ok,[{user_id,15},{auth_time, 5000}]} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  [#session{session_id = Id}] = ets:tab2list(flu_session:table()),
  ets:update_element(flu_session:table(), Id, [{#session.last_access_time, 0}]),
  flu_session ! clean,
  gen_server:call(flu_session, sync_call),

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [])),

  assertBackendRequested(backend_wasnt_requested_second_time),
  ok.


test_dont_expire_monitored_session() ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    Self ! backend_request,
    {ok,[{user_id,15},{auth_time, 5000}]} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [{type,<<"rtmp">>},{pid,self()}])),
  assertBackendRequested(backend_wasnt_requested),
  [#session{session_id = Id}] = ets:tab2list(flu_session:table()),
  ets:update_element(flu_session:table(), Id, [{#session.last_access_time, 0}]),

  flu_session ! clean,
  gen_server:call(flu_session, sync_call),

  ?assertMatch([_], ets:tab2list(flu_session:table())),

  ok.




test_rerequest_expiring_session() ->
  Self = self(),

  meck:expect(flu, now_ms, fun() -> 10000 end),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    Self ! backend_request,
    {ok,[{user_id,15},{auth_time, 5000}]} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  meck:expect(flu, now_ms, fun() -> 12000 end),
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendNotRequested(backend_was_requested1),


  meck:expect(flu, now_ms, fun() -> 14000 end),
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendNotRequested(backend_was_requested2),


  meck:expect(flu, now_ms, fun() -> 16000 end),
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested_second_time),

  ok.



test_rerequest_expiring_unique_session() ->
  Self = self(),
  meck:expect(flu, now_ms, fun() -> 10000 end),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    Self ! backend_request,
    {ok,[{user_id,15},{auth_time, 5000},{unique,true}]} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  meck:expect(flu, now_ms, fun() -> 30000 end),
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),

  assertBackendRequested(backend_wasnt_requested_second_time),
  ok.






test_session_is_not_destroyed_after_rerequest() ->
  Self = self(),
  meck:expect(flu, now_ms, fun() -> 10000 end),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    Self ! backend_request,
    {ok,[{user_id,15},{auth_time, 5000}]} 
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested),

  #session{} = Session = flu_session:find_session(Identity),
  Session1 = Session#session{bytes_sent = 5254},
  ets:insert(flu_session:table(), Session1),

  meck:expect(flu, now_ms, fun() -> 20000 end),
  ?assertMatch({ok, _}, flu_session:verify(http_mock_url(), Identity, [])),
  assertBackendRequested(backend_wasnt_requested_second_time),

  #session{bytes_sent = BytesSent} = flu_session:find_session(Identity),
  ?assertEqual(5254, BytesSent),
  ok.






test_unique_session_with_new_ip() ->
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    {ok,[{user_id,14},{unique,true}]} 
  end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"1">>},{name,<<"cam0">>}], [])),

  ?assertMatch([#session{ip = <<"127.0.0.1">>}], ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"94.95.96.97">>},{token,<<"1">>},{name,<<"cam0">>}], [])),

  ?assertMatch([#session{ip = <<"94.95.96.97">>}], ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),
  ?assertMatch([#session{ip = <<"127.0.0.1">>}], ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= denied} = E) -> E end))),
  ok.



test_unique_session_with_persistent_connection() ->
  Connection = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    {ok,[{user_id,14},{unique,true}]} 
  end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"1">>},{name,<<"cam0">>}], [{pid,Connection}])),

  ?assertMatch([#session{ip = <<"127.0.0.1">>, pid = Connection}], 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"94.95.96.97">>},{token,<<"2">>},{name,<<"cam0">>}], [])),

  ?assertEqual(false, erlang:is_process_alive(Connection)),
  ?assertMatch([#session{ip = <<"127.0.0.1">>, pid = Connection}], 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= denied} = E) -> E end))),

  ok.



test_unique_session_with_persistent_connection_same_ip() ->
  Pid1 = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    {ok,[{user_id,14},{unique,true}]} 
  end),
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"1">>},{name,<<"cam0">>}], [{pid,Pid1}])),

  ?assertMatch([#session{ip = <<"127.0.0.1">>, pid = Pid1}], 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),


  Pid2 = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"1">>},{name,<<"cam0">>}], [{pid,Pid2}])),

  ?assertEqual(false, erlang:is_process_alive(Pid1)),
  ?assertMatch([#session{ip = <<"127.0.0.1">>, pid = Pid2}], 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),

  ok.






% Perhaps we should recheck authorization once in some time for persistent connections

test_periodic_refresh_of_auth() ->
  meck:expect(flu, now_ms, fun() -> 10000 end),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    {ok,[{user_id,14},{unique,true},{auth_time,4000}]} 
  end),

  Self = self(),
  Pid1 = spawn(fun() ->
    receive
      refresh_auth -> Self ! auth_was_refreshed;
      Msg -> Msg end
  end),

  Identity = [{ip,<<"127.0.0.1">>},{token,<<"1">>},{name,<<"cam0">>}],
  ?assertMatch({ok, _},
    flu_session:verify(http_mock_url(), Identity, [{pid,Pid1}])),
  ?assertMatch([#session{ip = <<"127.0.0.1">>, pid = Pid1}], 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 14, access= granted} = E) -> E end))),

  ?assert(erlang:is_process_alive(Pid1)),

  meck:expect(flu, now_ms, fun() -> 20000 end),
  meck:expect(flu_session, backend_request, fun(_, _, _) ->
    {error,[{code,403}]} 
  end),

  flu_session:recheck_connected(),

  receive
    auth_was_refreshed -> ok
  after
    100 -> error(auth_not_refreshed)
  end,


  % FIXME: здесь Pid1 должен умереть
  ?assertNot(erlang:is_process_alive(Pid1)),
  
  ok.



params_validation_test_() ->
  [
   ?_assertEqual({error, bad_auth_url}, flu_session:verify(undefined, [], []))
  ,?_assertEqual({error, bad_identity}, flu_session:verify("http://no_url/", undefined, []))
  ,?_assertEqual({error, bad_token}, flu_session:verify("http://no_url/", [{token, undefined}], []))
  ,?_assertEqual({error, bad_token}, flu_session:verify("http://no_url/", [{token, 1234}], []))
  ,?_assertEqual({error, bad_ip}, flu_session:verify("http://no_url/", [{token, <<"1234">>}], []))
  ,?_assertEqual({error, bad_ip}, flu_session:verify("http://no_url/", [{token, <<"1234">>},{ip, lala}], []))
  ,?_assertEqual({error, bad_name}, flu_session:verify("http://no_url/", [{token, <<"1234">>},{ip, <<"127.0.0.1">>}], []))
  ,?_assertEqual({error, bad_name}, flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, lala}], []))
  ,?_assertEqual({error, bad_name}, flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, undefined}], []))

  ,?_assertEqual({error, bad_params}, flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, <<"stream">>}], [[{key,value}]]))
  ].







