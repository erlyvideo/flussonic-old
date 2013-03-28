-module(flu_session_SUITE).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("common_test/include/ct.hrl").
-include("../src/flu_session.hrl").
-compile(export_all).









all() ->
  [
    {group, sessions},
    {group, mocked_sessions}
  ].


groups() ->
  [
    {sessions, [parallel], [
      new_session,
      remember_positive_with_changing_reply,
      cached_positive,
      remember_negative,
      monitor_session,
      no_session_created_when_backend_down,
      stale_granted_when_backend_down,
      session_info,
      params_validation,
      expire_and_delete_session,
      dont_expire_monitored_session,
      unique_session_with_new_ip,
      unique_session_with_persistent_connection,
      unique_session_with_persistent_connection_same_ip
    ]},
    {mocked_sessions, [], [
      rerequest_expiring_session,
      rerequest_expiring_unique_session,
      session_is_not_destroyed_after_rerequest,
      periodic_refresh_of_auth,
      backend_arguments_on_stream,
      backend_arguments_on_file,
      backend_arguments_on_live
    ]}
  ].

init_per_group(sessions, Config) ->
  R = flu_test:setup(),
  ServerConf = [
    {file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6070/vodauth"}]},
    {rewrite, "cam2", "passive://localhost/", [{sessions, "http://127.0.0.1:6070/streamauth"}]},
    {live, "live", [{sessions, "http://127.0.0.1:6070/liveauth"}]}
  ],
  flu_test:set_config(ServerConf),
  [{r,R}|Config];


init_per_group(mocked_sessions, Config) ->
  R = flu_test:setup(),
  ServerConf = [
    {file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6070/vodauth"}]},
    {rewrite, "cam2", "passive://localhost/", [{sessions, "http://127.0.0.1:6070/streamauth"}]},
    {live, "live", [{sessions, "http://127.0.0.1:6070/liveauth"}]}
  ],
  flu_test:set_config(ServerConf),
  [{r,R},{meck,[flu,flu_session]}|Config].

end_per_group(_, Config) ->
  {value,{r, R},Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1.




init_per_testcase(_, Config) ->
  case proplists:get_value(meck,Config,[]) of
    [] -> ok;
    Meck -> meck:new(Meck,[{passthrough,true}])
  end,
  Config.

end_per_testcase(_,Config) ->
  case proplists:get_value(meck,Config,[]) of
    [] -> ok;
    Meck -> meck:unload(Meck)
  end,
  Config.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Stateless tests
%%



new_session(_Config) ->
  #session{access = granted} = flu_session:new_or_update([{ip,<<"127.0.0.1">>},{token,<<"tok10">>},{name,<<"cam0">>}], [{access,granted}]).


remember_positive_with_changing_reply(_Config) ->
  put(mock_backend, fun(_,_) -> {ok, []} end),
  {ok, _} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok20">>},{name,<<"cam0">>}], []),

  put(mock_backend, fun(_,_) -> {error, [{code,403}]} end),
  {ok, _} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok20">>},{name,<<"cam0">>}], []).



cached_positive(_Config) ->
  put(request_count,0),
  put(mock_backend, fun(_,_) ->
    put(request_count, get(request_count) + 1),
    {ok, [{user_id,6},{auth_time, 10000}]}
  end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok30">>},{name,<<"cam0">>}],
  {ok, SessionId} = flu_session:verify(mocked, Identity, []),
  1 = get(request_count),

  {ok, SessionId} = flu_session:verify(mocked, Identity, []),
  1 == get(request_count) orelse error(backend_was_requested2),

  ok.



remember_negative(_Config) ->
  put(mock_backend, fun(_,_) -> {error, [{code,403}]} end),
  {error, 403, _} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok40">>},{name,<<"cam0">>}], []),

  put(mock_backend, fun(_,_) -> {ok, []} end),
  {error, 403, _} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok40">>},{name,<<"cam0">>}], []),
  ok.


monitor_session(_Config) ->
  put(mock_backend, fun(_,_) -> {ok, []} end),
  Identity = [{ip,<<"127.0.0.5">>},{token,<<"tok50">>},{name,<<"cam0">>}],

  {ok, SessionId} = flu_session:verify(mocked, Identity, [{type,<<"rtmp">>},{pid,self()}]),
  [Info] = [S || S <- flu_session:list(), proplists:get_value(id,S) == SessionId],
  ?assertEqual(<<"127.0.0.5">>, proplists:get_value(ip, Info)),
  Session = flu_session:find_session(Identity),

  flu_session ! {'DOWN', flu_session:ref(Session), undefined, self(), undefined},
  gen_server:call(flu_session, sync_call),

  undefined = flu_session:find_session(Identity),
  [] = [S || S <- flu_session:list(), proplists:get_value(id,S) == SessionId],

  gen_server:call(flu_session, {unregister, flu_session:ref(Session)}),
  ok.


no_session_created_when_backend_down(_Config) ->
  put(mock_backend, fun(_,_) -> undefined end),
  {error,403,_} = flu_session:verify("http://127.0.0.5/", [{ip,<<"127.0.0.1">>},{token,<<"tok60">>},{name,<<"cam0">>}], []).


stale_granted_when_backend_down(_Config) ->
  put(mock_backend, fun(_,_) -> {ok, []} end),
  Identity = [{ip,<<"127.0.0.5">>},{token,<<"tok70">>},{name,<<"cam0">>}],

  {ok, SessionId} = flu_session:verify(mocked, Identity, [{type,<<"rtmp">>}]),

  put(mock_backend, fun(_,_) -> undefined end),

  {ok, SessionId} = flu_session:verify(mocked, Identity, [{type,<<"rtmp">>}]),
  ok.



session_info(_Config) ->
  put(mock_backend, fun(_,_) -> {ok, [{user_id,7}]} end),
  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok70">>},{name,<<"cam0">>}],
  {ok, _} = flu_session:verify(mocked, Identity, []),
  Info = flu_session:info(Identity),
  <<"cam0">> = proplists:get_value(name, Info),
  7 = proplists:get_value(user_id, Info),
  granted = proplists:get_value(access, Info),
  ok.




params_validation(_Config) ->
  {error, bad_auth_url} = flu_session:verify(undefined, [], []),
  {error, bad_identity} = flu_session:verify("http://no_url/", undefined, []),
  {error, bad_token} = flu_session:verify("http://no_url/", [{token, undefined}], []),
  {error, bad_token} = flu_session:verify("http://no_url/", [{token, 1234}], []),
  {error, bad_ip} = flu_session:verify("http://no_url/", [{token, <<"1234">>}], []),
  {error, bad_ip} = flu_session:verify("http://no_url/", [{token, <<"1234">>},{ip, lala}], []),
  {error, bad_name} = flu_session:verify("http://no_url/", [{token, <<"1234">>},{ip, <<"127.0.0.1">>}], []),
  {error, bad_name} = flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, lala}], []),
  {error, bad_name} = flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, undefined}], []),

  {error, bad_params} = flu_session:verify("http://no_url/", 
      [{token, <<"1234">>},{ip, <<"127.0.0.1">>}, {name, <<"stream">>}], [[{key,value}]]),
  ok.






expire_and_delete_session(_Config) ->
  put(backend_count,0),
  put(mock_backend, fun(_,_) ->
    put(backend_count,get(backend_count)+1),
    {ok, [{user_id,8}]} 
  end),

  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok80">>},{name,<<"cam0">>}],
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),

  ets:update_element(flu_session:table(), Id, [{#session.last_access_time, 0}]),
  flu_session ! clean,
  gen_server:call(flu_session, sync_call),

  {ok, Id} = flu_session:verify(mocked, Identity, []),
  2 = get(backend_count),
  ok.




dont_expire_monitored_session(_Config) ->
  put(backend_count,0),
  put(mock_backend, fun(_,_) ->
    put(backend_count,get(backend_count)+1),
    {ok, [{user_id,9},{auth_time, 5000}]} 
  end),


  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok90">>},{name,<<"cam0">>}],
  {ok, Id} = flu_session:verify(mocked, Identity, [{type,<<"rtmp">>},{pid,self()}]),
  ets:update_element(flu_session:table(), Id, [{#session.last_access_time, 0}]),

  flu_session ! clean,
  gen_server:call(flu_session, sync_call),

  [_] = ets:lookup(flu_session:table(), Id),

  ok.





unique_session_with_new_ip(_Config) ->
  put(mock_backend, fun(_,_) ->
    {ok, [{user_id,10},{unique,true}]} 
  end),

  {ok, Id1} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok91">>},{name,<<"cam0">>}], []),

  [#session{session_id = Id1, ip = <<"127.0.0.1">>}] = 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 10, access= granted} = E) -> E end)),

  {ok, Id2} = flu_session:verify(mocked, [{ip,<<"94.95.96.97">>},{token,<<"tok91">>},{name,<<"cam0">>}], []),

  [#session{session_id = Id1, ip = <<"127.0.0.1">>}] = 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 10, access= denied} = E) -> E end)),
  [#session{session_id = Id2, ip = <<"94.95.96.97">>}] = 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 10, access= granted} = E) -> E end)),
  ok.




unique_session_with_persistent_connection(_Config) ->
  Connection = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  put(mock_backend, fun(_,_) ->
    {ok, [{user_id,11},{unique,true}]} 
  end),

  {ok, Id1} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok92">>},{name,<<"cam0">>}], [{pid,Connection}]),

  [#session{ip = <<"127.0.0.1">>, pid = Connection, session_id = Id1}] = 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 11, access= granted} = E) -> E end)),

  {ok, Id2} = flu_session:verify(mocked, [{ip,<<"94.95.96.97">>},{token,<<"tok93">>},{name,<<"cam0">>}], []),

  false = erlang:is_process_alive(Connection),
  [#session{ip = <<"127.0.0.1">>, pid = Connection, session_id = Id1}] =
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 11, access= denied} = E) -> E end)),
  [#session{ip = <<"94.95.96.97">>, session_id = Id2}] =
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 11, access= granted} = E) -> E end)),

  ok.



unique_session_with_persistent_connection_same_ip(_Config) ->
  Pid1 = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  put(mock_backend, fun(_,_) ->
    {ok, [{user_id,4},{unique,true}]} 
  end),
  
  {ok, Id1} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok93">>},{name,<<"cam0">>}], [{pid,Pid1}]),

  [#session{ip = <<"127.0.0.1">>, pid = Pid1, session_id = Id1}] =
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 4, access= granted} = E) -> E end)),


  Pid2 = spawn(fun() ->
    receive
      Msg -> Msg
    end
  end),

  {ok, Id2} = flu_session:verify(mocked, [{ip,<<"127.0.0.1">>},{token,<<"tok93">>},{name,<<"cam0">>}], [{pid,Pid2}]),

  false = erlang:is_process_alive(Pid1),
  [#session{ip = <<"127.0.0.1">>, pid = Pid2, session_id = Id2}] = 
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 4, access= granted} = E) -> E end)),

  ok.












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Statefull tests
%%












rerequest_expiring_session(_Config) ->

  meck:expect(flu, now_ms, fun() -> 10000 end),

  put(backend_count,0),
  put(mock_backend, fun(_,_) ->
    put(backend_count,get(backend_count)+1),
    {ok, [{user_id,1},{auth_time, 5000}]} 
  end),

  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok100">>},{name,<<"cam0">>}],
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),

  meck:expect(flu, now_ms, fun() -> 12000 end),
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),


  meck:expect(flu, now_ms, fun() -> 14000 end),
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),


  meck:expect(flu, now_ms, fun() -> 16000 end),
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  2 = get(backend_count),

  ok.






rerequest_expiring_unique_session(_Config) ->
  meck:expect(flu, now_ms, fun() -> 10000 end),

  put(backend_count,0),
  put(mock_backend, fun(_,_) ->
    put(backend_count,get(backend_count)+1),
    {ok, [{user_id,2},{auth_time, 5000},{unique,true}]} 
  end),

  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok110">>},{name,<<"cam0">>}],
  {ok, Id} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),

  meck:expect(flu, now_ms, fun() -> 30000 end),
  {ok, Id} = flu_session:verify(mocked, Identity, []),

  2 = get(backend_count),
  ok.



session_is_not_destroyed_after_rerequest(_Config) ->
  meck:expect(flu, now_ms, fun() -> 10000 end),

  put(backend_count,0),
  put(mock_backend, fun(_,_) ->
    put(backend_count,get(backend_count)+1),
    {ok, [{user_id,3},{auth_time, 5000}]} 
  end),


  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok120">>},{name,<<"cam0">>}],
  {ok, _} = flu_session:verify(mocked, Identity, []),
  1 = get(backend_count),

  #session{} = Session = flu_session:find_session(Identity),
  ets:insert(flu_session:table(), Session#session{bytes_sent = 5254}),

  meck:expect(flu, now_ms, fun() -> 20000 end),
  {ok, _} = flu_session:verify(mocked, Identity, []),
  2 = get(backend_count),

  #session{bytes_sent = BytesSent} = flu_session:find_session(Identity),
  5254 = BytesSent,
  ok.



% Perhaps we should recheck authorization once in some time for persistent connections

periodic_refresh_of_auth(_Config) ->
  meck:expect(flu, now_ms, fun() -> 10000 end),

  put(mock_backend, fun(_,_) ->
    {ok, [{user_id,5},{auth_time,4000},{unique,true}]} 
  end),


  Self = self(),
  Pid1 = spawn(fun() ->
    receive
      refresh_auth -> Self ! auth_was_refreshed;
      Msg -> Msg end
  end),

  Identity = [{ip,<<"127.0.0.1">>},{token,<<"tok130">>},{name,<<"cam0">>}],
  {ok, Id1} = flu_session:verify(mocked, Identity, [{pid,Pid1}]),
  [#session{ip = <<"127.0.0.1">>, pid = Pid1, session_id = Id1}] =
    ets:select(flu_session:table(), ets:fun2ms(fun(#session{user_id = 5, access= granted} = E) -> E end)),

  true = erlang:is_process_alive(Pid1),

  meck:expect(flu, now_ms, fun() -> 20000 end),

  put(mock_backend, fun(_,_) ->
    {error,[{code,403}]} 
  end),

  flu_session:recheck_connected(),

  receive
    auth_was_refreshed -> ok
  after
    100 -> error(auth_not_refreshed)
  end,


  false = erlang:is_process_alive(Pid1),
  
  ok.





backend_arguments_on_file(_Config) ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),
  {ok, {{403,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/vod/bunny.mp4/manifest.f4m?token=tok140", 
    get, [{"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}], 1000),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  <<"http://127.0.0.1:6070/vodauth">> = URL,
  <<"tok140">> = proplists:get_value(token, Identity),
  <<"vod/bunny.mp4">> = proplists:get_value(name, Identity),
  <<"94.95.96.97">> = proplists:get_value(ip, Identity),
  <<"http://ya.ru/">> = proplists:get_value(referer, Options),

  ok.



backend_arguments_on_stream(_Config) ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),

  {ok, {{403,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/cam2/manifest.f4m?token=tok150", 
    get, [{"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}], 1000),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  <<"http://127.0.0.1:6070/streamauth">> = URL,
  <<"tok150">> = proplists:get_value(token, Identity),
  <<"cam2">> = proplists:get_value(name, Identity),
  <<"94.95.96.97">> = proplists:get_value(ip, Identity),
  <<"http://ya.ru/">> = proplists:get_value(referer, Options),

  ok.



backend_arguments_on_live(_Config) ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {error, [{code,403}]}
  end),
  {ok, {{403,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/live/ustream/manifest.f4m?token=tok160", 
    get, [{"Referer", "http://ya.ru/"}, {"X-Forwarded-For", "94.95.96.97"}], 1000),

  {URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,

  <<"http://127.0.0.1:6070/liveauth">> = URL,
  <<"tok160">> = proplists:get_value(token, Identity),
  <<"live/ustream">> = proplists:get_value(name, Identity),
  <<"94.95.96.97">> = proplists:get_value(ip, Identity),
  <<"http://ya.ru/">> = proplists:get_value(referer, Options),


  ok.




backend_is_working_without_options(_Config) ->
  Self = self(),
  meck:expect(flu_session, backend_request, fun(URL, Identity, Options) ->
    Self ! {backend_request, {URL, Identity, Options}},
    {ok, []}
  end),

  {ok, {{203,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/vod/bunny.mp4/manifest.f4m?token=tok170", 
    get, [{"X-Forwarded-For", "94.95.96.97"}], 1000),

  {_URL, Identity, Options} = receive
    {backend_request, QsVals} -> QsVals
  after
    10 -> error(backend_wasnt_requested)
  end,
  <<"tok170">> = proplists:get_value(token, Identity),
  <<"vod/bunny.mp4">> = proplists:get_value(name, Identity),
  <<"94.95.96.97">> = proplists:get_value(ip, Identity),
  false = lists:keyfind(referer, 1, Options),
  ok.




