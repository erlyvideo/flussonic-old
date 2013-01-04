-module(auth_http_backend_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


http_mock_url() -> "http://127.0.0.1:6070/auth".

backend_test_() ->
  {foreach, fun() ->
    fake_auth:start_http(),
    meck:new(fake_auth, [{passthrough,true}]),
    ok
  end, fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(inets),
    meck:unload(fake_auth),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end,
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))]
  }.



test_backend_request1() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[], <<"">>} end),
  ?assertEqual({ok, [{access,granted},{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request2() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-AuthDuration">>, <<"600">>}], <<"">>} end),
  ?assertEqual({ok, [{access,granted},{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>}]},
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
  ?assertEqual({ok, [{access,granted},{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_backend_request7() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-UserId">>,<<"15">>}], <<"">>} end),
  ?assertEqual({ok, [{access,granted},{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).



test_backend_unique_uid() ->
  meck:expect(fake_auth, reply, fun(_) -> {200,[{<<"X-UserId">>,<<"15">>},{<<"X-Unique">>, <<"true">>}], <<"">>} end),
  ?assertEqual({ok, [{access,granted},{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>},{unique,true},{user_id,15}]},
    auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).


test_backend_arguments() ->
  Self = self(),
  meck:expect(fake_auth, reply, fun(Req) ->
    {QsVals, _} = cowboy_req:qs_vals(Req),
    % ?debugFmt("qs_vals: ~p", [QsVals]),
    Self ! {backend_request, QsVals},
    {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
  end),
  auth_http_backend:verify(http_mock_url(), [{ip,<<"94.95.96.97">>},{token,<<"123">>},{name,<<"bunny.mp4">>}],
  [{referer,<<"http://ya.ru/">>}]),

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


















