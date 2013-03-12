-module(auth_http_backend_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


http_mock_url() -> "http://127.0.0.1:5671/auth/check".

backend_test_() ->
  {setup, 
  flu_test:setup_([{meck, [fake_auth, auth_http_backend]}]),
  flu_test:teardown_(),
  flu_test:tests(?MODULE)}.



test_simple() ->
  ?assertEqual({ok, [{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/normal", 
      [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_with_auth_duration() ->
  ?assertEqual({ok, [{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/600", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

% test_backend_request3() ->
%   meck:expect(fake_auth, reply, fun() -> {302,[{<<"X-Name">>, <<"cam1">>}], <<"">>} end),
%   ?assertEqual({ok, <<"cam1">>, [{access,granted}]},
%     auth_http_backend:verify(http_mock_url(), [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_403_response() ->
  ?assertEqual({error, [{auth_time,30000},{code,403},{delete_time,30000}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/deny", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_500_response() ->
  ?assertEqual(undefined,
    auth_http_backend:verify("http://127.0.0.1:5671/auth/500", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).

test_backend_is_down() ->
  ?assertEqual(undefined,
    auth_http_backend:verify("http://127.0.0.1:6071/", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], []) ).


test_with_user_id_and_duration() ->
  ?assertEqual({ok, [{auth_time,600000},{delete_time,600000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/user15_600", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).

test_with_user_id() ->
  ?assertEqual({ok, [{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>},{user_id,15}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/user15", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).



test_backend_unique_uid() ->
  ?assertEqual({ok, [{auth_time,30000},{delete_time,30000},{referer,<<"http://ya.ru/">>},{unique,true},{user_id,15}]},
    auth_http_backend:verify("http://127.0.0.1:5671/auth/user15_unique", [{ip,<<"127.0.0.1">>},{token,<<"123">>},{name,<<"cam0">>}], [{referer,<<"http://ya.ru/">>}]) ).


test_backend_arguments() ->
  Self = self(),
  meck:expect(fake_auth, handle, fun(Req, _) ->
    {QsVals, _} = cowboy_req:qs_vals(Req),
    % ?debugFmt("qs_vals: ~p", [QsVals]),
    Self ! {backend_request, QsVals},
    {ok, R1} = cowboy_req:reply(200, [{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>, Req),
    {ok, R1, undefined}
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


test_url_prepare() ->
  Self = self(),
  meck:expect(auth_http_backend, http_get, fun(URL) ->
    Self ! {backend, URL},
    {error, rejected}
  end),

  auth_http_backend:verify("http://127.0.0.1:5671/auth/check", [{ip,<<"94.95.96.97">>},{token,<<"123">>},
    {name,<<"bunny.mp4">>}], [{total_users,0},{referer,<<"http://ya.ru/?token=456&name=lalala">>}]),

  URL = receive
    {backend, URL_} -> URL_
  after
    10 -> error(backend_wasnt_requested)
  end,

  {ok, {http, "", Host, _, Path, "?" ++ Qs}} = http_uri:parse(URL),
  ?assertEqual("127.0.0.1", Host),
  ?assertEqual("/auth/check", Path),
  Query = httpd:parse_query(Qs),
  ?assertEqual("94.95.96.97", proplists:get_value("ip", Query)),
  ?assertEqual("123", proplists:get_value("token", Query)),
  ?assertEqual("bunny.mp4", proplists:get_value("name", Query)),
  ?assertEqual("0", proplists:get_value("total_users", Query)),
  ?assertEqual("http://ya.ru/?token=456&name=lalala", proplists:get_value("referer", Query)),
  ok.




test_handle_strange_options_in_backend() ->
  Self = self(),
  meck:expect(auth_http_backend, http_get, fun(URL) ->
    Self ! {backend, URL},
    {error, rejected}
  end),

  auth_http_backend:verify(http_mock_url(), [{ip,<<"94.95.96.97">>},{token,<<"123">>},
    {name,<<"bunny.mp4">>}], 
    [{total_users,0},{referer,<<"http://ya.ru/?token=456&name=lalala">>},
    {pid,self()},{rtmp, [{swfUrl,<<"http://a/">>},{tcUrl, <<"player.swf">>}]}]),

  _URL = receive
    {backend, URL_} -> URL_
  after
    10 -> error(backend_wasnt_requested)
  end,

  ok.
















