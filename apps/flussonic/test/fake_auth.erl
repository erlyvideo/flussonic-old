-module(fake_auth).
-compile(export_all).

init(_,Req, Options) ->
  {ok, Req, Options}.

handle(Req, [unique_user_id]) ->
  {ok, R1} = cowboy_req:reply(200, [{<<"X-UserId">>, <<"8">>},{<<"X-Unique">>, <<"true">>}], <<"OK">>, Req),
  {ok, R1, undefined};

handle(Req, _) ->
  {Code, Headers, Body} = fake_auth:reply(Req),
  {ok, R1} = cowboy_req:reply(Code, Headers, Body, Req),
  {ok, R1, undefined}.

terminate(_,_) -> ok.

start_http() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  Dispatch = [{'_', [{[<<"auth">>], fake_auth, []}]}],
  {ok, Pid} = cowboy:start_http(fake_http, 1, [{port, 6070}],
    [{dispatch, Dispatch}]),
  {ok, Pid}.

