-module(fake_auth).
-compile(export_all).

init(_,Req,_) ->
  {ok, Req, state}.

handle(Req, _) ->
  {Code, Headers, Body} = fake_auth:reply(Req),
  {ok, R1} = cowboy_req:reply(Code, Headers, Body, Req),
  {ok, R1, undefined}.

terminate(_,_) -> ok.

start_http() ->
  Dispatch = [{'_', [{['...'], fake_auth, []}]}],
  {ok, Pid} = cowboy:start_http(fake_http, 1, [{port, 6070}],
    [{dispatch, Dispatch}]),
  {ok, Pid}.

