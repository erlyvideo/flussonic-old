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

reply(_Req) ->
  {500, [], "backend error"}.



terminate(_,_,_) -> ok.

start_http() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  application:start(public_key),
  application:start(ssl),
  application:start(lhttpc),
  Dispatch = [{'_', [{"/auth", fake_auth, []}]}],
  {ok, Pid} = cowboy:start_http(fake_auth, 1, [{port, 6070}],
    [{env,[{dispatch, cowboy_router:compile(Dispatch)}]}]),
  {ok, Pid}.


stop_http() ->
  error_logger:delete_report_handler(error_logger_tty_h),  
  application:stop(lhttpc),
  application:stop(ssl),
  application:stop(public_key),
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(crypto),
  error_logger:add_report_handler(error_logger_tty_h),
  ok.
