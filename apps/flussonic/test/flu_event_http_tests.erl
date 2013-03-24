-module(flu_event_http_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("../src/flu_event.hrl").


init(_,Req, Options) ->
  {ok, Req, Options}.

handle(Req, [Pid]) ->
  {ok, Body, R1} = cowboy_req:body(Req),
  {struct, Evt} = mochijson2:decode(Body),
  Pid ! Evt,
  {ok, R2} = cowboy_req:reply(200, [], "\n", R1),
  {ok, R2, undefined}.

terminate(_,_,_) -> ok.


start_http() ->
  Pid = self(),
  Dispatch = [{'_', [{"/evt", ?MODULE, [Pid]}]}],
  cowboy:start_http(fake_http, 1, [{port, 6070}],
    [{env,[{dispatch, cowboy_router:compile(Dispatch)}]}]),
  ok.


http_test_() ->
  {foreach, fun() ->
    Apps = [crypto,ranch,cowboy,public_key,ssl,lhttpc, flussonic],
    [application:start(App) || App <- Apps],
    Apps
  end, fun(Apps) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    [application:stop(App) || App <- lists:reverse(Apps)],
    error_logger:add_report_handler(error_logger_tty_h)
  end, 
    [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))]  
  }.

test_send_event() ->
  start_http(),
  flu_config:set_config([{flu_event, flu_event_http, [<<"http://127.0.0.1:6070/evt">> ,[]]}]),
  flu_event:start_handlers(),
  flu_event:stream_started(<<"stream1">>, []),
  Event = receive
    Msg_ -> Msg_
    after 100 -> error(http_not_working)
  end,
  ?assertEqual(<<"stream.started">>, proplists:get_value(<<"event">>,Event)),
  ?assertEqual(<<"stream1">>, proplists:get_value(<<"stream">>,Event)),
  ok.
