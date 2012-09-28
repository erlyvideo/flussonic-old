-module(flu_health).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-export([start_link/0, init/1, handle_event/2, handle_info/2, terminate/2]).

-record(health, {
  type = server,
  ref
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{priority,high}]).

init([]) ->
  self() ! check,
  % gen_event:add_sup_handler(alarm_handler, ?MODULE, [event]),  
  {ok, #health{}};

init([event]) ->
  {ok, #health{type = event}}.


handle_event({set, {process_memory_high_watermark, Pid}}, #health{} = State) ->
  error_logger:info_msg("Warning! Process ~p is consuming too much memory~n", [Pid]),
  spawn(fun() -> 
    erlang:exit(Pid, normal),
    timer:sleep(1000),
    erlang:exit(Pid, kill)
  end),
  {ok, State};

handle_event(_, #health{} = State) ->
  {ok, State}.
  

handle_info(_, #health{type = event} = State) ->
  {ok, State};

handle_info(check, #health{ref = OldRef} = State) ->
  case OldRef of
    undefined -> ok;
    _ -> erlang:cancel_timer(OldRef)
  end,
  Enabled = enabled(),
  case memsup:get_memory_data() of
    {Total, _, _} when Total > 0 andalso Enabled ->
      case ems_debug:top(full_memory) of
        [{Pid,Worst}|_] when Worst > 0 andalso Worst / Total > 0.4 ->
          error_logger:info_msg("Warning! Process ~p is consuming too much memory: ~B out of ~B~n", [Pid, Worst, Total]),
          io:format("~p~n", [ems_debug:dump()]),
          % erlang:exit(Pid, normal),
          % timer:sleep(1000),
          % erlang:exit(Pid, kill),
          ok;
        _ ->
          ok
      end;
    _ ->
      ok
  end,
  Ref = erlang:send_after(20001, self(), check),
  {noreply, State#health{ref = Ref}}.

terminate(_Arg, _State) ->
  ok.


enabled() ->
  not proplists:get_value(disable_health, flu_config:get_config(), false).

  