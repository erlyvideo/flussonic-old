-module(flu_session_log).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

%% for testing
-export([format/2]).

-include("log.hrl").
-include("flu_event.hrl").

-define(DEFAULT_FORMAT, "%e %n %i %y %t").

-record(state, {format, lager_trace}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Config) ->
  Format = proplists:get_value(format, Config, ?DEFAULT_FORMAT),
  case proplists:get_value(file, Config) of
    undefined -> 
      {error, "No file"};
    File ->
      {ok, Trace} = lager:trace_file(File, [{log, sessions}], info),
      {ok, #state{format=Format, lager_trace=Trace}}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(#flu_event{event='user.connected'=Event, options=Stats}, State) ->
  log(format(State#state.format, [{event,Event}|Stats])),
  {ok, State};

handle_event(#flu_event{event='user.disconnected'=Event, options=Stats}, State) ->
  log(format(State#state.format, [{event,Event}|Stats])),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  lager:stop_trace(State#state.lager_trace),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log(Text) ->
  lager:info([{log, sessions}], Text).

%% @doc format session stat to string
format([$%, $e | Fmt], Stats) -> print(event, Stats, "~s") ++ format(Fmt, Stats);
format([$%, $d | Fmt], Stats) -> print(duration, Stats) ++ format(Fmt, Stats);
format([$%, $s | Fmt], Stats) -> print(session_id, Stats) ++ format(Fmt, Stats);
format([$%, $t | Fmt], Stats) -> print(token, Stats) ++ format(Fmt, Stats);
format([$%, $i | Fmt], Stats) -> print(ip, Stats) ++ format(Fmt, Stats);
format([$%, $n | Fmt], Stats) -> print(name, Stats) ++ format(Fmt, Stats);
format([$%, $u | Fmt], Stats) -> print(user_id, Stats) ++ format(Fmt, Stats);
format([$%, $f | Fmt], Stats) -> print(flag, Stats) ++ format(Fmt, Stats);
format([$%, $y | Fmt], Stats) -> print(type, Stats) ++ format(Fmt, Stats);
format([$%, $c | Fmt], Stats) -> print(created_at, Stats) ++ format(Fmt, Stats);
format([$%, $x | Fmt], Stats) -> print(expire_time, Stats) ++ format(Fmt, Stats);
format([$%, $l | Fmt], Stats) -> print(last_access_time, Stats) ++ format(Fmt, Stats);
format([$%, $b | Fmt], Stats) -> print(bytes_sent, Stats) ++ format(Fmt, Stats);
format([Ch | Fmt], Stats) -> [Ch | format(Fmt, Stats)];
format([], _Stats) -> [].

%% @doc get proplist value as string
print(Prop, Proplist) ->
  print(Prop, Proplist, "~p").

print(Prop, Proplist, Format) ->
  case proplists:get_value(Prop, Proplist) of
    V when is_binary(V) -> binary_to_list(V);
    V when is_list(V) -> V;
    V -> io_lib:format(Format, [V])
  end.
