-module(flu_event_log).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-include("flu_event.hrl").


-record(log, {
  dir,
  path,
  f,
  timer
}).

init([LogDir]) ->
  Log1 = #log{dir = LogDir},
  Log2 = reopen(Log1),
  Timer = erlang:send_after(3600*1000, self(), reopen),
  {ok, Log2#log{timer = Timer}}.


% handle_event(_, #log{f = undefined} = Log) ->
%   {ok, Log};

% handle_event(#flu_event{event = 'stream.started', stream = })

handle_event(_Event, #log{} = Log) ->
  % lager:info("event: ~p", [Event]),
  {ok, Log}.


handle_info(reopen, #log{timer = OldTimer} = Log) ->
  erlang:cancel_timer(OldTimer),
  Timer = erlang:send_after(3600*1000, self(), reopen),
  Log2 = reopen(Log#log{timer = Timer}),
  {ok, Log2};

handle_info(_, #log{} = Log) ->
  {ok, Log}.


terminate(_,_) -> ok.


reopen(#log{f = F} = Log) when F =/= undefined ->
  file:close(F),
  reopen(Log#log{f = undefined});

reopen(#log{} = Log) ->
  Log.
