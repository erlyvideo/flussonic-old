-module(fake_event_handler).

-export([init/1, handle_event/2, handle_call/2, terminate/2]).


init(Args) ->
  {ok, Args}.

handle_event(_Event, Args) ->
  {ok, Args}.


handle_call({update_options, Args}, _) ->
  {ok, ok, Args};

handle_call(options, Args) ->
  {ok, Args, Args};

handle_call(_Call, Args) ->
  {ok, unknown, Args}.


terminate(_,_) ->
  ok.
