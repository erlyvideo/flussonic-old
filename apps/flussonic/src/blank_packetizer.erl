-module(blank_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, handle_info/2, terminate/2]).

init(_) ->
  ok.


handle_info(_, State) ->
  {noreply, State}.

terminate(_,_) -> ok.
