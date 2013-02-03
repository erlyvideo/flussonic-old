-module(fake_inet).

-export([peername/1]).

peername(socket) ->
  {ok, {{127,0,0,1},45302}}.
