%%% @private
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP listener
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_listener).
-author('Max Lapshin <max@maxidoors.ru>').

%% External API
-export([start_link/4]).
-export([init/4]).



start_link(ListenerPid, Socket, _Transport, [Callback, Args]) ->
  % {ok, RTMP} = rtmp_sup:start_rtmp_socket(accept),
  RTMP = proc_lib:spawn_link(?MODULE, init, [ListenerPid, Socket, Callback, Args]),
  {ok, RTMP}.


init(ListenerPid, Socket, Callback, Args) ->
  ranch:accept_ack(ListenerPid),
  {ok, Pid} = erlang:apply(Callback, create_client, [self()|Args]),
  {ok, StateName, StateData1, Timeout} = rtmp_socket:init([accept]),
  StateData2 = rtmp_socket:set_options(StateData1, [{consumer, Pid}]),

  rtmp_socket:set_socket(self(), Socket),
  gen_fsm:enter_loop(rtmp_socket, [], StateName, StateData2, Timeout).
