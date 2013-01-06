%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP listener module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%% 
%%% erlang-rtsp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtsp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtsp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp_listener).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").

%% External API
-export([start_link/4, init/4]).


start_link(ListenerPid, Socket, _Transport, [Callback, Args]) ->
  {ok, RTSP} = proc_lib:start_link(?MODULE, init, [ListenerPid, Socket, Callback, Args]),
  {ok, RTSP}.

init(ListenerPid, Socket, Callback, Args) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),
  {ok, State, Timeout} = rtsp_socket:init([Socket, Callback, Args]),
  gen_server:enter_loop(rtsp_socket, [], State, Timeout).


