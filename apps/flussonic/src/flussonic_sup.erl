%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        flussonic sup
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(flussonic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).


%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================



init([flu_http]) ->
  {ok, {{one_for_all, 10, 100}, []}};

init([]) ->
  Supervisors = [ 
  {flu_router_sup, {flu_router, start_link, []}, permanent, 100, worker, []},
  {static_stream_watcher,{static_stream_watcher, start_link, []},permanent,1000,worker,[]},
  {flu_session,{flu_session, start_link, []},permanent,1000,worker,[]},
  {rate_limiter, {rate_limiter, start_link, []}, permanent, 100, worker, [rate_limiter]},
  {flu_event,{flu_event, start_link, []},permanent,1000,worker,[flu_event]},
  {flu_health,{flu_health, start_link, []},permanent,1000,worker,[flu_health]},
  
  {flu_streams, {gen_tracker, start_link, [flu_streams]}, permanent, infinity, supervisor, []},
  {flu_files, {gen_tracker, start_link, [flu_files]}, permanent, infinity, supervisor, []}
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

