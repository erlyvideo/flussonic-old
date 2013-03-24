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
-export([stop_stream/1]).

-export([start_stream_helper/3, stop_stream_helper/2, find_stream_helper/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


stop_stream(Stream) when is_binary(Stream) ->
  supervisor:delete_child(flu_streams, Stream).

start_stream_helper(Stream, Id, {M,F,A}) when is_binary(Stream) ->
  case gen_tracker:find(flu_streams, Stream) of
    {ok, Sup} ->
      {helper, Helper, _, _} = lists:keyfind(helper, 1, supervisor:which_children(Sup)),
      ChildSpec = {Id, {M, F, A}, transient, 200, worker, []},
      case supervisor:start_child(Helper, ChildSpec) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, _}} ->
          supervisor:terminate_child(Helper, Id),
          supervisor:delete_child(Helper, Id),
          start_stream_helper(Stream, Id, {M,F,A});
        {error, already_present} ->
          supervisor:delete_child(Helper, Id),
          start_stream_helper(Stream, Id, {M,F,A});          
        {error, Error} -> error(Error)
      end;
    undefined ->
      undefined
  end.

stop_stream_helper(Stream, Id) ->
  case gen_tracker:find(flu_streams, Stream) of
    {ok, Sup} ->
      {helper, Helper, _, _} = lists:keyfind(helper, 1, supervisor:which_children(Sup)),
      case lists:keyfind(Id, 1, supervisor:which_children(Helper)) of
        false -> 
          {error, no_child};
        {Id, _, _, _} ->
          supervisor:terminate_child(Helper, Id),
          supervisor:delete_child(Helper, Id),
          ok
      end;
    undefined ->
      undefined
  end.


find_stream_helper(Stream, Id) ->
  case gen_tracker:find(flu_streams, Stream) of
    {ok, Sup} ->
      {helper, Helper, _, _} = lists:keyfind(helper, 1, supervisor:which_children(Sup)),
      case lists:keyfind(Id, 1, supervisor:which_children(Helper)) of
        false ->
          {error, no_child};
        {Id, Pid, _, _} ->
          {ok, Pid}
      end;
    undefined ->
      {error, no_stream}
  end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([flu_stream, Name, Options]) ->
  {ok, {{one_for_all, 0, 10}, [
    {stream, {flu_stream, start_link, [Name, Options]}, permanent, 2000, worker, []},
    {helper, {supervisor, start_link, [?MODULE, [flu_stream_helper, Name]]}, permanent, infinity, supervisor, []}
  ]}};

init([flu_stream_helper, _Name]) ->
  {ok, {{one_for_one, 10, 50}, [
  ]}};

init([flu_http]) ->
  {ok, {{one_for_all, 10, 100}, []}};

init([]) ->
  Supervisors = [ 
  {flu_router_sup, {flu_router, start_link, []}, permanent, 100, worker, []},
  {   static_stream_watcher_sup,
      {static_stream_watcher, start_link, []},
      permanent,
      1000,
      worker,
      [static_stream_watcher]
  },
  {   flu_session_sup,
      {flu_session, start_link, []},
      permanent,
      1000,
      worker,
      [flu_session]
  },
  {rate_limiter, {rate_limiter, start_link, []}, permanent, 100, worker, [rate_limiter]},
  {   flu_event_sup,
      {flu_event, start_link, []},
      permanent,
      1000,
      worker,
      [flu_event]
  },
  {   flu_health_sup,
      {flu_health, start_link, []},
      permanent,
      1000,
      worker,
      [flu_health]
  },
  {flu_streams, {gen_tracker, start_link, [flu_streams]}, permanent, infinity, supervisor, []},
  {flu_files, {gen_tracker, start_link, [flu_files]}, permanent, infinity, supervisor, []}
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

