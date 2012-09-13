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
-export([start_flu_file/2, start_flu_stream/2, stop_stream/1, start_stream_helper/3, stop_stream_helper/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_flu_file(Name, Options) when is_binary(Name) ->
  supervisor:start_child(flu_file_sup, [Name, Options]).

start_flu_stream(Name, Options) when is_binary(Name) ->
  StreamSup = {Name, {supervisor, start_link, [?MODULE, [flu_stream,Name, Options]]}, temporary, infinity, supervisor, []},
  {ok, Sup} = supervisor:start_child(flu_streams_sup, StreamSup),
  {stream, Pid, _, _} = lists:keyfind(stream, 1, supervisor:which_children(Sup)),
  {ok, Pid}.


stop_stream(Stream) when is_binary(Stream) ->
  case lists:keyfind(Stream, 1, supervisor:which_children(flu_streams_sup)) of
    {Stream, _Sup, _, _} ->
      supervisor:terminate_child(flu_streams_sup, Stream),
      supervisor:delete_child(flu_streams_sup, Stream),
      ok;
    false ->
      {error, no_stream}
  end.

start_stream_helper(Stream, Id, {M,F,A}) when is_binary(Stream) ->
  case lists:keyfind(Stream, 1, supervisor:which_children(flu_streams_sup)) of
    {Stream, Sup, _, _} ->
      {helper, Helper, _, _} = lists:keyfind(helper, 1, supervisor:which_children(Sup)),
      ChildSpec = {Id, {M, F, A}, transient, 2000, worker, []},
      case supervisor:start_child(Helper, ChildSpec) of
        {ok, Pid} -> {ok, Pid};
        {error, already_present} ->
          supervisor:terminate_child(Helper, Id),
          supervisor:delete_child(Helper, Id),
          start_stream_helper(Stream, Id, {M,F,A});
        {error, {already_started, Pid}} -> {ok, Pid}
      end;
    false ->
      undefined
  end.

stop_stream_helper(Stream, Id) ->
  case lists:keyfind(Stream, 1, supervisor:which_children(flu_streams_sup)) of
    {Stream, Sup, _, _} ->
      {helper, Helper, _, _} = lists:keyfind(helper, 1, supervisor:which_children(Sup)),
      case lists:keyfind(Id, 1, supervisor:which_children(Helper)) of
        false -> 
          {error, no_child};
        {Id, _, _, _} ->
          supervisor:terminate_child(Helper, Id),
          supervisor:delete_child(Helper, Id),
          ok
      end;
    false ->
      undefined
  end.
  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([flu_file]) ->
  {ok, {{simple_one_for_one, 1000, 1000}, [
    {   undefined,                               % Id       = internal id
    {flu_file,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};

init([flu_streams]) ->
  {ok, {{one_for_one, 1000, 1000}, []}};

init([flu_stream, Name, Options]) ->
  {ok, {{one_for_all, 0, 10}, [
    {stream, {flu_stream, start_link, [Name, Options]}, permanent, 2000, worker, []},
    {helper, {supervisor, start_link, [?MODULE, [flu_stream_helper, Name]]}, permanent, infinity, supervisor, []}
  ]}};

init([flu_stream_helper, _Name]) ->
  {ok, {{one_for_one, 10, 50}, [
  ]}};


init([]) ->
  Supervisors = [ 
  {   flu_file_sup,
      {supervisor,start_link,[{local, flu_file_sup}, ?MODULE, [flu_file]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  },
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
  {   flu_streams_sup,
      {supervisor,start_link,[{local, flu_streams_sup}, ?MODULE, [flu_streams]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

