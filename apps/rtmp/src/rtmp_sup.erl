%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP encoding/decoding module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
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
%% @private
-module(rtmp_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.1).

-behaviour(supervisor).

-export([init/1,start_link/0, start_rtmp_client/2]).
-export([start_shared_object/2, start_rtmp_session/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_rtmp_client(HostPort, Options) -> 
  supervisor:start_child(rtmp_client_sup, [HostPort, Options]).

-spec start_rtmp_session(RTMPSocket::pid(), Callback::atom()) -> {'error',_} | {'ok',pid()}.
start_rtmp_session(RTMPSocket, Callback) ->
  {ok, Pid} = supervisor:start_child(rtmp_session_sup, [Callback]),
  rtmp_session:set_socket(Pid, RTMPSocket),
  {ok, Pid}.


start_shared_object(Name, Persistent) -> supervisor:start_child(shared_object_sup, [Name, Persistent]).



init([shared_object]) ->
  {ok,
    {{simple_one_for_one, 5, 60},
      [
        {   undefined,                               % Id       = internal id
            {shared_object,start_link,[]},             % StartFun = {M, F, A}
            temporary,                               % Restart  = permanent | transient | temporary
            2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                                  % Type     = worker | supervisor
            []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };

init([rtmp_client]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
    {undefined, {rtmp_socket,start_client,[]},
    temporary, 2000, worker,[]}
  ]}};

init([rtmp_session]) ->
  {ok,
    {{simple_one_for_one, 5, 60},
      [
        {   undefined,                               % Id       = internal id
            {rtmp_session,start_link,[]},             % StartFun = {M, F, A}
            temporary,                               % Restart  = permanent | transient | temporary
            2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                                  % Type     = worker | supervisor
            []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };

init([]) ->
  Supervisors = [
    {rtmp_client_sup,
      {supervisor,start_link,[{local, rtmp_client_sup}, ?MODULE, [rtmp_client]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {rtmp_session_sup,
      {supervisor,start_link,[{local, rtmp_session_sup}, ?MODULE, [rtmp_session]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {shared_object_sup,
      {supervisor,start_link,[{local, shared_object_sup}, ?MODULE, [shared_object]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {shared_objects_sup,
      {shared_objects,start_link,[]},
      permanent,                               % Restart  = permanent | transient | temporary
      1000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                              % Type     = worker | supervisor
      [shared_objects]                                       % Modules  = [Module] | dynamic
    },
    {rtmp_stat_collector_sup,
      {rtmp_stat_collector,start_link,[[{depth,100},{timer,5000}]]},
      permanent,                               % Restart  = permanent | transient | temporary
      1000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                              % Type     = worker | supervisor
      [rtmp_stat_collector]                                       % Modules  = [Module] | dynamic
    }
  ],
  
  
  ets:new(rtmpt_sessions, [public,named_table]),
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
