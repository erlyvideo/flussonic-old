
-module(pulse_sup).

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

init([]) ->
  Supervisors = [
    {pulse_collector, {pulse_collector, start_link, []}, permanent, 2000, worker, [pulse_collector]},
    
    {   traffic_collector_sup,
        {traffic_collector,start_link,[]},
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [traffic_collector]                      % Modules  = [Module] | dynamic
    }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

