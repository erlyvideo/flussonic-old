
-module(gen_tracker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_tracker/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_tracker(Name) ->
  case erlang:whereis(rack_sup) of
    undefined -> application:start(rack);
    _ -> ok
  end,
  {ok, Pid1} = case erlang:whereis(Name) of
    undefined ->
      RetVal = supervisor:start_child(gen_tracker_sup, {
        Name,
        {gen_tracker, start_link, [Name]},
        permanent,
        1000,
        worker,
        [gen_tracker]
      }),
      case RetVal of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
      end;
    Pid ->
      {ok, Pid}
  end,
  case ets:info(Name) of
    undefined -> gen_server:call(Pid1, wait);
    _ -> ok
  end,
  {ok, Pid1}.
      

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, []} }.

