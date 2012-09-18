-module(gen_tracker).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1, find/2, find_or_open/3, list/1, setattr/3, getattr/3, getattr/4, increment/4,delattr/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([delete_by_name/2, delete_by_pid/2, make_entry/1]).

-export([remove_me/2]).

-record(tracker, {
  zone
}).

-record(entry, {
  name,
  pid
}).

make_entry(Props) ->
  #entry{name = proplists:get_value(name,Props), pid = proplists:get_value(pid,Props)}.

info(Zone, Name) ->
  ets:select(attr_table(Zone), ets:fun2ms(fun({{N, K}, V}) when N == Name -> {K,V} end)).

setattr(Zone, Name, Attributes) ->
  ets:insert(attr_table(Zone), [{{Name, K}, V} || {K,V} <- Attributes]).

getattr(Zone, Name, Key) ->
  case ets:lookup(attr_table(Zone), {Name, Key}) of
    [{_, V}] -> {ok, V};
    [] -> undefined
  end.

getattr(_Zone, _Name, _Key, Timeout) when Timeout < -1000 ->
  undefined;

getattr(Zone, Name, Key, Timeout) ->
  case getattr(Zone, Name, Key) of
    undefined ->
      timer:sleep(1000),
      getattr(Zone, Name, Key, Timeout - 1000);
    Else ->
      Else
  end.  

increment(Zone, Name, Key, Incr) ->
  ets:update_counter(attr_table(Zone), {Name, Key}, Incr).

list(Zone) ->
  [{Name,[{pid,Pid}|info(Zone, Name)]} || #entry{name = Name, pid = Pid} <- ets:tab2list(Zone)].

find(Zone, Name) ->
  case ets:lookup(Zone, Name) of
    [] -> undefined;
    [#entry{pid = Pid}] -> {ok, Pid}
  end.

find_or_open(Zone, Name, SpawnFun) ->
  case ets:lookup(Zone, Name) of
    [] -> gen_server:call(Zone, {find_or_open, Name, SpawnFun}, 10000);
    [#entry{pid = Pid}] -> {ok, Pid}
  end.

start_link(Zone) ->
  gen_server:start_link({local, Zone}, ?MODULE, [Zone], []).


attr_table(Zone) ->
  list_to_atom(atom_to_list(Zone)++"_attrs").

init([Zone]) ->
  process_flag(trap_exit, true),
  ets:new(Zone, [public,named_table,{keypos,#entry.name}, {read_concurrency, true}, protected]),
  ets:new(attr_table(Zone), [public,named_table]),
  {ok, #tracker{zone = Zone}}.


handle_call(wait, _From, Tracker) ->
  {reply, ok, Tracker};

handle_call({find_or_open, Name, SpawnFun}, _From, #tracker{zone = Zone} = Tracker) ->
  case ets:lookup(Zone, Name) of
    [] ->
      try SpawnFun() of
        {ok, Pid} ->
          link(Pid),
          ets:insert(Zone, #entry{name = Name, pid = Pid}),
          {reply, {ok,Pid}, Tracker};
        Error ->
          error_logger:error_msg("Spawn function in gen_tracker ~p~n for name ~240p~n returned error: ~p~n", [Zone, Name, Error]),
          {reply, Error, Tracker}
      catch
        _Class:Error ->
          error_logger:error_msg("Spawn function in gen_tracker ~p~n for name ~240p~n failed with error: ~p~nStacktrace: ~n~p~n", 
            [Zone, Name,Error, erlang:get_stacktrace()]),
          {reply, Error, Tracker}
      end;  
    [#entry{pid = Pid}] ->
      {reply, {ok, Pid}, Tracker}
  end;
  
handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

handle_info({'EXIT', Pid, _Reason}, #tracker{zone = Zone} = Tracker) ->
  delete_by_pid(Zone, Pid),
  {noreply, Tracker};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.

terminate(_,_) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

delattr(Zone,Name,Value) ->
  ets:delete(attr_table(Zone),{Name,Value}).

remove_me(Zone, Id) when is_binary(Id) ->
  unlink(whereis(Zone)),
  delete_by_name(Zone, Id);

remove_me(Zone, Id) when is_pid(Id) ->
  unlink(whereis(Zone)),
  delete_by_pid(Zone, Id).

delete_by_name(Zone, Name) ->
  ets:select_delete(Zone, ets:fun2ms(fun(#entry{name = N}) when N == Name -> true end)),
  ets:select_delete(attr_table(Zone), ets:fun2ms(fun({{N, _}, _}) when N == Name -> true end)),
  ok.

delete_by_pid(Zone, Pid) ->
  case ets:select(Zone, ets:fun2ms(fun(#entry{pid = P, name = Name}) when P == Pid -> Name end)) of
    [Name] ->
      ets:select_delete(Zone, ets:fun2ms(fun(#entry{pid = P}) when P == Pid -> true end)),
      ets:select_delete(attr_table(Zone), ets:fun2ms(fun({{N, _}, _}) when N == Name -> true end) );
    [] ->
      % ?D({unknown_pid_failed,File, ets:tab2list(Zone)})
      ok
  end.
  
  
