-module(flu_stream_data).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").

-export([start_link/0]).
-export([register/2, get/2, set/3, erase/2]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Name, Pid) when is_binary(Name) andalso is_pid(Pid) ->
  gen_server:call(?MODULE, {register, Name, Pid}).

get(Name, Key) when is_binary(Name) ->
  case ets:lookup(flu_stream_data, {Name,Key}) of
    [{{Name,Key}, Value}] -> {ok, Value};
    [] -> undefined
  end.
  
set(Name, Key, Value) when is_binary(Name) ->
  gen_server:call(?MODULE, {set, Name, Key, Value}).

erase(Name, Key) when is_binary(Name) ->
  ets:delete(flu_stream_data, {Name,Key}).


-record(data, {
  streams = []
}).

init([]) ->
  ets:new(flu_stream_data, [public,named_table]),
  Streams = [{Name,proplists:get_value(pid,Info)} || {Name,Info} <- flu_stream:list()],
  [erlang:monitor(process, Pid) || {_Name, Pid} <- Streams],
  {ok, #data{streams = Streams}}.

handle_info({'DOWN', _, _, Pid, _}, #data{streams = Streams} = Data) ->
  case lists:keytake(Pid, 2, Streams) of
    {value, {Name, Pid}, Streams1} ->
      ets:match_delete(flu_stream_data, {{Name,'$1'},'_'}),
      {noreply, Data#data{streams = Streams1}};
    false ->
      {noreply, Data}
  end;

handle_info(_Msg, Data) ->
  {noreply, Data}.

handle_call({register, Name, Pid}, _From, #data{streams = Streams} = Data) ->
  erlang:monitor(process, Pid),
  Streams1 = lists:keystore(Name, 1, Streams, {Name,Pid}),
  {reply, ok, Data#data{streams = Streams1}};

handle_call({set, Name, Key, Value}, {Pid, _Ref}, #data{streams = Streams} = Data) ->
  Data1 = case lists:keyfind(Name, 1, Streams) of
    false ->
      erlang:monitor(process, Pid),
      Data#data{streams = [{Name,Pid}|Streams]};
    _ ->
      Data
  end,
  ets:insert(flu_stream_data, {{Name,Key},Value}),
  {reply, ok, Data1};

handle_call(_Call, _From, #data{} = Data) ->
  {reply, {unknown_call, _Call}, Data}.

terminate(_, _) ->
  ok.

