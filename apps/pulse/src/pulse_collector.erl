-module(pulse_collector).

-export([start_link/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").

-export([init/1, handle_info/2, handle_call/3, terminate/2]).


start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-record(pulse, {
  clean_timer,
  file_timer
}).

init([]) ->
  ets:new(pulse_traffic_min, [public,named_table,{write_concurrency,true}]),
  ets:new(pulse_traffic_hour, [public,named_table,{write_concurrency,true}]),
  ets:new(pulse_file_min, [public,named_table,{write_concurrency,true}]),
  ets:new(pulse_file_hour, [public,named_table,{write_concurrency,true}]),
  ets:new(pulse_file_timeouts, [public,named_table,{write_concurrency,true}]),
  % ets:new(pulse_hls_reader, [public,named_table]),
  % ets:new(pulse_read_timeouts, [public,named_table]),
  Clean = erlang:send_after(0, self(), clean),
  File = erlang:send_after(17000, self(), file),
  {ok, #pulse{clean_timer = Clean, file_timer = File}}.


handle_call({create_table, Table}, _From, #pulse{} = Pulse) ->
  ets:new(Table, [public,named_table,{write_concurrency,true}]),
  {reply, ok, Pulse}.


handle_info(file, #pulse{file_timer = OldFile} = Pulse) ->
  erlang:cancel_timer(OldFile),

  {Mega,Sec,_} = os:timestamp(),
  Minute = ((Mega*1000000 + Sec) div 60) - 1,

  Stats1 = ets:select(pulse_file_min, ets:fun2ms(fun({S,Read,Seg,Size,Duration}) when S div 60 == Minute ->
    {Read,Seg,Size,Duration}
  end)),

  {Read,Seg,Size,Duration} = lists:foldl(fun({Read1,Seg1,Size1,Duration1}, {Read2,Seg2,Size2,Duration2}) ->
    {Read1+Read2,Seg1+Seg2,Size1+Size2,Duration1+Duration2}
  end, {0,0,0,0}, Stats1),

  ets:insert(pulse_file_hour, {Minute*60, Read,Seg,Size,Duration}),
  ets:select_delete(pulse_file_min, ets:fun2ms(fun({S,_,_,_,_}) when S div 60 < Minute ->
    true
  end)),


  File = erlang:send_after(60000, self(), file),
  {noreply, Pulse#pulse{file_timer = File}};

handle_info(clean, #pulse{clean_timer = OldClean} = Pulse) ->
  erlang:cancel_timer(OldClean),
  Oldest = flu:now() - 3600,

  ets:select_delete(pulse_traffic_min, [{{{'_','$1'},'_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),
  ets:select_delete(pulse_traffic_hour, [{{{'_','$1'},'_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),
  ets:select_delete(pulse_file_min, [{{'$1','_','_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),
  ets:select_delete(pulse_file_hour, [{{'$1','_','_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),
  ets:select_delete(pulse_file_timeouts, [{{'$1','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),
  % ets:select_delete(pulse_hls_reader, MS),

  NewClean = erlang:send_after(60000, self(), clean),
  {noreply, Pulse#pulse{clean_timer = NewClean}}.

terminate(_,_) -> ok.
