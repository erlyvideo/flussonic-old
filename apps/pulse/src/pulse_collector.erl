-module(pulse_collector).

-export([start_link/0]).

-export([init/1, handle_info/2, handle_call/3, terminate/2]).


start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-record(pulse, {
  
}).

init([]) ->
  ets:new(pulse_traffic, [public,named_table]),
  ets:new(pulse_disk, [public,named_table]),
  ets:new(pulse_segments, [public,named_table]),
  ets:new(pulse_hls_reader, [public,named_table]),
  ets:new(pulse_read_timeouts, [public,named_table]),
  self() ! clean,
  {ok, #pulse{}}.


handle_call({create_table, Table}, _From, #pulse{} = Pulse) ->
  ets:new(Table, [public,named_table]),
  {reply, ok, Pulse}.


handle_info(clean, #pulse{} = Pulse) ->
  Oldest = flu:now() - 3600,

  MS = [{{{'_','$1'},'_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}],

  ets:select_delete(pulse_traffic, MS),
  ets:select_delete(pulse_disk, MS),
  ets:select_delete(pulse_segments, MS),
  ets:select_delete(pulse_hls_reader, MS),
  ets:select_delete(pulse_read_timeouts, MS),

  erlang:send_after(60000, self(), clean),
  {noreply, Pulse}.

terminate(_,_) -> ok.
