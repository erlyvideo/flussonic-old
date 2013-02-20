-module(pulse_collector).

-export([start_link/0]).

-export([init/1, terminate/2]).


start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-record(pulse, {
  
}).

init([]) ->

  {ok, #pulse{}}.


terminate(_,_) -> ok.
