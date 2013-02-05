-module(rate_limiter).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-behaviour(cowboy_middleware).
-export([execute/2]).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).




execute(Req, Env) ->
  {Ip,Req1} = cowboy_req:peer_addr(Req),
  ets:insert_new(rate_limit, {Ip,0}),
  Value = ets:update_counter(rate_limit, Ip, 1),
  Limit = proplists:get_value(rate_limit, Env),
  case lists:keyfind(rate_limit, 1, Env) of
    {rate_limit, Limit} when is_number(Limit) andalso Value > Limit ->
      {error, 429, Req1};
    _ ->
      {ok, Req1, Env}
  end.



start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-define(TIMEOUT, 10000).

init([]) ->
  ets:new(rate_limit, [public,named_table,{write_concurrency,true}]),
  {ok, limiter, ?TIMEOUT}.


handle_info(timeout, State) ->
  ets:delete_all_objects(rate_limit),
  {noreply, State, ?TIMEOUT}.


terminate(_,_) ->
  ok.

