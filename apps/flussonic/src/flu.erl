%% Misc features
-module(flu).
-include("log.hrl").

-export([rebuild/0, reload/1, reconf/0]).

-export([now/0, now_ms/0]).

-export([join/2]).
-export([extract_config_if_required/0]).
-export([to_hex/1]).
-export([version/0]).
-export([status/0]).
-export([json_info/0]).
-export([start_webserver/1]).

version() ->
  application:load(flussonic),
  {ok, Version} = application:get_key(flussonic,vsn),
  Version.


status() ->
  S = [io_lib:format("Flussonic ~s is running with streams:\n", [flu:version()]),
  [io_lib:format("~s(~s) with delay:~p~n", [Name,proplists:get_value(url,Info),proplists:get_value(ts_delay,Info)]) ||
    {Name,Info} <- flu_stream:list()]
  ],
  lists:flatten(S).


json_info() ->
  Vsn = list_to_binary(flu:version()),
  License = erlang:module_loaded(hls),
  Info = [{event,'server.info'},{version,Vsn},{license,License}],
  Info.




extract_config_if_required() ->
  case file:read_file_info("priv/flussonic.conf") of
    {error, enoent} -> extract_config();
    _ -> ok
  end.

extract_config() ->
  [begin
    Path1 = re:replace(Path, "sample/", "", [{return,list}]),
    filelib:ensure_dir(Path1),
    {ok, Bin} = static_file:read_internal(Path),
    file:write_file(Path1, Bin)
  end || Path <- static_file:wildcard("priv/sample/[^/]*\\.conf")].



join([], _) -> <<>>;
join(Items, Sep) ->
  [[Sep,Item]|It1] = [[Sep,Item] || Item <- Items],
  iolist_to_binary([Item|It1]).
  
rebuild() ->
  io:format("Recompiling flussonic ...~n"),
	make:all([load]),
  % flussonic_app:unload_config(),
	% flussonic_app:load_config(),
  ok.

reconf() ->
  lager:info("Reloading config"),
  try flussonic_app:load_config() of
    _ ->
      case lists:keyfind({ranch_listener_sup,flu_http}, 1, supervisor:which_children(ranch_sup)) of
        {_, ListenerSup, _, _} ->
          case lists:keyfind(ranch_acceptors_sup, 1, supervisor:which_children(ListenerSup)) of
            {_, AcceptorSup, _, _} ->
              [erlang:exit(Pid,normal) || {_,Pid,_,_} <- supervisor:which_children(AcceptorSup)],
              ok;
            false ->
              ok
          end;
        false -> ok
      end
  catch
    throw:invalid_config -> {error, invalid_config}
  end.

reload(App) ->
	application:load(App),
	case application:get_key(App,modules) of
		undefined ->
			ok;
		{ok,Modules} ->
			io:format("Reloading ~p Modules: ~p~n", [App, Modules]),
			[reload_mod(Module) || Module <- Modules]
	end.

reload_mod(Module) when is_atom(Module) ->
	code:soft_purge(Module),
	code:purge(Module),
	code:load_file(Module),
	Module.



now() ->
  {Mega,Sec,_} = erlang:now(),
  Mega*1000000 + Sec.

now_ms() ->
  {Mega, Sec, Micro} = erlang:now(),
  (Mega*1000000 + Sec)*1000 + (Micro div 1000).

to_hex(L) when is_binary(L) ->
  to_hex(binary_to_list(L));

to_hex(L) when is_list(L) -> 
  list_to_binary(lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).





start_webserver(Config) ->
  Dispatch = [{'_', proplists:get_value(prepend_routes, Config, []) ++ flu_config:parse_routes(Config)}],
  {http, HTTPPort} = lists:keyfind(http, 1, Config),


  RateLimit = proplists:get_value(rate_limit, Config),
  Middlewares = case RateLimit of
    undefined -> [];
    _ -> [rate_limiter]
  end ++ [flu_router, api_handler, flu_www, cowboy_router, cowboy_handler],
  ProtoOpts = [{env,[
      {api, api_handler:compile(Config)},
      {rate_limit,RateLimit},
      {dispatch, cowboy_router:compile(Dispatch)},
      {router,flu_router:compile(Config)}
    ]},
    {max_keepalive,4096},
    {middlewares, Middlewares}],
  
  start_http(flu_http, 100, 
    [{port,HTTPPort},{backlog,4096},{max_connections,32768}],
    ProtoOpts
  ).

start_http(Ref, NbAcceptors, TransOpts, ProtoOpts) when is_integer(NbAcceptors) ->
  {port, Port} = lists:keyfind(port, 1, TransOpts),
  case (catch ranch:get_port(Ref)) of
    Port -> ranch:set_protocol_options(Ref, ProtoOpts);
    _ -> 
      cowboy:stop_listener(Ref),
      cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts)
  end.



