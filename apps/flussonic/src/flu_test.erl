-module(flu_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").



setup_() -> fun() -> setup() end.
setup_(A) -> fun() -> setup(A) end.
setup_(A,B) -> fun() -> setup(A,B) end.


setup() ->
  setup([]).

setup(Opts) when is_list(Opts) ->
  setup(Opts, fun() -> ok end);

setup(Fun) when is_function(Fun) ->
  setup([], Fun).

setup(Opts, Fun) ->
  Apps = [crypto, ranch, gen_tracker, cowboy, public_key, ssl, lhttpc, pulse, http_file, flussonic] ++ proplists:get_value(apps, Opts, []),
  [application:start(App) || App <- Apps],
  gen_tracker_sup:start_tracker(flu_files),
  gen_tracker_sup:start_tracker(flu_streams),
  case proplists:get_value(log,Opts) of
    undefined -> ok;
    Level ->
      application:load(lager),
      application:set_env(lager,error_logger_redirect,false),
      Level1 = case Level of
        true -> info;
        _ -> Level
      end,
      application:set_env(lager,handlers,[{lager_console_backend,Level1}]),
      lager:start()
  end,
  Mods = proplists:get_value(meck, Opts, []),
  case Mods of
    [] -> ok;
    _ -> meck:new(Mods, [{passthrough,true}])
  end,

  set_config([]),

  R = Fun(),
  {Apps, Mods, Opts, R}.


teardown({Apps, Mods, Opts, R}, Fun) ->
  error_logger:delete_report_handler(error_logger_tty_h),
  [application:stop(App) || App <- lists:reverse(Apps ++ proplists:get_value(apps,Opts,[]))],
  case proplists:get_value(log,Opts) of
    true -> 
      application:stop(lager),
      code:soft_purge(lager_console_backend);
    _ -> ok
  end,
  case Mods of
    [] -> ok;
    _ -> meck:unload(Mods)
  end,
  case proplists:get_value(arity, erlang:fun_info(Fun)) of
    1 -> Fun(R);
    0 -> Fun()
  end,
  error_logger:add_report_handler(error_logger_tty_h),
  ok.

teardown(Retval) ->
  teardown(Retval, fun() -> ok end).

teardown_() -> fun(R) -> teardown(R) end.
teardown_(Fun) -> fun(R) -> teardown(R,Fun) end.


tests(Mod) ->
  tests(Mod, "test_").

tests(Mod, Prefix) ->
  [{atom_to_list(F), fun Mod:F/0} || {F,0} <- Mod:module_info(exports),
    lists:prefix(Prefix, atom_to_list(F))].



set_config(Config) ->
  {ok, Compiled} = flu_config:parse_config(Config, undefined),
  application:set_env(flussonic, config, Compiled),
  cowboy:stop_listener(flu_http),

  flu:start_webserver([{http,5670}|Compiled]),

  AuthDispatch = [{'_', [
    {"/auth/:behaviour", fake_auth, []},
    {"/video/[...]", cowboy_static, [{directory, "../../../priv"},{mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
  ]}],
  cowboy:stop_listener(fake_auth),
  cowboy:start_http(fake_auth, 1, [{port, 5671}],
    [{env,[{dispatch, cowboy_router:compile(AuthDispatch)}]}]),
  wait_for_connect(5671),
  wait_for_connect(5670),

  case proplists:get_value(rtmp, Compiled) of
    undefined -> ok;
    RTMP ->
      rtmp_socket:start_server(RTMP, rtmp_listener1, flu_rtmp),
      wait_for_connect(RTMP)
  end,

  ok.


wait_for_connect(Port) ->
  case gen_tcp:connect("localhost", Port, [binary]) of
    {ok, S} -> gen_tcp:close(S);
    {error, econnrefused} -> timer:sleep(10), wait_for_connect(Port)
  end.


req(URL) ->
  req(URL, [], get).

req(URL, Headers, Method) ->

  {ok, {_Schema, _Auth, Host, Port, Path, Query1}} = http_uri:parse(binary_to_list(iolist_to_binary(URL))),
  Query = case Query1 of
    [$?|Q] -> Q;
    [] -> []
  end,

  Method1 = iolist_to_binary(string:to_upper(io_lib:format("~s", [Method]))),

  cowboy_req:new(fake_sock, flu_test, Method1, list_to_binary(Path) , list_to_binary(Query) , <<>>,
    {1,2}, Headers, iolist_to_binary(Host), Port, <<>>, false, undefined, undefined).


send(_,_) -> ok.
close(_) -> ok.

peername(_) ->
  {ok, {{127,0,0,1}, 45430}}.






media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  MediaInfo = mp4_reader:media_info(R),
  file:close(F),
  MediaInfo.  


gop() ->
  gop(undefined).

gop(N) ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  {ok, Gop} = mp4_reader:read_gop(R, N),
  file:close(F),
  Gop.

























