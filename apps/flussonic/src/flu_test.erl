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
  Apps = [crypto, ranch, cowboy, public_key, ssl, lhttpc, pulse, http_file, flussonic] ++ proplists:get_value(apps, Opts, []),
  [case application:start(App) of
    ok -> ok;
    {error, {already_started,App}} -> ok
  end || App <- Apps],
  log(proplists:get_value(log,Opts)),
  Mods = proplists:get_value(meck, Opts, []),
  case Mods of
    [] -> ok;
    _ -> meck:new(Mods, [{passthrough,true}])
  end,

  set_config([]),

  R = Fun(),
  {Apps, Mods, Opts, R}.


log() ->
  log(true).

log(undefined) -> ok;
log(true) -> log(info);
log(Level) ->
  application:load(lager),
  application:set_env(lager,error_logger_redirect,false),
  application:set_env(lager,handlers,[{lager_console_backend,Level}]),
  lager:start().



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
  wait_for_not_connect(5670, 5),
  {ok, _} = flu:start_webserver([{http,5670}|Compiled]),
  wait_for_connect(5670, 5),

  AuthDispatch = [{'_', [
    {"/auth/:behaviour", fake_auth, []},
    {"/video/[...]", cowboy_static, [{directory, "../../../priv"},{mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
  ]}],
  cowboy:stop_listener(fake_auth),
  wait_for_not_connect(5671, 5),
  {ok, _} = cowboy:start_http(fake_auth, 1, [{port, 5671}],
    [{env,[{dispatch, cowboy_router:compile(AuthDispatch)}]}]),
  wait_for_connect(5671, 5),

  case whereis(rtmp_sup) of
    undefined -> ok;
    _ ->
      supervisor:terminate_child(rtmp_sup, rtmp_listener1),
      supervisor:delete_child(rtmp_sup, rtmp_listener1)
  end,
  case proplists:get_value(rtmp, Compiled) of
    undefined -> ok;
    RTMP ->
      rtmp_socket:start_server(RTMP, rtmp_listener1, flu_rtmp),
      wait_for_connect(RTMP, 5)
  end,


  case proplists:get_value(rtsp, Compiled) of
    undefined -> rtsp:stop_server(rtsp_listener1);
    RTSPPort -> rtsp:start_server(RTSPPort, rtsp_listener1, flu_rtsp)
  end,

  ok.

wait_for_not_connect(Port, 0) -> error({still_listening,Port});
wait_for_not_connect(Port, Count) ->
  case gen_tcp:connect({127,0,0,1}, Port, [binary], 300) of
    {error, econnrefused} -> ok;
    {ok, S} -> gen_tcp:close(S), timer:sleep(10), wait_for_not_connect(Port, Count - 1);
    _ -> timer:sleep(10), wait_for_not_connect(Port, Count - 1)
  end.


wait_for_connect(Port, 0) ->
  error({not_listening,Port});

wait_for_connect(Port, Count) ->
  case gen_tcp:connect({127,0,0,1}, Port, [binary]) of
    {ok, S} -> gen_tcp:close(S);
    {error, _} -> timer:sleep(10), wait_for_connect(Port, Count - 1)
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






bunny() ->
  case file:open("../../priv/bunny.mp4", [read,binary,raw]) of
    {error, _} -> file:open("../../../priv/bunny.mp4",[read,binary,raw]);
    {ok, F_} -> {ok, F_}
  end.

media_info() ->
  {ok, F} = bunny(),
  {ok, R} = mp4_reader:init({file,F},[]),
  MediaInfo = mp4_reader:media_info(R),
  file:close(F),
  MediaInfo.  


gop() ->
  gop(undefined).

gop(N) ->
  {ok, F} = bunny(),
  {ok, R} = mp4_reader:init({file,F},[]),
  {ok, Gop} = mp4_reader:read_gop(R, N),
  file:close(F),
  Gop.



% Here goes _very_ simple http client with lhttpc api
request(URL, Method, Headers, Timeout) ->
  try request0(URL, Method, Headers, Timeout)
  catch
    throw:Reply -> Reply
  end.


request0(URL, Method, Headers, Timeout) ->
  {ok, {http, Auth, Host, Port, Path, Query}} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,false}, {packet, http}, 
    {send_timeout, Timeout}], Timeout),

  AuthHeader = case Auth of
    "" -> "";
    _ -> ["Authorization: Basic ",base64:encode_to_string(Auth),"\r\n"]
  end,
  Meth = string:to_upper(lists:flatten(io_lib:format("~s", [Method]))),
  Request = [Meth, " ", Path, Query, " HTTP/1.1\r\n",
  "Host: ", Host, "\r\n",
  "Connection: keepalive\r\n",
  AuthHeader,
  [[K,": ",V,"\r\n"] || {K,V} <- Headers],
  "\r\n"],
  ok = gen_tcp:send(Socket, Request),
  {ok, {http_response, _HttpVersion, Code, Phrase}} = gen_tcp:recv(Socket, 0, Timeout),

  ReplyHeaders = collect_headers(Socket, Timeout),
  case proplists:get_value("content-length", ReplyHeaders) of
    undefined ->
      {ok, {{Code, Phrase}, ReplyHeaders, fun(R) -> body_fun(R,Socket,ReplyHeaders,Timeout) end}};
    Length ->
      inet:setopts(Socket, [{packet,raw}]),
      {ok, Body} = gen_tcp:recv(Socket, list_to_integer(Length), Timeout),
      gen_tcp:close(Socket),
      {ok, {{Code, Phrase}, ReplyHeaders, Body}}
  end.

body_fun(close, Socket, _Headers, _Timeout) ->
  gen_tcp:close(Socket);
body_fun(next, Socket, ReplyHeaders, Timeout) ->
  case proplists:get_value("transfer-encoding",ReplyHeaders) of
    "chunked" ->
      inet:setopts(Socket, [{packet,line}]),
      {ok, Line} = gen_tcp:recv(Socket, 0, Timeout),
      ChunkSize = list_to_integer(string:sublist(Line,1,length(Line) - 2), 16),
      inet:setopts(Socket, [{packet,raw}]),
      {ok, Chunk} = gen_tcp:recv(Socket, ChunkSize, Timeout),
      {ok, Chunk};
    _ ->
      gen_tcp:recv(Socket, 0, Timeout) 
  end;
body_fun(Size, Socket, _, Timeout) when is_number(Size) ->
  inet:setopts(Socket, [{packet,raw}]),
  gen_tcp:recv(Socket, Size, Timeout).




collect_headers(Socket, Timeout) ->
  case gen_tcp:recv(Socket, 0, Timeout) of
    {ok, http_eoh} -> [];
    {ok, {http_header, _, H, _, Value}} ->
      Header = string:to_lower(if is_atom(H) -> atom_to_list(H); true -> H end),
      [{Header, Value}|collect_headers(Socket,Timeout)]
  end.

