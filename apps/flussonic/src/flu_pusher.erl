-module(flu_pusher).

-export([start_link/2]).

-export([init/1, handle_info/2, terminate/2]).

-export([http_post/1]).
-include("log.hrl").

start_link(Name, URL) ->
  proc_lib:start_link(?MODULE, init, [[Name, URL]]).


-record(pusher, {
  name,
  error_count = 0,
  url,
  proto,
  socket,
  timer
}).

init([Name, URL_]) ->
  URL = iolist_to_binary(URL_),
  put(name,{pusher,Name,URL}),
  case parse(URL) of
    {ok, {Proto, _Auth, _Host, _Port, _Path, _Query}} ->
      proc_lib:init_ack({ok, self()}),
      Timer = erlang:send_after(0, self(), connect),
      ?D({going_to_connect,Proto,URL}),
      gen_server:enter_loop(?MODULE, [], #pusher{name = Name, url = URL, proto = Proto, timer = Timer});
    {error, _} = Error ->
      proc_lib:init_ack(Error)
  end.

parse(URL) ->
  ParseOpts = [{scheme_defaults, [{tshttp,80},{rtmp,1935}]}],
  http_uri:parse(binary_to_list(URL), ParseOpts).

-define(TIMEOUT, 10000).

handle_info(connect, #pusher{name = Name, url = URL, proto = tshttp, timer = OldTimer, error_count = Count} = Pusher) ->
  erlang:cancel_timer(OldTimer),
  case http_post(URL) of
    {ok, Socket} ->
      inet:setopts(Socket, [{active,once}]),
      flu_stream:subscribe(Name, [{proto,chunked_mpegts},{socket,Socket}]),
      {noreply, Pusher#pusher{socket = Socket,error_count =0}};
    {error, Error} ->
      if Count < 3 orelse true -> lager:info("Failed to push ~s to ~s: ~p", [Name,URL,Error]);
      true -> ok end,
      Delay = 1000 + (Count div 10)*500,
      Timer = erlang:send_after(Delay, self(), connect),
      {noreply, Pusher#pusher{socket = undefined,error_count = Count+1,timer=Timer}}
  end;


handle_info(_, State) ->
  {noreply, State}.

http_post(URL) ->
  {ok, {_,_, Host, Port, Path1, Query}} = parse(URL),
  Path = case re:run(Path1, "/mpegts$") of
    nomatch -> Path1 ++ "/mpegts";
    _ -> Path1
  end,
  case gen_tcp:connect(Host, Port, [binary,{active,false},{send_timeout,?TIMEOUT},{packet,http}],?TIMEOUT) of
    {ok, Sock} ->
      ok = gen_tcp:send(Sock, [
        "POST ", Path, Query, " HTTP/1.0\r\n"
        "Host: ", Host, "\r\n"
        % "Content-Type: video/M2TP\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"]),
      recv_http_response(Sock);
    Else ->
      Else
  end.

recv_http_response(Sock) ->
  case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
    {ok, {http_response, _HttpVersion, 200, _HttpString}} ->
      recv_http_headers(Sock);
    {ok, {http_response, _HttpVersion, Code, _HttpString}} ->
      inet:setopts(Sock,[{packet,raw}]),
      {ok,Bin} = gen_tcp:recv(Sock, 0),
      ?D(Bin),
      gen_tcp:close(Sock),
      {error, Code};
    {error, Error} ->
      gen_tcp:close(Sock),
      {error, Error}
  end.

recv_http_headers(Sock) ->
  case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
    {ok, http_eoh} -> {ok, Sock};
    {ok, {http_header, _, _Key, _, _Value}} -> recv_http_headers(Sock);
    {error, _} = Error -> gen_tcp:close(Sock), Error
  end.





terminate(_,_) -> ok.
