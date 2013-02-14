-module(log_uploader).

-export([upload/0]).
-include("log.hrl").
-include_lib("kernel/include/file.hrl").

upload() ->
  Files = collect_files(),
  Zip = create_zip(Files),
  {ok, Ticket} = request(Zip),
  {ok, Ticket}.




collect_files() ->
  ifaddrs() ++
  version() ++
  read_file("/etc/flussonic/flussonic.conf") ++
  read_file("priv/flussonic.conf") ++
  read_file("/etc/flussonic/streams.conf") ++
  read_file("priv/streams.conf") ++
  read_file("/etc/flussonic/cookie") ++
  read_file("/etc/flussonic/license.txt") ++
  read_file("/var/log/flussonic/flussonic.log") ++
  read_file("/var/log/flussonic/flussonic.log.0") ++
  read_file("/var/log/flussonic/flussonic.log.1") ++
  read_file("/var/log/flussonic/crash.log") ++
  read_file("/var/log/flussonic/crash.log.0") ++
  read_file("/var/log/flussonic/crash.log.1") ++
  read_file("/var/log/flussonic/console/erlang.log.1") ++
  read_file("/var/log/flussonic/console/erlang.log.2").

read_file(Path) ->
  case file:read_file(Path) of
    {ok, Bin} ->
      {ok, Info} = file:read_file_info(Path),
      [{Path,Bin,Info}];
    {error, _} ->
      []
  end.

ifaddrs() ->
  {ok, Info} = inet:getifaddrs(),
  Bin = iolist_to_binary(io_lib:format("~p~n", [Info])),
  [{"netstat", Bin, file_info(Bin)}].

version() ->
  Bin = iolist_to_binary(flu:version()),
  [{"version", Bin, file_info(Bin)}].

file_info(Bin) ->
  Now = erlang:localtime(),
  #file_info{type = regular, size = iolist_size(Bin), access = read, atime = Now, mtime = Now, ctime = Now, mode = 0644, links = 1, uid = 0, gid = 0}.


create_zip(Files) ->
  {ok, {_,Zip}} = zip:create("logs.zip", Files, [memory]),
  Zip.



request(Zip) ->
  {ok, S} = gen_tcp:connect("erlyvideo.org", 80, [binary,{packet,http},{active,false}]),
  ok = gen_tcp:send(S, ["PUT /logs/upload HTTP/1.1\r\nHost: erlyvideo.org\r\n"
    "X-Version: ", flu:version(),"\r\n"
    "Content-Length: ", integer_to_list(size(Zip)), "\r\n\r\n",
  Zip]),
  {ok, {http_response, _HttpVersion, 200, _HttpString}} = gen_tcp:recv(S,0),
  ContentLength = read_headers(S, undefined),
  is_integer(ContentLength) orelse error(invalid_content_length),
  inet:setopts(S,[{packet,raw},{active,false}]),
  {ok, JSON} = gen_tcp:recv(S,ContentLength),
  gen_tcp:close(S),
  {struct, Reply} = mochijson2:decode(JSON),
  {_,Ticket} = lists:keyfind(<<"ticket">>,1,Reply),
  {ok,Ticket}.

read_headers(S, BodyLen) ->
  case gen_tcp:recv(S,0) of
    {ok, http_eoh} -> BodyLen;
    {ok, {http_header, _, 'Content-Length', _, Length}} -> read_headers(S, list_to_integer(Length));
    {ok, {http_header, _, _K, _, _V}} -> read_headers(S,BodyLen)
  end.


