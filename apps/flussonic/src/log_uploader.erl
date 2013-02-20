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
  top() ++
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


top() ->
  Bin = iolist_to_binary(io_lib:format("~p~n", [ems_debug:dump()])),
  [{"top", Bin, file_info(Bin)}].



file_info(Bin) ->
  Now = erlang:localtime(),
  #file_info{type = regular, size = iolist_size(Bin), access = read, atime = Now, mtime = Now, ctime = Now, mode = 0644, links = 1, uid = 0, gid = 0}.


create_zip(Files) ->
  {ok, {_,Zip}} = zip:create("logs.zip", Files, [memory]),
  Zip.



request(Zip) ->
  Result = lhttpc:request("http://erlyvideo.org/logs/upload", "PUT", [{"X-Version", flu:version()}], Zip, 120000),
  case Result of
    {ok, {{200, _}, _Headers, JSON}} ->
      {struct, Reply} = mochijson2:decode(JSON),
      {_,Ticket} = lists:keyfind(<<"ticket">>,1,Reply),
      {ok,Ticket};
    _Else ->
      {error, uploading}
  end.



