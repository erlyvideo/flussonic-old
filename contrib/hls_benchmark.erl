#!/usr/bin/env escript
%% -smp enable

-mode(compile).

main([URL]) ->
  % inets:start(),
  hls(URL);

main([Count_|URLs]) ->
  ets:new(hls_readers, [public, named_table]),
  ets:insert(hls_readers, {count, 0}),
  _Pids = start_readers(URLs, list_to_integer(Count_)),
  control(URLs),
  % [receive {'DOWN',_,_,Pid,_} -> ok end || Pid <- Pids],
  ok.

start_readers(URLs, Count) ->
  Pids = [begin
    proc_lib:spawn_link(fun() -> hls(URL) end),
    timer:sleep((5000 div Count)+5)
  end || URL <- URLs, _I <- lists:seq(1,Count)],
  [erlang:monitor(process,Pid) || Pid <- Pids],
  Pids.  

control(URLs) ->
  case io:get_line("command> ") of
    [$a|Count_] when length(Count_) > 1 ->
      Count = list_to_integer(string:strip(Count_,both,$\n)),
      start_readers(URLs, Count),
      control(URLs);
    [$i|_] ->
      io:format("count: ~p~n", [ets:lookup_element(hls_readers, count, 2)]),
      control(URLs);
    [$q|_] ->
      halt(0);
    _ ->
      control(URLs)
  end.


hls(URL) ->
  Sess = integer_to_list(erlang:phash2(erlang:now())),
  put(t1, erlang:now()),
  io:format("start hls reader ~s~n", [URL]),
  ets:update_counter(hls_readers, count, 1),
  try hls0(URL,-1,Sess, undefined)
  catch
    Class:Error -> 
      io:format("HLS(~p) ~p:~p~n~p~n", [URL, Class, Error, erlang:get_stacktrace()]),
      ets:update_counter(hls_readers, count, -1),
      ok
  end.
    

hls0(URL, Num, Sess, Socket) ->
  {ok, Socket1, Body} = fetch(URL ++ "/index.m3u8?session="++Sess, Socket),
  {Segments, IsFile} = parse_playlist(binary:split(Body, <<"\n">>, [global]), URL),
  case [Seg || {Seq,_,_} = Seg <- Segments, Seq > Num] of
    [] when IsFile ->
      io:format("file ~s ready~n", [URL]),
      ok;
    [] ->
      timer:sleep(2000 + random:uniform(1000)),
      hls0(URL, Num, Sess, Socket1);
    [{Seq,Duration,SegUrl}|_] ->
      if Seq > Num + 1 andalso Num =/= -1 ->
        io:format("Delayed: ~p -> ~p~n",[Num ,Seq]);
      true -> ok end,
      Timer = erlang:start_timer(Duration, self(), resume),
      Socket2 = case fetch(SegUrl, Socket1) of
        {ok, Socket2_, _} -> Socket2_;
        {error, Reason} ->
          io:format("Failed to download ~s: ~p~n", [SegUrl, Reason]),
          undefined
      end,
      Remain = erlang:read_timer(Timer),
      if Remain - 0 > 400 -> ok;
        true -> io:format("Delayed fetching segment ~s~n",[SegUrl])
      end,
      %io:format("~6.. B fetched ~s (~p)~n",[timer:now_diff(erlang:now(),get(t1)) div 1000, SegUrl, Remain]),
      case IsFile of
        false -> erlang:cancel_timer(Timer);
        true -> 
          receive
            {timeout, Timer, _} -> ok
          end
      end,
      hls0(URL, Seq, Sess, Socket2)
  end.


to_f(Bin) when is_binary(Bin) -> to_f(binary_to_list(Bin));
to_f(List) -> case (catch list_to_float(List)) of
    {'EXIT', _} -> list_to_integer(List);
    Float -> Float
  end.

parse_playlist([], {_,_,Segments, IsFile}) ->
  {Segments, IsFile};
parse_playlist([<<"#EXTINF:", Duration/binary>>, Path|Playlist], {Seq, URL, Segments, IsFile}) ->
  parse_playlist(Playlist, {Seq+1, URL, Segments ++ [
    {Seq, round(to_f(binary:part(Duration,{0,size(Duration)-1}))*1000), URL ++ "/" ++ binary_to_list(Path)}
  ], IsFile});

parse_playlist([<<"#EXT-X-MEDIA-SEQUENCE:", Seq/binary>>|Playlist], URL) ->
  parse_playlist(Playlist, {to_f(Seq), URL, [], false});

parse_playlist([<<"#EXT-X-ENDLIST">>|Playlist], {Seq,URL, Segments, _}) ->
  parse_playlist(Playlist, {Seq,URL, Segments, true});

parse_playlist([_|Playlist], Acc) ->
  parse_playlist(Playlist, Acc).



-define(TIMEOUT, 10000).

fetch(URL, Socket) ->
  fetch(URL, Socket, 5).

fetch(URL, Socket, Count) ->
  try fetch0(URL, Socket, Count)
  catch
    throw:{error, Reason} ->
      {error, Reason};
    throw:{restart, URL, NewCount} ->
      fetch(URL, undefined, NewCount)
  end.

fetch0(URL, undefined, Count) ->
  {ok, {http, _, Host, Port, _, _}} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,false}, {send_timeout, ?TIMEOUT}]),
  fetch0(URL, Socket, Count);

fetch0(URL, Socket, RetryCount) ->
  {ok, {http, Host, _, _, Path, Query}} = http_uri:parse(URL),
  Request = [<<"GET ">>, Path, Query, <<" HTTP/1.1\r\nHost: ">>, Host, <<"\r\nConnection: keepalive\r\n\r\n">>],
  inet:setopts(Socket, [{active,false}, {packet, http_bin}]),
  gen_tcp:send(Socket, Request),
  case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
    {ok, {http_response, _HttpVersion, 200, _HttpString}} ->
      ok;
    {ok, {http_response, _HttpVersion, Code, _HttpString}} ->
      gen_tcp:close(Socket),
      throw({error, Code});
    {error, closed} when RetryCount > 1 ->
      gen_tcp:close(Socket),
      throw({restart, URL, RetryCount - 1});
    {error, Error} ->
      gen_tcp:close(Socket),
      throw({error, Error})
  end,

  HeaderRead = fun(F, Sock, BodyLen) ->
    case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
      {ok, http_eoh} -> BodyLen;
      {ok, {http_header, _, 'Content-Length', _, Length}} ->
        F(F, Socket, list_to_integer(binary_to_list(Length)));
      {ok, {http_header, _, _Header, _, _Value}} ->
        F(F, Sock, BodyLen);
      % {error, closed} when RetryCount > 1 ->
      %   gen_tcp:close(Socket),
      %   throw({restart, URL, RetryCount - 1});
      {error, Reason} ->
        gen_tcp:close(Sock),
        throw({error, {header,Reason}})
    end
  end,
  case HeaderRead(HeaderRead, Socket, undefined) of
    undefined ->
      {ok, Socket, undefined};
    ContentLength ->
      inet:setopts(Socket, [{packet,raw}]),
      case gen_tcp:recv(Socket, ContentLength) of
        {ok, Body} ->
          % io:format("Fetched ~s (~B bytes)~n", [URL, size(Body)]),
          {ok, Socket, Body};
        {error, Reason} ->
          gen_tcp:close(Socket),
          throw({error, {body, ContentLength, Reason}})
      end
  end.





