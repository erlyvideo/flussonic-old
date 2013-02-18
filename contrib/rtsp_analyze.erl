#!/usr/bin/env escript

-mode(compile).


main(["record", URL]) ->
  code:add_pathz("apps/erlmedia/ebin"),
  code:add_pathz("apps/rtsp/ebin"),
  code:add_pathz("deps/lager/ebin"),
  code:add_pathz("apps/flussonic/ebin"),
  LocalPort = 4554,
  {ok, {rtsp, _, RemoteHost, RemotePort, _Path, _Query}} = http_uri:parse(URL, [{scheme_defaults,[{rtsp,554}]}]),

  Writer = spawn(fun() ->
    {ok, L} = gen_tcp:listen(LocalPort, [binary,{reuseaddr,true}]),
    accept_loop(L, RemoteHost, RemotePort)
  end),
  erlang:monitor(process, Writer),
  Consumer = spawn(fun() ->
    timer:send_interval(1000, flush),
    frame_consumer(0)
  end),
  {ok, Reader} = rtsp_reader:start_link(URL, [{consumer,Consumer},{hostport, {"127.0.0.1", LocalPort}}]),
  rtsp_reader:media_info(Reader),
  erlang:monitor(process, Reader),
  receive {'DOWN',_,_,Reader,_} -> ok end,
  receive {'DOWN',_,_,Writer,_} -> ok end,
  ok;

main([]) ->
  io:format("./contrib/rtsp_analyze.erl record URL\n");

main([Path]) ->
  {ok, Bin} = file:read_file(Path),
  read(Bin).


frame_consumer(Count) ->
  receive
    flush -> io:format("~3.. B frames~n", [Count]), frame_consumer(0);
    {'$gen_call', From, _VideoFrame} -> gen_server:reply(From, ok), frame_consumer(Count + 1);
    Else -> io:format("Consumer: ~p~n", [Else]), frame_consumer(Count)
  end.

read(<<>>) ->
  ok;

read(<<$$, Channel, Length:16, _RTP:Length/binary, Rest/binary>>) ->
  io:format("rtp ~B ~B~n", [Channel, Length]),
  read(Rest);

read(<<"RTSP/1.0 ", _/binary>> = Bin) ->
  [Response, RestBody] = binary:split(Bin, <<"\r\n\r\n">>),
  case re:run(Response, "Content-[lL]ength: (\\d+)", [{capture,all_but_first,list}]) of
    {match, [Len_]} ->
      Len = list_to_integer(Len_),
      <<Body:Len/binary, Rest/binary>> = RestBody,
      io:format("~s\r\n\r\n~s\n", [Response, Body]),
      read(Rest);
    nomatch ->
      io:format("~s\r\n", [Response]),
      read(RestBody)
  end.


accept_loop(L, RemoteHost, RemotePort) ->
  {M1,M2,M3} = os:timestamp(),
  T = (M1*1000000 + M2)*1000 + M3 div 1000,
  {ok, C} = gen_tcp:accept(L),
  DumpPath = io_lib:format("dump-~B.rtsp",[T]),
  {ok, F} = file:open(DumpPath, [binary, write, raw]),
  io:format("Accept and dump to ~s~n", [DumpPath]),
  inet:setopts(C, [binary,{active,once}]),
  {ok, S} = gen_tcp:connect(RemoteHost, RemotePort, [binary, {active,once}]),
  loop(C, S, F),
  gen_tcp:close(C),
  gen_tcp:close(S),
  accept_loop(L, RemoteHost, RemotePort).



loop(C, S, F) ->
  receive
    {tcp_closed, _} -> ok;
    {tcp, C, Bin} ->
      inet:setopts(C, [{active,once}]),
      gen_tcp:send(S, Bin),
      loop(C, S, F);
    {tcp, S, Bin} ->
      inet:setopts(S, [{active,once}]),  
      gen_tcp:send(C, Bin),
      file:write(F, Bin),
      loop(C, S, F);
    Else ->
      io:format("Else: ~p~n", [Else]),
      loop(C, S, F)
  end.
