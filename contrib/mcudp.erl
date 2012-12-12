#!/usr/bin/env escript
-mode(compile).


main(["udp://"++HostPort, OutPath]) ->
  [Host, Port_] = string:tokens(HostPort, ":"),
  Port = list_to_integer(Port_),
  {ok, Addr} = inet_parse:address(Host),
  Options = [binary,{ip,Addr},{active,true},{recbuf,65536},inet,{reuseaddr,true},{multicast_ttl,4},{multicast_loop,true}],
  {ok, Socket} = gen_udp:open(Port, Options),
  ok = inet:setopts(Socket,[{add_membership,{Addr,{0,0,0,0}}}]),
  {ok, F} = file:open(OutPath, [binary,write,raw]),
  Counters = dict:new(),
  loop(F, Counters).


loop(F, Counters) ->
  receive
    {udp,_, _,_, Bin} ->
      Packets = [{Pid,Counter} || <<16#47, _:3, Pid:13, _:4, Counter:4, _:184/binary>> <= Bin],
      Counters2 = lists:foldl(fun({Pid,Counter}, Counters1) ->
        case dict:fetch(Pid, Counters1) of
          {ok, Value} when (Value + 1) rem 16 == Counter -> ok;
          {ok, Value} when (Value + 1) rem 16 =/= Counter -> io:format("Pid ~p desync ~B -> ~B~n", [Pid,Value,Counter]);
          error -> ok
        end,
        dict:store(Pid,Counter,Counters1)
      end, Counters),
      % file:write(F, Bin)
      ok
  end,
  loop(F).

