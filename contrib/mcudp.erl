#!/usr/bin/env escript
-mode(compile).


main([]) ->
  io:format("~s udp://239.0.0.1:3000 [output.ts]~n", [escript:script_name()]),
  erlang:halt(1);

main(["udp://"++HostPort|Args]) ->

  [Host, Port_] = string:tokens(HostPort, ":"),
  Port = list_to_integer(Port_),
  {ok, Addr} = inet_parse:address(Host),
  Options = [binary,{ip,Addr},{active,true},{recbuf,65536},inet,{reuseaddr,true},{multicast_ttl,4},{multicast_loop,true}],
  {ok, Socket} = gen_udp:open(Port, Options),
  ok = inet:setopts(Socket,[{add_membership,{Addr,{0,0,0,0}}}]),
  F = case Args of
    [] -> undefined;
    [OutPath] ->
      {ok, F1} = file:open(OutPath, [binary,write,raw]),
      F1
  end,
  Counters = dict:new(),
  timer:send_interval(1000, dump),
  loop(F, Counters, 0).


loop(F, Counters, Count) ->
  receive
    {udp,_, _,_, Bin} ->
      Packets = [{Pid,Counter} || <<16#47, _:3, Pid:13, _:4, Counter:4, _:184/binary>> <= Bin],
      Counters2 = lists:foldl(fun({Pid,Counter}, Counters1) ->
        case dict:find(Pid, Counters1) of
          {ok, Value} when (Value + 1) rem 16 == Counter -> ok;
          {ok, Value} when (Value + 1) rem 16 =/= Counter -> io:format("Pid ~p desync ~B -> ~B~n", [Pid,Value,Counter]);
          error -> ok
        end,
        dict:store(Pid,Counter,Counters1)
      end, Counters, Packets),
      case F of
        undefined -> ok;
        _ -> file:write(F, Bin)
      end,
      loop(F, Counters2, Count + length(Packets));
    dump ->
      io:format("~B packets~n", [Count]),
      loop(F, Counters, 0);
    Else ->
      io:format("Strange: ~p~n", [Else]),
      ok
  end.
