#!/usr/bin/env escript

main([Path]) ->
  {ok, F} = file:open(Path, [read,binary,raw]),
  Counters = loop(1, F, []),
  io:format("~p~n", [Counters]).


loop(N, F, Counters) ->
  case file:read(F, 188) of
    {ok, <<16#47, _:3, Pid:13, _:2, Adaptation:1, _:1, Counter:4, TS/binary>> = Packet} ->
      PCR = extract_pcr(Packet),
      DTS = dts(Packet),
      io:format("~p ~p ~p ~p ~p~n", [N, Pid, Counter, DTS, PCR]),
      loop(N+1, F, lists:keystore(Pid, 1, Counters, {Pid,Counter}));
    eof ->
      Counters
  end.

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 0:1, 1:1, _Counter:4, Payload/binary>>)  -> 
  Payload;

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 1:1, 1:1, _Counter:4, 
              AdaptationLength, _AdaptationField:AdaptationLength/binary, Payload/binary>>) -> 
  Payload;

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              _Adaptation:1, 0:1, _Counter:4, _Payload/binary>>)  ->
  % ?D({"Empty payload on pid", _Pid}),
  <<>>.


dts(<<16#47, TS/binary>>) ->
  Payload = ts_payload(TS),
  extract_dts(Payload).

extract_pcr(<<16#47, _:16, _:2, 1:1, _:5, Length, _:3, PCR:1, _OPCR:1, _:3, Pcr1:33, Pcr2:9, _/bitstring>> = TS) when Length > 0 andalso PCR == 1 ->
  round(Pcr1 / 90 + Pcr2 / 27000);

extract_pcr(_) ->
  undefined.

extract_dts(<<1:24, _:5/binary, Length, _:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, _/bitstring>>) when Length > 0 ->
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  PTS1;

extract_dts(_) ->
  undefined.
