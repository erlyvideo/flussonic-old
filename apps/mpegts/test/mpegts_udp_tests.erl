-module(mpegts_udp_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


unicast_test() ->
  run_test(5077, []).


multicast_test() ->
  run_test(5077, [{multicast_ttl,4},{ip,{239,5,4,2}}]).


run_test(Port, Options) ->
  {ok, Input} = mpegts_udp:open(5077, Options),
  Addr = proplists:get_value(ip, Options, {127,0,0,1}),
  {ok, Output} = gen_udp:open(0, [binary]),
  gen_udp:connect(Output, Addr, Port),
  {ok, Bin} = file:read_file("../test/fixtures/fileSequence0.ts"),
  Chunks = chunks(Bin),
  Received = iolist_to_binary(send(Chunks, Output, Input)),
  ?assertEqual(size(Bin), size(Received)),
  ?assertEqual(Bin, Received).



send([Chunk|Chunks], Output, Input) ->
  ok = gen_udp:send(Output, Chunk),
  send(Chunks, Output, Input);

send([], Output, Input) ->
  gen_udp:close(Output),
  recv(Input).

recv(Input) ->
  receive
    {mpegts_udp, Input, Chunk} -> [Chunk|recv(Input)]
  after
    700 -> mpegts_udp:close(Input), []
  end.


chunks(<<Chunk:1316/binary, Bin/binary>>) ->
  [Chunk|chunks(Bin)];

chunks(Bin) ->
  [Bin].


