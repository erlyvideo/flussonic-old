-module(mpegts_udp_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

udp_test_() ->
  case file:read_file_info("../test/fixtures") of
    {ok, _} ->
      [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
      lists:prefix("test_", atom_to_list(F))];
    {error, _} -> []
  end.


test_unicast() ->
  run_suite(5079, []).


test_multicast() ->
  run_suite(5078, [{multicast_ttl,4},{ip,{239,5,4,2}}]).


run_suite(Port, Options) ->
  Self = self(),
  Addr = proplists:get_value(ip, Options, {127,0,0,1}),
  {ok, Out} = gen_udp:open(0, [binary]),
  gen_udp:connect(Out, Addr, Port),
  {ok, Bin} = file:read_file("../test/fixtures/fileSequence0.ts"),
  Chunks = chunks(Bin),

  _Reader = proc_lib:spawn_link(fun() ->
    {ok, In} = mpegts_udp:open(Port, Options),
    mpegts_udp:active_once(In),
    Self ! shoot,
    Reply = reader(In, [], size(Bin)),
    Self ! {read, iolist_to_binary(Reply)}
  end),

  receive shoot -> ok end,
  [gen_udp:send(Out, Chunk) || Chunk <- Chunks],
  gen_udp:close(Out),
  receive
    {read, Received} ->
      ?assertEqual(size(Bin), size(Received)),
      ?assertEqual(Bin, Received)
  after
    5000 -> error(timeout)
  end.


reader(_In, Acc, 0) ->
  lists:reverse(Acc);

reader(In, Acc, Size) ->
  receive
    {mpegts_udp, In, Chunk} ->
      mpegts_udp:active_once(In),
      reader(In, [Chunk|Acc], Size - size(Chunk))
  after
    3700 -> lists:reverse(Acc)
  end.


chunks(<<Chunk:1316/binary, Bin/binary>>) ->
  [Chunk|chunks(Bin)];

chunks(Bin) ->
  [Bin].


