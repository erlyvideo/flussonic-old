-module(mpegts_decoder_c_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).



pids(<<16#47, _:3, Pid:13, _:185/binary, Rest/binary>>) -> [Pid|pids(Rest)];
pids(<<>>) -> [].


decoder_test1() ->
  {ok, Bin} = file:read_file("../test/fixtures/10-06800.ts"),
  Pids = pids(Bin),
  ?debugFmt("pids: ~240p ~B ~.1f ~n",[lists:usort(Pids), length(Pids), size(Bin) / 188]),
  decode_file(Bin, mpegts_decoder_c:init()),
  ok.

decode_file(<<Chunk:8192/binary, Rest/binary>>, Decoder) ->
  {ok, Decoder2, _Frames} = mpegts_decoder_c:decode(Chunk, Decoder),
  decode_file(Rest, Decoder2);

decode_file(Chunk, Decoder) ->
  mpegts_decoder_c:decode(Chunk, Decoder).

