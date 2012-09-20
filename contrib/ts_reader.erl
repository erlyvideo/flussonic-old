#!/usr/bin/env ERL_LIBS=apps escript

-include_lib("mpegts/include/mpegts_psi.hrl").

-mode(compile).

-define(D(X), io:format("ts_reader:~p ~p~n", [?LINE, X])).

main([Path]) ->
  Root = filename:join(filename:dirname(escript:script_name()), ".."),
  [code:add_pathz(P) || P <- filelib:wildcard(Root ++ "/apps/*/ebin")],
  
  {ok, Bin} = file:read_file(Path),
  
  decode(Bin).



-record(decoder, {
  pmt_pid,
  streams = []
}).

-record(stream, {
  pid,
  dts,
  pts,
  codec,
  length,
  buffer = []
}).

-define(PAT_PID, 0).


decode(Bin) ->
  decode(Bin, #decoder{}, []).


%% TS header
%% 16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, HasAdaptationField:1, HasPayload:1, Counter:4, Payload:184
%%
%%
%% PES header
%% 1:24, StreamId:8, PESLength:16, Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1,
%% OriginalOrCopy:1, PtsDtsIndicator:2, ESCR:1, ESRate:1, DSM:1, AdditionalCopy:1, CRC:1, Extension:1
%% 

ts_payload(<<16#47, _:18, 0:1, 1:1, _:4, Payload:184/binary, _/binary>>)  -> 
  Payload;

ts_payload(<<16#47, _:18, 1:1, 1:1, _:4, AdaptationLength, _AdaptationField:AdaptationLength/binary, Rest/binary>>) ->
  Length = 183 - AdaptationLength,
  case Rest of
    <<Payload:Length/binary, _/binary>> -> Payload;
    _ -> eof
  end.




decode(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder, Acc) ->
  {pat, [{PMT,_}]} = mpegts_psi:psi(ts_payload(Packet)),
  io:format("pid 0: PAT with pmt on pid ~p~n", [PMT]),
  decode(Rest, Decoder#decoder{pmt_pid = PMT}, Acc);

decode(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>> = Packet, #decoder{pmt_pid = PMT, streams = Streams} = Decoder, Acc) ->
  {pmt, Descriptors} = mpegts_psi:psi(ts_payload(Packet)),
  io:format("pid ~B: PMT with streams: ", [PMT]),
  [io:format("~s on ~B; ", [Codec, Pid]) || #pmt_entry{codec = Codec, pid = Pid} <- Descriptors],
  io:format("~n"),
  Streams1 = lists:foldl(fun(#pmt_entry{codec = Codec, pid = Pid}, Streams_) ->
    case lists:keymember(Pid, #stream.pid, Streams_) of
      true -> Streams_;
      false -> [#stream{pid = Pid, codec = Codec}|Streams_]
    end
  end, Streams, Descriptors),
  decode(Rest, Decoder#decoder{streams = Streams1}, Acc);

decode(<<16#47, _:1, 1:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{streams = Streams} = Decoder, Acc) ->
  case lists:keytake(Pid, #stream.pid, Streams) of
    {value, #stream{codec = Codec} = Stream, Streams1} ->
      {ok, Reply, Stream1} = flush(Stream),
      PES = ts_payload(Packet),
      <<1:24, _StreamId, PESLength1:16, _:8, PtsDts:2, _:6, HeaderLength, PESHeader:HeaderLength/binary, Data/binary>> = PES,
      PESLength = case PESLength1 of
        0 -> 0;
        _ -> PESLength1 - 3 - HeaderLength
      end,
      {DTS, PTS} = pes_timestamp(PtsDts, PESHeader),
      io:format("pid ~B: start PES ~4s(~.2f, ~.2f) length: ~B~n", [Pid, Codec, DTS, PTS, PESLength1]),
      Stream2 = Stream1#stream{dts = DTS, pts = PTS, length = PESLength, buffer = [Data]},
      Acc1 = case Reply of
        [] -> Acc;
        _ -> Acc ++ Reply
      end,
      decode(Rest, Decoder#decoder{streams = [Stream2|Streams1]}, Acc1);
    false ->
      decode(Rest, Decoder, Acc)
  end;

decode(<<16#47, _:3, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{streams = Streams} = Decoder, Acc) ->
  case lists:keytake(Pid, #stream.pid, Streams) of
    {value, #stream{buffer = Buffer, length = Length} = Stream, Streams1} ->
      Data = ts_payload(Packet),
      case iolist_size(Buffer) + size(Data) of
        _ when Length == undefined ->
          decode(Rest, Decoder, Acc);
        L when L == Length ->
          io:format("pid ~B: PES ready (~B)~n", [Pid, L]),
          {ok, Reply, Stream1} = flush(Stream),
          Acc1 = case Reply of
            [] -> Acc;
            _ -> Acc ++ Reply
          end,
          decode(Rest, Decoder#decoder{streams = [Stream1|Streams1]}, Acc1);
        L when L < Length orelse Length == 0 ->
          io:format("pid ~B: PES continue (~B/~B)~n", [Pid, L, Length]),
          decode(Rest, Decoder#decoder{streams = [Stream#stream{buffer = [Data|Buffer]}|Streams1]}, Acc)
      end;    
    false ->
      decode(Rest, Decoder, Acc)
  end;

decode(<<>>, #decoder{} = Decoder, Acc) ->
  {ok, Acc, Decoder}.




%% PES handling

pes_timestamp(2#11, <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
    2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1, _/binary>>) ->
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  <<DTS1:33>> = <<Dts1:3, Dts2:15, Dts3:15>>,
  {DTS1 / 90, PTS1 / 90};

pes_timestamp(2#10, <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, _/binary>>) ->
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  {PTS1/90, PTS1/90};

pes_timestamp(_, _) ->
  erlang:error(stream_without_pts_dts).


flush(#stream{codec = Codec, buffer = Buffer} = Stream) when length(Buffer) > 0 ->
  Body = iolist_to_binary(lists:reverse(Buffer)),
  case Codec of
    h264 ->
      NALS = [NAL || NAL <- binary:split(Body, [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>],
      Types = [{h264:type(NAL), size(NAL)} || NAL <- NALS],
      io:format("~20s h264 frame: ~p~n", ["", Types]),
      ok;
    aac ->
      io:format("~20s  aac frame~n", [""]),
      ok
  end,
  {ok, [], Stream#stream{dts = undefined, pts = undefined, buffer = []}};

flush(#stream{buffer = []} = Stream) ->
  {ok, [], Stream}.




