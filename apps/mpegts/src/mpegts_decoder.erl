-module(mpegts_decoder).

-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").


-export([read_file/1, decode_file/1, decode_stream/2]).



% -define(DD(Fmt,X), io:format("ts_reader:~p "++Fmt++"~n", [?LINE|X])).
-define(DD(Fmt,X), ok).


-record(decoder, {
  pmt_pid,
  saved_frames = [],
  stream_map = [],
  stream1,
  stream2,
  stream3,
  stream4,
  stream5,
  stream6,
  stream7,
  stream8
}).

-define(STREAM_COUNT, 8).

-record(stream, {
  pid,
  dts,
  pts,
  codec,
  specific,
  length,
  ts_buffer = [],
  es_buffer = <<>>
}).

-define(PAT_PID, 0).


-spec read_file(string()) -> {ok, [any()]}.
read_file(Path) ->
  {ok, Bin} = file:read_file(Path),
  decode_file(Bin).

-spec decode_file(binary()) -> {ok, [any()]}.

decode_file(Bin) when is_binary(Bin) ->
  {ok, Frames1, Decoder1, <<>>} = decode(Bin, #decoder{}),
  {ok, Frames2} = decode(eof, Decoder1),
  Frames = case Frames2 of [] -> Frames1; _ -> Frames1 ++ Frames2 end,
  {ok, video_frame:sort(Frames)}.



-spec decode_stream(binary(), undefined | any()) -> {ok, [any()], any(), binary()}.
decode_stream(Bin, undefined) ->
  decode_stream(Bin, #decoder{});

decode_stream(Bin, #decoder{saved_frames = Saved} = Decoder) ->
  {ok, Frames, Decoder1, Rest} = decode(Bin, Decoder),
  if length(Frames) + length(Saved) < 15 ->
    {ok, [], Decoder, Bin};
  true ->
    {Send, Save} = video_frame:sort(Frames ++ Saved),
    {ok, Send, Decoder1#decoder{saved_frames = Save}, Rest}
  end.


decode(Bin, #decoder{} = Decoder) ->
  decode(Bin, Decoder, []).


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
  end;

ts_payload(<<16#47, _:18, 1:1, 0:1, _:4, AdaptationLength, _AdaptationField:AdaptationLength/binary, Rest/binary>>) ->
  Length = 183 - AdaptationLength,
  case Rest of
    <<Payload:Length/binary, _/binary>> -> Payload;
    _ -> eof
  end;

ts_payload(<<16#47, _:18, 0:1, 0:1, _:4, _:184/binary, _/binary>>)  -> 
  erlang:error(invalid_payload).






decode(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder, Acc) ->
  {pat, [{PMT,_}]} = mpegts_psi:psi(ts_payload(Packet)),
  % io:format("pid 0: PAT with pmt on pid ~p~n", [PMT]),
  decode(Rest, Decoder#decoder{pmt_pid = PMT}, Acc);

decode(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>> = Packet, #decoder{pmt_pid = PMT} = Decoder, Acc) ->
  {pmt, Descriptors} = mpegts_psi:psi(ts_payload(Packet)),
  % io:format("pid ~B: PMT with streams: ", [PMT]),
  % [io:format("~s on ~B; ", [Codec, Pid]) || #pmt_entry{codec = Codec, pid = Pid} <- Descriptors],
  % io:format("~n"),
  Decoder1 = lists:foldl(fun(#pmt_entry{codec = Codec, pid = Pid}, #decoder{stream_map = Streams_} = Decoder_) ->
    case proplists:get_value(Pid, Streams_) of
      undefined when Streams_ == [] ->
        Decoder_#decoder{stream_map = [{Pid,1}], stream1 = #stream{pid = Pid, codec = Codec}};
      undefined ->
        Num = lists:max([N || {_Pid,N} <- Streams_]) + 1,
        setelement(#decoder.stream_map + Num, Decoder_#decoder{stream_map = [{Pid,Num}|Streams_]},#stream{pid = Pid, codec = Codec});
      _ -> 
        Decoder_
    end
  end, Decoder, Descriptors),
  ?DD("Stream mapping: ~p", [Decoder1#decoder.stream_map]),
  decode(Rest, Decoder1, Acc);

decode(<<16#47, _:1, 1:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{stream_map = Streams} = Decoder, Acc) ->
  
  case proplists:get_value(Pid, Streams) of
    undefined ->
      decode(Rest, Decoder, Acc);
    Num ->
      Stream = element(#decoder.stream_map + Num, Decoder),
      {ok, Reply, Stream1} = flush(Stream),
      PES = ts_payload(Packet),
      <<1:24, _StreamId, PESLength1:16, _:8, PtsDts:2, _:6, HeaderLength, PESHeader:HeaderLength/binary, Data/binary>> = PES,
      PESLength = case PESLength1 of
        0 -> 0;
        _ -> PESLength1 - 3 - HeaderLength
      end,
      {DTS, PTS} = pes_timestamp(PtsDts, PESHeader),
      ?DD("pid ~B: start PES ~4s(~.2f, ~.2f) length: ~B", [Pid, Stream#stream.codec, DTS, PTS, PESLength1]),
      Stream2 = Stream1#stream{dts = DTS, pts = PTS, length = PESLength, ts_buffer = [Data]},
      Acc1 = case Reply of
        [] -> Acc;
        _ -> Acc ++ Reply
      end,
      decode(Rest, setelement(#decoder.stream_map+Num, Decoder, Stream2), Acc1)
  end;

decode(<<16#47, _:3, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{stream_map = Streams} = Decoder, Acc) ->
  case proplists:get_value(Pid, Streams) of
    undefined ->
      decode(Rest, Decoder, Acc);
    Num ->
      #stream{ts_buffer = Buffer, length = Length} = Stream = element(#decoder.stream_map + Num, Decoder),
      Data = ts_payload(Packet),
      case iolist_size(Buffer) + size(Data) of
        _ when Length == undefined ->
          decode(Rest, Decoder, Acc);
        L when L == Length ->
          ?DD("pid ~B: PES ready (~B)", [Pid, L]),
          {ok, Reply, Stream1} = flush(Stream#stream{ts_buffer = [Data|Buffer]}),
          Acc1 = case Reply of
            [] -> Acc;
            _ -> Acc ++ Reply
          end,
          decode(Rest, setelement(#decoder.stream_map+Num,Decoder, Stream1), Acc1);
        L when L < Length orelse Length == 0 ->
          ?DD("pid ~B: PES continue (~B/~B)", [Pid, L, Length]),
          decode(Rest, setelement(#decoder.stream_map+Num,Decoder, Stream#stream{ts_buffer = [Data|Buffer]}), Acc)
      end
  end;

decode(eof, #decoder{stream_map = StreamNums} = Decoder, Acc) ->
  Streams = [element(#decoder.stream_map + N, Decoder) || {_Pid,N} <- StreamNums],
  Acc1 = lists:foldl(fun(Stream, Acc_) ->
    {ok, Reply, _Stream1} = flush(Stream),
    case Reply of [] -> Acc_; _ -> Acc_ ++ Reply end
  end, Acc, Streams),
  {ok, Acc1}; 

decode(<<>>, #decoder{} = Decoder, Acc) ->
  {ok, Acc, Decoder, <<>>};

decode(<<16#47, _/binary>> = Packet, #decoder{} = Decoder, Acc) when size(Packet) < 188->
  {ok, Acc, Decoder, Packet};

decode(<<_, Rest/binary>>, Decoder, Acc) ->
  ?D(lost_sync),
  try_sync(Rest, Decoder, Acc, 1).


try_sync(<<16#47, _:187/binary, 16#47, _:187/binary, 16#47, _/binary>> = Data, Decoder, Acc, _Count) ->
  decode(Data, Decoder, Acc);

try_sync(_, _, _, 1024) ->
  erlang:error(cannot_sync_mpegts);

try_sync(<<>>, Decoder, Acc, _) ->
  {ok, Acc, Decoder, <<>>};

try_sync(<<_, Rest/binary>>, Decoder, Acc, Count) ->
  try_sync(Rest, Decoder, Acc, Count + 1).




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


flush(#stream{codec = Codec, ts_buffer = TSBuffer, es_buffer = ESBuffer} = Stream) when length(TSBuffer) > 0 ->
  Body = iolist_to_binary([ESBuffer|lists:reverse(TSBuffer)]),
  case Codec of
    h264 ->
      {ok, Frames, Stream1} = unpack_h264(Stream, Body),
      % io:format("~20s h264 frame count ~B~n", ["", length(Frames)]),
      {ok, Frames, Stream1};
    aac ->
      {ok, Frames, Stream1} = unpack_aac(Stream, Body),
      % io:format("~20s  aac frame count ~B~n", ["", length(Frames)]),
      {ok, Frames, Stream1}
  end;
  

flush(#stream{ts_buffer = []} = Stream) ->
  {ok, [], Stream}.


unpack_h264(#stream{dts = DTS, pts = PTS, specific = H264_0} = Stream, Bin) ->
  H264_1 = case H264_0 of
    undefined -> h264:init();
    _ -> H264_0
  end,
  
  NALs1 = [NAL || NAL <- binary:split(Bin, [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>],
  NALs2 = [NAL || NAL <- NALs1, h264:type(NAL) =/= delim],
  ConfigNals = [NAL || NAL <- NALs1, lists:member(h264:type(NAL), [sps, pps])],
  
  Keyframe = length(ConfigNals) > 0 orelse length([NAL || NAL <- NALs2, h264:type(NAL) == idr]) > 0, 
  H264_2 = lists:foldl(fun(NAL, H264) -> h264:parse_nal(NAL, H264) end, H264_1, ConfigNals),
  
  ConfigFrame = case {h264:has_config(H264_1), h264:has_config(H264_2)} of
    {false, true} -> [(h264:video_config(H264_2))#video_frame{dts = DTS, pts = DTS}];
    _ -> []
  end,
  
  Frame = #video_frame{
   	content = video,
		body    = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- NALs2]),
		flavor  = if Keyframe -> keyframe; true -> frame end,
		codec   = h264,
		dts     = DTS,
		pts     = PTS
  },
  
  {ok, ConfigFrame ++ [Frame], Stream#stream{specific = H264_2, dts = undefined, pts = undefined}}.
  
unpack_aac(#stream{dts = DTS, specific = undefined} = Stream, Bin) ->
  Config = aac:adts_to_config(Bin),
  #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
  AudioConfig = #video_frame{
   	content = audio,
   	flavor  = config,
		dts     = DTS,
		pts     = DTS,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
	},
  {ok, Frames, Stream1} = unpack_aac(Stream#stream{specific = SampleRate}, Bin),
  {ok, [AudioConfig|Frames], Stream1};

unpack_aac(#stream{} = Stream, <<>>) ->
  {ok, [], Stream#stream{ts_buffer = []}};

unpack_aac(#stream{dts = DTS, specific = SampleRate} = Stream, Bin) ->
  case aac:unpack_adts(Bin) of
    {ok, AAC, Rest} ->
      Frame = #video_frame{     
        content = audio,
        flavor  = frame,
        dts     = DTS,
        pts     = DTS,
        body    = AAC,
    	  codec	  = aac,
    	  sound	  = {stereo, bit16, rate44}
      },
      {ok, Frames, Stream1} = unpack_aac(Stream#stream{dts = DTS + 1024*1000 / SampleRate}, Rest),
      {ok, [Frame|Frames], Stream1};
    {more, _Rest} ->
      {ok, [], Stream#stream{es_buffer = Bin}};
    {error, Bin} ->
      error({desync_adts,Bin})
  end.

