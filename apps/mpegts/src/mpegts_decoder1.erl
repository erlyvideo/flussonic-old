-module(mpegts_decoder1).

-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0]).
-export([decode/1, read_file/1]).


-define(PAT_PID, 0).

-define(DD(Fmt,X), ?debugFmt(Fmt, X)).


read_file(Path) ->
  case file:open(Path, [binary,read,raw]) of
    {ok, F} ->
      Decoder = init(),
      {ok, Frames1, Decoder1} = read_file_loop(F, Decoder, []),
      {ok, Frames2, _Decoder2} = decode(eof, Decoder1, Frames1),
      file:close(F),
      {ok, lists:reverse(Frames2)};
    {error, _} = Error ->
      Error
  end.

read_file_loop(F, Decoder, Acc) ->
  case file:read(F, 188*100) of
    {ok, Bin} ->
      {ok, Frames, Decoder1} = decode(Bin, Decoder, Acc),
      read_file_loop(F, Decoder1, Frames);
    eof ->
      {ok, Acc, Decoder}
  end.



decode(Bin) ->
  Decoder = init(),
  {ok, Frames1, Decoder1} = decode(Bin, Decoder, []),
  {ok, Frames2, _Decoder2} = decode(eof, Decoder1, Frames1),
  {ok, lists:reverse(Frames2)}.


-record(stream, {
  pid,
  codec,
  dts,
  pts,
  sample_rate,
  es_buffer = []
}).

-record(decoder,{
  pmt,
  streams = []
}).

init() ->
  #decoder{}.


%% TS header
%% 16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, HasAdaptationField:1, HasPayload:1, Counter:4, Payload:184
%%
%%
%% PES header
%% 1:24, StreamId:8, PESLength:16, Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1,
%% OriginalOrCopy:1, PtsDtsIndicator:2, ESCR:1, ESRate:1, DSM:1, AdditionalCopy:1, CRC:1, Extension:1
%% 



decode(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder, Frames) ->
  {pat, [{PMT,_}|_]} = mpegts_psi:psi(ts_payload(Packet)),
  decode(Rest, Decoder#decoder{pmt = PMT}, Frames);

decode(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>> = Packet, #decoder{pmt = PMT, streams = Streams1} = Decoder, Frames) ->
  {pmt, Descriptors} = mpegts_psi:psi(ts_payload(Packet)),
  Streams2 = lists:foldl(fun(#pmt_entry{pid = Pid, codec = Codec}, Streams_) ->
    case lists:keyfind(Pid, #stream.pid, Streams_) of
      #stream{} -> Streams_;
      false -> [#stream{pid = Pid, codec = Codec}|Streams_]
    end
  end, Streams1, Descriptors),
  decode(Rest, Decoder#decoder{streams = Streams2}, Frames);

decode(<<16#47, _:1, 1:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{streams = Streams} = Decoder, Frames) ->
  {Streams2, NewFrames} = case lists:keytake(Pid, #stream.pid, Streams) of
    {value, #stream{es_buffer = Buffer} = Stream, Streams1} ->
      PES = ts_payload(Packet),
      <<1:24, _StreamId, _PESLength:16, _:8, PtsDts:2, _:6, HeaderLength, PESHeader:HeaderLength/binary, Data/binary>> = PES,
      % case PESLength of
      %   0 -> ok;
      %   _ -> PESLength = size(PES) - 6
      % end,
      {DTS, PTS} = pes_timestamp(PtsDts, PESHeader),
      {Stream1, NewFrames_} = flush_pes(Stream#stream{
        es_buffer = iolist_to_binary(lists:reverse([Data|Buffer]))
      }),
      % ?DD("pid: ~p, dts: ~p, frames: ~p", [Pid,round(DTS), length(NewFrames_)]),
      {[Stream1#stream{dts = DTS, pts = PTS}|Streams1], NewFrames_};
    false ->
      {Streams, []}
  end,
  decode(Rest, Decoder#decoder{streams = Streams2}, NewFrames ++ Frames);

decode(<<16#47, _:1, 0:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{streams = Streams} = Decoder, Frames) ->
  Streams2 = case lists:keytake(Pid, #stream.pid, Streams) of
    {value, #stream{es_buffer = Buffer} = Stream, Streams1} ->
      Data = ts_payload(Packet),
      [Stream#stream{es_buffer = [Data|Buffer]}|Streams1];
    false ->
      Streams
  end,
  decode(Rest, Decoder#decoder{streams = Streams2}, Frames);

decode(<<>>, #decoder{} = Decoder, Frames) ->
  {ok, Frames, Decoder};

decode(eof, #decoder{} = Decoder, Frames) ->
  {ok, Frames, Decoder}.


flush_pes(#stream{} = Stream) -> flush_pes(Stream, []).

flush_pes(#stream{codec = h264, es_buffer = Data, dts = DTS, pts = PTS, pid = Pid} = Stream, Acc) ->
  NALs1 = [NAL || NAL <- binary:split(Data, [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>],
  {NALs2, Tail} = lists:split(length(NALs1)-1, NALs1),
  NALs = [NAL || NAL <- NALs2, h264:type(NAL) =/= delim],
  NalTypes = [h264:type(NAL) || NAL <- NALs],
  {ConfigNals, PayloadNals} = lists:partition(fun(NAL) -> lists:member(h264:type(NAL), [sps,pps]) end, NALs),
  Config = case ConfigNals of
    [] -> [];
    _ -> [(h264:video_config(ConfigNals))#video_frame{dts = DTS, pts = DTS, track_id = Pid}]
  end,
  Keyframe = lists:member(idr, NalTypes),
  Payload = case PayloadNals of
    [] -> [];
    _ -> [#video_frame{
    content = video,
    track_id= Pid,
    body    = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- NALs, not lists:member(h264:type(NAL), [sps, pps])]),
    flavor  = if Keyframe -> keyframe; true -> frame end,
    codec   = h264,
    dts     = DTS,
    pts     = PTS
    }]
  end,
  {Stream#stream{es_buffer = Tail}, Payload ++ Config ++ Acc};


flush_pes(#stream{codec = aac, es_buffer = Data, sample_rate = undefined, pid = Pid, dts = DTS} = Stream, Acc) ->
  Config = aac:adts_to_config(Data),
  #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
  AudioConfig = #video_frame{
    content = audio,
    flavor  = config,
    track_id= Pid,
    dts     = DTS,
    pts     = DTS,
    body    = Config,
    codec   = aac
  },
  flush_pes(Stream#stream{sample_rate = SampleRate}, [AudioConfig|Acc]);

flush_pes(#stream{codec = aac, es_buffer = Data, dts = DTS, sample_rate = SampleRate, pid = Pid} = Stream, Acc) ->
  case aac:unpack_adts(Data) of
    {ok, AAC, Rest} ->
      Frame = #video_frame{     
        content = audio,
        flavor  = frame,
        track_id= Pid,
        dts     = DTS,
        pts     = DTS,
        body    = AAC,
        codec   = aac
      },
      flush_pes(Stream#stream{dts = DTS + 1024*1000 / SampleRate, es_buffer = Rest}, [Frame|Acc]);
    {more, _} ->
      {Stream#stream{es_buffer = [Data]}, Acc};
    {error, _} ->
      <<_, Rest/binary>> = Data,
      flush_pes(Stream#stream{es_buffer = Rest}, Acc)
  end.
























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

