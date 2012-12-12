-module(mpegts_decoder).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0]).
-export([decode/2]).
-export([read_file/1, decode_file/1]).
-export([media_info/1]).


-define(PAT_PID, 0).

-define(DD(Fmt,X), ?debugFmt(Fmt, X)).




read_file(Path) ->
  case file:open(Path, [binary,read,raw]) of
    {ok, F} ->
      Decoder = ?MODULE:init(),
      {ok, _Decoder1, Frames1} = read_file_loop(F, Decoder),
      file:close(F),
      {ok, Frames1};
    {error, _} = Error ->
      Error
  end.

read_file_loop(F, Decoder) ->
  case file:read(F, 8192) of
    {ok, Bin} ->
      {ok, Decoder1, Frames} = ?MODULE:decode(Bin, Decoder),
      {ok, Decoder2, Acc} = read_file_loop(F, Decoder1),
      {ok, Decoder2, Frames ++ Acc};
    eof ->
      {ok, Decoder1, Frames} = ?MODULE:decode(eof, Decoder),
      {ok, Decoder1, Frames}
  end.


decode_file(Bin) ->
  Decoder = ?MODULE:init(),
  {ok, Decoder1, Frames1} = ?MODULE:decode(Bin, Decoder),
  {ok, _Decoder2, Frames2} = ?MODULE:decode(eof, Decoder1),
  {ok, Frames1 ++ Frames2}.


%% Further goes real API

-define(DELAY, 7).


-record(stream, {
  pid,
  codec,
  dts,
  pts,
  sample_rate,
  es_buffer = <<>>,
  sent_config = false,
  switching = false,
  pes_header = undefined
}).

-record(decoder,{
  pmt,
  video,
  audio,
  ts_buffer = undefined,
  media_info = undefined,
  delayed = []
}).

decode(Bin, #decoder{delayed = Delayed} = Decoder) ->
  {ok, #decoder{media_info = MI} = Decoder1, Frames} = decode(Bin, Decoder, []),
  % Quick and dirty frame resorting
  Sorted = video_frame:sort(Frames ++ Delayed),
  {Send,Save} = if 
    Bin == eof -> {Sorted, []};
    length(Sorted) > 2*?DELAY -> lists:split(?DELAY, Sorted);
    true -> {[], Sorted}
  end,
  MI2 = case video_frame:has_media_info(MI) of
    true -> MI;
    false -> video_frame:define_media_info(MI, Sorted)
  end,
  {ok, Decoder1#decoder{delayed = Save, media_info = MI2}, Send}.


media_info(#decoder{media_info = MI}) ->
  case video_frame:has_media_info(MI) of
    true -> MI;
    false -> undefined
  end.



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

decode(NewTS, #decoder{ts_buffer = <<16#47, _/binary>> = OldTS} = Decoder, []) when size(NewTS) + size(OldTS) < 188 ->
  {ok, Decoder#decoder{ts_buffer = <<OldTS/binary, NewTS/binary>>}, []};

decode(NewTS, #decoder{ts_buffer = <<16#47, _/binary>> = OldTS} = Decoder, []) when size(NewTS) + size(OldTS) >= 188 ->
  ChunkSize = 188 - size(OldTS),
  <<Chunk:ChunkSize/binary, Rest/binary>> = NewTS,
  TS = <<OldTS/binary, Chunk/binary>>,
  {ok, Decoder1, Frames} = decode(TS, Decoder#decoder{ts_buffer = undefined}, []),
  decode(Rest, Decoder1, Frames);


decode(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder, Frames) ->
  {pat, [{PMT,_}|_]} = mpegts_psi:psi(ts_payload(Packet)),
  decode(Rest, Decoder#decoder{pmt = PMT}, Frames);

decode(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>> = Packet, #decoder{pmt = PMT, 
  media_info = MI} = Decoder, Frames) ->
  {pmt, Descriptors} = mpegts_psi:psi(ts_payload(Packet)),
  #decoder{audio = A, video = V} = Decoder2 = lists:foldl(fun
    (#pmt_entry{pid = Pid, codec = h264}, #decoder{video = #stream{} = V} = Decoder1) -> 
      Decoder1#decoder{video = V#stream{pid = Pid}};
    (#pmt_entry{pid = Pid, codec = aac},  #decoder{audio = #stream{} = A} = Decoder1) -> 
      Decoder1#decoder{audio = A#stream{pid = Pid}};

    (#pmt_entry{pid = Pid, codec = h264}, #decoder{video = undefined} = Decoder1) -> 
      Decoder1#decoder{video = #stream{pid = Pid, codec = h264}};

    (#pmt_entry{pid = Pid, codec = aac},  #decoder{audio = undefined} = Decoder1) -> 
      Decoder1#decoder{audio = #stream{pid = Pid, codec = aac}};

    (#pmt_entry{pid = _Pid, codec = _Codec}, Decoder1) ->
      % ?ERR("unknown codec ~p on pid ~p", [Codec, Pid]), 
      Decoder1
  end, Decoder, Descriptors),

  NewStreams = case V of 
    undefined -> []; 
    #stream{codec = Codec} -> [#stream_info{codec = Codec, track_id = 1, content = video}]
  end ++
  case A of 
    undefined -> []; 
    #stream{codec = Codec} -> [#stream_info{codec = Codec, track_id = 2, content = audio}]
  end,
  MI1 = case MI of
    undefined -> #media_info{};
    _ -> MI
  end,
  MI2 = MI1#media_info{streams = lists:ukeymerge(#stream_info.track_id, MI1#media_info.streams, NewStreams)},
  decode(Rest, Decoder2#decoder{media_info = MI2}, Frames);


decode(<<16#47, _:1, Start:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder, Frames) ->
  Data = ts_payload(Packet),
  {Decoder2, NewFrames} = case Decoder of
    #decoder{video = #stream{pid = Pid} = Stream} ->
      {Stream1, NewFrames_} = pes_data(Data, Stream, Start),
      {Decoder#decoder{video = Stream1}, NewFrames_};
    #decoder{audio = #stream{pid = Pid} = Stream} ->
      {Stream1, NewFrames_} = pes_data(Data, Stream, Start),
      {Decoder#decoder{audio = Stream1}, NewFrames_};
    #decoder{} ->
      {Decoder, []}
  end,
  decode(Rest, Decoder2, NewFrames ++ Frames);


decode(<<>>, #decoder{} = Decoder, Frames) ->
  {ok, Decoder, Frames};

decode(<<16#47, _/binary>> = TS, #decoder{ts_buffer = undefined} = Decoder, Frames) when size(TS) < 188 ->
  {ok, Decoder#decoder{ts_buffer = TS}, Frames};

% decode(<<_, TS/binary>>, Decoder, Frames) ->
%   decode(TS, Decoder, Frames);

decode(eof, #decoder{audio = A, video = V} = Decoder, Frames) ->
  {A1, AFrames} = case A of undefined -> {A, []}; _ -> pes_data(eof, A, 0) end,
  {V1, VFrames} = case V of undefined -> {V, []}; _ -> pes_data(eof, V, 0) end,

  {ok, Decoder#decoder{audio = A1, video = V1}, AFrames ++ VFrames ++ Frames}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                  PES level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pes_data(eof, #stream{} = Stream, 0) ->
  new_pes_packet(eof, undefined, undefined, Stream);

pes_data(Data, #stream{pes_header = PES} = Stream, 0) when size(PES) > 0 ->
  pes_data(<<PES/binary, Data/binary>>, Stream#stream{pes_header = undefined}, 1);

%%
%% PES header
%% 1:24, StreamId:8, PESLength:16, Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1,
%% OriginalOrCopy:1, PtsDtsIndicator:2, ESCR:1, ESRate:1, DSM:1, AdditionalCopy:1, CRC:1, Extension:1

pes_data(<<1:24, _:32, PtsDts:2, _:6, Len, Header:Len/binary, Data/binary>>, #stream{} = Stream, 1) ->
  {DTS, PTS} = case pes_timestamp(PtsDts, Header) of
    undefined -> {Stream#stream.dts + 40*90, Stream#stream.pts + 40*90};
    {D,P} -> {D,P}
  end,

  % ?debugFmt("new dts ~B ~s (~B buffer)", [DTS, Stream#stream.codec, size(Stream#stream.es_buffer)]),
  new_pes_packet(Data, DTS, PTS, Stream);

pes_data(<<1:24, _/binary>> = PES, #stream{pes_header = undefined} = Stream, 1) ->
  {Stream#stream{pes_header = PES}, []};

pes_data(Data, #stream{switching = {DTS, PTS}} = Stream, 0) ->
  new_pes_packet(Data, DTS, PTS, Stream);

pes_data(Data, #stream{es_buffer = Buffer, switching = false} = Stream, 0) ->
  {Stream#stream{es_buffer = <<Buffer/binary, Data/binary>>}, []}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                  ES level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% H264

new_pes_packet(NewData, DTS, PTS, #stream{codec = h264, dts = undefined} = Stream) ->
  {Stream#stream{es_buffer = NewData, dts = DTS, pts = PTS}, []};

new_pes_packet(eof, _, _, #stream{codec = h264} = Stream) ->
  {Stream1, Frames} = h264_frames(Stream),
  {Stream1#stream{es_buffer = <<>>}, Frames};

new_pes_packet(NewData, DTS, PTS, #stream{codec = h264, es_buffer = Buffer} = Stream) ->
  case binary:split(NewData, [<<1:32>>, <<1:24>>]) of
    [Tail,NewBuffer] ->
      % ?debugFmt("close old h264 frame ~B (~p)", [Stream#stream.dts, size(Buffer) + size(Tail)]),
      {Stream1, Frames} = h264_frames(Stream#stream{es_buffer = <<Buffer/binary, Tail/binary>>}),
      {Stream1#stream{es_buffer = NewBuffer, dts = DTS, pts = PTS, switching = false}, Frames};
    [_] ->
      {Stream#stream{es_buffer = <<Buffer/binary, NewData/binary>>, switching = {DTS,PTS}}, []}
  end;


%% AAC

new_pes_packet(eof, _DTS, _PTS, #stream{dts = DTS, codec = aac, es_buffer = Buffer} = Stream) ->
  new_pes_packet(Buffer, DTS, DTS, Stream#stream{es_buffer = <<>>});

new_pes_packet(Data, DTS, PTS, #stream{codec = aac, sample_rate = undefined} = Stream) ->
  {AudioConfig, SampleRate} = aac_config(DTS, Data),
  {Stream1, Frames} = new_pes_packet(Data, DTS, PTS, Stream#stream{sample_rate = SampleRate}),
  {Stream1, [AudioConfig|Frames]};


new_pes_packet(Data, DTS, PTS, #stream{codec = aac, es_buffer = Old, dts = OldDTS} = Stream) when size(Old) > 0 ->
  case aac_frame(OldDTS, <<Old/binary, Data/binary>>) of
    {ok, Frame, Rest} ->
      {Stream1, Frames} = new_pes_packet(Rest, DTS, PTS, Stream#stream{es_buffer = <<>>}),
      {Stream1, [Frame|Frames]};
    {more, _} ->
      new_pes_packet(Data, DTS, PTS, Stream#stream{es_buffer = <<>>});
    {error, _} ->
      new_pes_packet(Data, DTS, PTS, Stream#stream{es_buffer = <<>>})
  end;

new_pes_packet(Data, DTS, PTS, #stream{codec = aac, sample_rate = SampleRate} = Stream) ->
  case aac_frame(DTS, Data) of
    {ok, Frame, Rest} ->
      {Stream1, Frames} = new_pes_packet(Rest, DTS + 1024*90*1000 div SampleRate, PTS, Stream),
      {Stream1, [Frame|Frames]};
    {more, _} ->
      {Stream#stream{es_buffer = Data, dts = DTS}, []};
    {error, _} ->
      {Stream#stream{es_buffer = <<>>, dts = DTS}, []}
  end;

new_pes_packet(_, _, _, #stream{} = Stream) ->
  {Stream#stream{es_buffer = <<>>}, []}.





% more_pes_data(#stream{codec = aac, closing = true, old_buffer = OldBuffer, es_buffer = Buffer} = Stream, Acc) 
%   when size(OldBuffer) > 0 ->
%   more_pes_data(Stream#stream{old_buffer = <<>>, es_buffer = <<OldBuffer/binary, Buffer/binary>>}, Acc);



% more_pes_data(#stream{} = Stream, Acc) ->
%   {Stream#stream{es_buffer = <<>>}, Acc}.


aac_config(DTS, Data) when size(Data) > 0 ->
  Config = aac:adts_to_config(Data),
  #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
  AudioConfig = #video_frame{
    content = audio,
    flavor  = config,
    track_id= 2,
    dts     = DTS / 90,
    pts     = DTS / 90,
    body    = Config,
    codec   = aac
  },
  {AudioConfig, SampleRate}.


aac_frame(DTS, Data) ->
  case aac:unpack_adts(Data) of
    {ok, AAC, Rest} ->
      Frame = #video_frame{     
        content = audio,
        flavor  = frame,
        track_id= 2,
        dts     = DTS / 90,
        pts     = DTS / 90,
        body    = AAC,
        codec   = aac
      },
      {ok, Frame, Rest};
    Else ->
      Else
  end.


h264_frames(#stream{dts = DTS, pts = PTS, es_buffer = AnnexB, sent_config = SentConfig} = Stream) ->
  NALs = [NAL || NAL <- binary:split(AnnexB, [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>, h264:type(NAL) =/= delim],
  NalTypes = [h264:type(NAL) || NAL <- NALs],
  {ConfigNals, PayloadNals} = lists:partition(fun(NAL) -> lists:member(h264:type(NAL), [sps,pps]) end, NALs),
  Config = case ConfigNals of
    [] -> [];
    _ -> [(h264:video_config(ConfigNals))#video_frame{dts = DTS / 90, pts = DTS / 90, track_id = 1}]
  end,
  Keyframe = lists:member(idr, NalTypes),
  Payload = case PayloadNals of
    [] -> [];
    _ -> 
    % ?debugFmt("h264 fr ~B ~p", [DTS, [{h264:type(NAL), size(NAL)} || NAL <- NALs]]),
    [#video_frame{
    content = video,
    track_id= 1,
    body    = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- NALs, not lists:member(h264:type(NAL), [sps, pps])]),
    flavor  = if Keyframe -> keyframe; true -> frame end,
    codec   = h264,
    dts     = DTS / 90,
    pts     = PTS / 90
    }]
  end,
  Frames = Payload ++ case SentConfig of
    true -> [];
    false -> Config
  end,
  {Stream#stream{sent_config = SentConfig orelse length(Config) > 0}, Frames}.























pes_timestamp(2#11, <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
    2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1, _/binary>>) ->
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  <<DTS1:33>> = <<Dts1:3, Dts2:15, Dts3:15>>,
  {DTS1, PTS1};

pes_timestamp(2#10, <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, _/binary>>) ->
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  {PTS1, PTS1};

pes_timestamp(_, _) ->
  undefined.





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

