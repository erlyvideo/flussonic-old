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


-export([bench/1]).

-define(PAT_PID, 0).
-define(MAX_DELAY, 50).

-define(DD(Fmt,X), ?debugFmt(Fmt, X)).



bench(Path) ->
  {ok, F} = file:open(Path, [read,raw,binary]),
  Decoder = ?MODULE:init(),
  T1 = erlang:now(),
  Count = bench_loop(F, Decoder, 0, 0),
  T2 = erlang:now(),
  Delta = timer:now_diff(T2,T1),
  io:format("~B frames in ~B us, ~B us/frame~n", [Count, Delta, Delta div Count]),
  ok.

bench_loop(F, Decoder, Offset, Count) ->
  case file:pread(F, Offset, 819200) of
    {ok, Bin} ->
      {ok, Decoder1, Frames} = ?MODULE:decode(Bin, Decoder),
      bench_loop(F, Decoder1, Offset + size(Bin), Count + length(Frames));
    eof ->
      Count
  end.




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

-define(DELAY, 10).

-type decoder() :: any().

-spec init() -> decoder().

-spec decode(Bin::binary(), Decoder::decoder()) -> {ok, Decoder1::decoder(), [Frame::video_frame()]}.






-record(stream, {
  pid,
  codec,
  dts,
  pts,
  sample_rate,
  private,
  es_buffer = [],
  sent_config = false,
  switching = false,
  pes_header = undefined,
  frames = []
}).

-record(decoder,{
  pmt,
  video,
  audio,
  ts_buffer = undefined,
  sent_media_info = false,
  media_info = undefined
}).

init() -> 
  #decoder{}.


media_info(#decoder{media_info = MI}) ->
  case video_frame:has_media_info(MI) of
    true -> MI;
    false -> undefined
  end.



% Don't remove these three cases. They are for hls_reader_SUITE
decode(<<"3006.ts">>,D) -> {ok, D, [#video_frame{track_id=1,dts = N*1000, flavor = case N rem 4 of 0 -> keyframe; _ -> frame end} || N <- lists:seq(0,11)]};
decode(<<"3007.ts">>,D) -> {ok, D, [#video_frame{track_id=1,dts = (12+N)*1000, flavor = case 12+N rem 4 of 0 -> keyframe; _ -> frame end} || N <- lists:seq(0,5)]};
decode(<<"3008.ts">>,D) -> {ok, D, [#video_frame{track_id=1,dts = (18+N)*1000, flavor = case 18+N rem 4 of 0 -> keyframe; _ -> frame end} || N <- lists:seq(0,10)]};
decode(<<"3009.ts">>,D) -> {ok, D, [#video_frame{track_id=1,dts = (29+N)*1000, flavor = case 29+N rem 4 of 0 -> keyframe; _ -> frame end} || N <- lists:seq(0,10)]};

decode(Bin, #decoder{} = Decoder) ->
  {ok, #decoder{sent_media_info = SentMI, media_info = MI} = Decoder1} = decode_ts(Bin, Decoder),
  {Frames, Decoder2} = flush_frames(Decoder1, Bin == eof),
  
  Decoder3 = case SentMI of
    true -> Decoder2;
    false ->
      case video_frame:has_media_info(MI) of
        true -> Decoder2;
        false -> 
          MI2 = video_frame:define_media_info(MI, Frames),
          Decoder2#decoder{media_info = MI2}
      end
  end,
  {ok, Decoder3, Frames}.




flush_frames(#decoder{video = V, audio = A} = Decoder, true) ->
  Frames = video_frame:sort(stream_frames(V) ++ stream_frames(A)),
  {Frames, Decoder};

flush_frames(#decoder{} = Decoder, false) ->
  flush_frames(Decoder).


stream_frames(undefined) -> [];
stream_frames(#stream{frames = Frames}) -> Frames.

flush_frames(#decoder{video = undefined, audio = undefined} = Decoder) ->
  {[], Decoder};

flush_frames(#decoder{video = undefined, audio = #stream{frames = Frames} = A} = Decoder) ->
  {Frames, Decoder#decoder{audio = A#stream{frames = []}}};

flush_frames(#decoder{video = #stream{frames = Frames} = V, audio = undefined} = Decoder) ->
  {Frames, Decoder#decoder{video = V#stream{frames = []}}};

flush_frames(#decoder{video = #stream{frames = VFrames} = V, 
                      audio = #stream{frames = AFrames} = A} = Decoder) ->
  {Frames, VFrames1, AFrames1} = merge_frames(VFrames, AFrames),
  {Frames, Decoder#decoder{video = V#stream{frames = VFrames1}, audio = A#stream{frames = AFrames1}}}.


merge_frames([#video_frame{dts = DTS1}=F|V], [#video_frame{dts = DTS2}|_] = A) when DTS1 =< DTS2 ->
  {Frames, V1,A1} = merge_frames(V,A),
  {[F|Frames], V1, A1};

merge_frames([#video_frame{dts = DTS1}|_]=V, [#video_frame{dts = DTS2}=F|A]) when DTS1 > DTS2 ->
  {Frames, V1,A1} = merge_frames(V,A),
  {[F|Frames], V1, A1};

merge_frames([], A) when length(A) > ?MAX_DELAY -> 
  {Send,A1} = lists:split(?MAX_DELAY,A), {Send, [], A1};

merge_frames(V, []) when length(V) > ?MAX_DELAY -> 
  {Send,V1} = lists:split(?MAX_DELAY,V), {Send, V1, []};

merge_frames([], A) -> {[], [], A};
merge_frames(V, []) -> {[], V, []}.


% buffer_frames(Delayed, Frames, _Closing = true) ->
%   {video_frame:sort(Frames ++ Delayed), []};

% buffer_frames([], [], false) ->
%   {[], []};

% buffer_frames(Delayed, Frames, false) ->
%   [#video_frame{content = Content}|_] = Delayed2 = lists:foldl(fun(Frame, Delayed1) ->
%     merge_sort(Frame, Delayed1)
%   end, Delayed, Frames),
%   {Send, Save} = flush_delayed(Delayed2, Content),
%   {Send, Save}.


% merge_sort(#video_frame{dts = DTS1, content = Content1}=F1, [#video_frame{dts = DTS2, content = Content2}|_] = Delayed) 
%   when DTS1 > DTS2 orelse 
%   (DTS1 == DTS2 andalso Content1 == audio andalso Content2 == video) ->
%   [F1|Delayed];

% merge_sort(#video_frame{dts = DTS1}=F1, [#video_frame{dts = DTS2}=F2|Delayed]) 
%   when DTS1 =< DTS2 ->
%   [F2|merge_sort(F1, Delayed)];

% merge_sort(F, []) ->
%   [F].


% flush_delayed([#video_frame{content = Content}=F|Delayed], Content) ->
%   {Send, Save} = flush_delayed(Delayed, Content),
%   {Send, [F|Save]};

% flush_delayed([], _Content) ->
%   {[], []};

% flush_delayed([#video_frame{content = Content1}|_] = Delayed, Content) when Content1 =/= Content ->
%   {lists:reverse(Delayed), []}.







%% TS header
%% 16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, HasAdaptationField:1, HasPayload:1, Counter:4, Payload:184
%%
%%
%% PES header
%% 1:24, StreamId:8, PESLength:16, Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1,
%% OriginalOrCopy:1, PtsDtsIndicator:2, ESCR:1, ESRate:1, DSM:1, AdditionalCopy:1, CRC:1, Extension:1
%% 

decode_ts(NewTS, #decoder{ts_buffer = <<16#47, _/binary>> = OldTS} = Decoder) when size(NewTS) + size(OldTS) < 188 ->
  {ok, Decoder#decoder{ts_buffer = <<OldTS/binary, NewTS/binary>>}};

decode_ts(NewTS, #decoder{ts_buffer = <<16#47, _/binary>> = OldTS} = Decoder) when size(NewTS) + size(OldTS) >= 188 ->
  ChunkSize = 188 - size(OldTS),
  <<Chunk:ChunkSize/binary, Rest/binary>> = NewTS,
  TS = <<OldTS/binary, Chunk/binary>>,
  {ok, Decoder1} = decode_ts(TS, Decoder#decoder{ts_buffer = undefined}),
  decode_ts(Rest, Decoder1);

decode_ts(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>>, #decoder{media_info = MI} = Decoder) when MI =/= undefined ->
  decode_ts(Rest, Decoder);

decode_ts(<<16#47, _:3, ?PAT_PID:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder) ->
  {pat, [{PMT,_}|_]} = mpegts_psi:psi(ts_payload(Packet)),
  decode_ts(Rest, Decoder#decoder{pmt = PMT});

% decode_ts(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>>, #decoder{media_info = MI, pmt = PMT} = Decoder) when MI =/= undefined ->
%   decode_ts(Rest, Decoder);

decode_ts(<<16#47, _:3, PMT:13, _:185/binary, Rest/binary>> = Packet, #decoder{pmt = PMT, 
  media_info = MI} = Decoder) ->
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
  decode_ts(Rest, Decoder2#decoder{media_info = MI2});


decode_ts(<<16#47, _:1, Start:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, #decoder{} = Decoder) ->
  Data = ts_payload(Packet),
  Decoder2 = case Decoder of
    #decoder{video = #stream{pid = Pid} = Stream} ->
      Stream1 = pes_data(Data, Stream, Start),
      Decoder#decoder{video = Stream1};
    #decoder{audio = #stream{pid = Pid} = Stream} ->
      Stream1 = pes_data(Data, Stream, Start),
      Decoder#decoder{audio = Stream1};
    #decoder{} ->
      Decoder
  end,
  decode_ts(Rest, Decoder2);

decode_ts(<<>>, #decoder{} = Decoder) ->
  {ok, Decoder};

decode_ts(<<16#47, _/binary>> = TS, #decoder{ts_buffer = undefined} = Decoder) when size(TS) < 188 ->
  {ok, Decoder#decoder{ts_buffer = TS}};

decode_ts(<<C, Bin/binary>>, #decoder{} = Decoder) when C =/= 16#47 ->
  % ?D(align),
  decode_ts(Bin, Decoder);

decode_ts(eof, #decoder{audio = A, video = V} = Decoder) ->
  A1 = case A of undefined -> A; _ -> pes_data(eof, A, 0) end,
  V1 = case V of undefined -> V; _ -> pes_data(eof, V, 0) end,

  {ok, Decoder#decoder{audio = A1, video = V1}}.



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
  Stream#stream{pes_header = PES};

pes_data(Data, #stream{switching = {DTS, PTS}} = Stream, 0) ->
  new_pes_packet(Data, DTS, PTS, Stream);

pes_data(Data, #stream{es_buffer = Buffer, switching = false} = Stream, 0) ->
  Stream#stream{es_buffer = [Data|Buffer]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                  ES level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% H264

new_pes_packet(NewData, DTS, PTS, #stream{codec = h264, dts = undefined} = Stream) ->
  Stream#stream{es_buffer = [NewData], dts = DTS, pts = PTS};

new_pes_packet(eof, _, _, #stream{codec = h264, frames = Delayed} = Stream) ->
  {Stream1, Frames} = h264_frames(Stream),
  Stream1#stream{es_buffer = [], frames = Delayed ++ Frames};

new_pes_packet(NewData, DTS, PTS, #stream{codec = h264, es_buffer = Buffer, frames = Delayed} = Stream) ->
  case binary:split(NewData, [<<1:32>>, <<1:24>>]) of
    [Tail,NewBuffer] ->
      % ?debugFmt("close old h264 frame ~B (~p)", [Stream#stream.dts, size(Buffer) + size(Tail)]),
      {Stream1, Frames} = h264_frames(Stream#stream{es_buffer = [Tail|Buffer]}),
      Stream1#stream{es_buffer = [NewBuffer], dts = DTS, pts = PTS, switching = false, frames = Delayed ++ Frames};
    [_] ->
      Stream#stream{es_buffer = [NewData|Buffer], switching = {DTS,PTS}}
  end;


%% AAC


new_pes_packet(eof, _DTS, _PTS, #stream{dts = DTS, codec = aac, es_buffer = Buffer} = Stream) ->
  case iolist_to_binary(lists:reverse(Buffer)) of
    <<>> -> Stream;
    Data -> new_pes_packet(Data, DTS, DTS, Stream#stream{es_buffer = []})
  end;

% new_pes_packet(<<>>, _, _, #stream{codec = aac} = Stream) ->
%   Stream;
new_pes_packet(<<>>, _DTS, _PTS, #stream{codec = aac, sample_rate = undefined} = Stream) ->
  Stream#stream{frames = []};

new_pes_packet(Data, DTS, PTS, #stream{codec = aac, sample_rate = undefined} = Stream) ->
  {AudioConfig, SampleRate} = aac_config(DTS, Data),
  Stream1 = #stream{frames = Frames} = new_pes_packet(Data, DTS, PTS, Stream#stream{sample_rate = SampleRate}),
  Stream1#stream{frames = [AudioConfig|Frames]};


new_pes_packet(Data, DTS, PTS, #stream{codec = aac, es_buffer = Old, dts = OldDTS, frames = Delayed} = Stream) when length(Old) > 0 ->
  ADTS = iolist_to_binary(lists:reverse([Data|Old])),
  case aac_frame(OldDTS, ADTS) of
    {ok, Frame, Rest} ->
      Stream1 = #stream{frames = Frames} = new_pes_packet(Rest, DTS, PTS, Stream#stream{es_buffer = [],frames = []}),
      Stream1#stream{frames = Delayed ++ [Frame|Frames]};
    {more, _} when size(ADTS) > 0 ->
      new_pes_packet(Data, DTS, PTS, Stream#stream{es_buffer = [ADTS]});
    {more, _} when size(ADTS) == 0 ->
      new_pes_packet(Data, DTS, PTS, Stream#stream{es_buffer = []});
    {error, _} ->
      new_pes_packet(Data, DTS, PTS, Stream#stream{es_buffer = []})
  end;

new_pes_packet(Data, DTS, PTS, #stream{codec = aac, sample_rate = SampleRate, frames = Delayed} = Stream) ->
  case aac_frame(DTS, Data) of
    {ok, Frame, Rest} ->
      Stream1 = #stream{frames = Frames} = new_pes_packet(Rest, DTS + 1024*90*1000 div SampleRate, PTS, Stream#stream{frames = []}),
      Stream1#stream{frames = Delayed ++ [Frame|Frames]};
    {more, _} when size(Data) > 0 ->
      Stream#stream{es_buffer = [Data], dts = DTS};
    {more, _} when size(Data) == 0 ->
      Stream#stream{es_buffer = [], dts = DTS};
    {error, _} ->
      Stream#stream{es_buffer = [], dts = DTS}
  end;

new_pes_packet(_, _, _, #stream{} = Stream) ->
  Stream#stream{es_buffer = []}.





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

separate_nals(NALs) ->
  lists:partition(fun(NAL) -> lists:member(h264:type(NAL), [sps,pps]) end, NALs).


h264_frames(#stream{dts = DTS, pts = PTS, es_buffer = AnnexB, sent_config = SentConfig, private = Private0} = Stream) ->
  NALs = [NAL || NAL <- binary:split(iolist_to_binary(lists:reverse(AnnexB)), [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>, h264:type(NAL) =/= delim],
  NalTypes = [h264:type(NAL) || NAL <- NALs],

  {ConfigNals, PayloadNals} = separate_nals(NALs),
  {Config, Private2} = case ConfigNals of
    [] -> {[], Private0};
    _ -> case h264:video_config(ConfigNals) of
      #video_frame{} = C ->
        {[C#video_frame{dts = DTS / 90, pts = DTS / 90, track_id = 1}], Private0};
      undefined ->
        Private1 = if
          Private0 == undefined -> [];
          is_list(Private0) -> Private0
        end,
        ConfigNals1 = ConfigNals ++ Private1,
        case h264:video_config(ConfigNals1) of
          #video_frame{body = Cfg} = C ->
            {_, ConfigNals2} = h264:unpack_config(Cfg),
            {[C#video_frame{dts = DTS / 90, pts = DTS / 90, track_id = 1}], ConfigNals2};
          undefined ->
            {[], ConfigNals1}
        end
    end
  end,
  Keyframe = lists:member(idr, NalTypes) orelse lists:member(pps, NalTypes),
  Payload = case PayloadNals of
    [] -> [];
    _ -> 
    % ?debugFmt("h264 fr ~B ~p", [DTS, [{h264:type(NAL), size(NAL)} || NAL <- NALs]]),
    [#video_frame{
    content = video,
    track_id= 1,
    body    = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- PayloadNals]),
    flavor  = if Keyframe -> keyframe; true -> frame end,
    codec   = h264,
    dts     = DTS / 90,
    pts     = PTS / 90
    }]
  end,
  Frames = case SentConfig of
    true -> [];
    false -> Config
  end ++ Payload,
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

