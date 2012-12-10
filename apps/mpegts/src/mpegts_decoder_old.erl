-module(mpegts_decoder_old).

-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([read_file/1, decode_file/1]).



% -define(DD(Fmt,X), io:format("ts_reader:~p "++Fmt++"~n", [?LINE|X])).
-define(DD(Fmt,X), ?debugFmt(Fmt, X)).
% -define(DD(Fmt,X), ok).


-define(PAT_PID, 0).


-spec read_file(string()) -> {ok, [any()]}.
read_file(Path) ->
  {ok, Bin} = file:read_file(Path),
  decode_file(Bin).

-spec decode_file(binary()) -> {ok, [any()]}.



decode_file(<<16#47,_:187/binary,16#47,_:187/binary,16#47,_/binary>> = Bin) ->
  {pat, [{PMT,_}]} = find_pat(Bin),
  {pmt, Streams} = find_pmt(Bin, PMT),

  Frames1 = lists:foldl(fun({Pid,Codec}, Acc) when Codec == h264 orelse Codec == aac ->
    TSPackets1 = select_pid(Bin, Pid),
    TSPackets2 = rewind_to_begin(TSPackets1),
    PESPackets = collect_pes_packets(TSPackets2),
    case Codec of
      h264 -> unpack_pes_h264(PESPackets, Pid);
      aac  -> unpack_pes_aac(PESPackets, Pid)
    end ++ Acc;
  (_, Acc) ->
    Acc
  end, [], Streams),

  Frames2 = video_frame:sort(Frames1),


  % [?debugFmt("~4s ~8s ~B ~B", [Codec, Flavor, round(DTS), size(Body)]) || 
  %   #video_frame{codec = Codec, flavor = Flavor, dts = DTS, body = Body} <-
  %   video_frame:sort(Frames2)],

  {ok, Frames2}.





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




find_pat(<<16#47, _:3, ?PAT_PID:13, _/binary>> = Packet) ->
  {pat, _} = PAT = mpegts_psi:psi(ts_payload(Packet)),
  PAT;
find_pat(<<16#47, _:187/binary, Rest/binary>>) -> find_pat(Rest);
find_pat(<<>>) -> error(no_pat_found_in_mpegts).



find_pmt(<<16#47, _:3, PMT:13, _/binary>> = Packet, PMT) ->
  {pmt, Descriptors} = mpegts_psi:psi(ts_payload(Packet)),
  % io:format("pid ~B: PMT with streams: ", [PMT]),
  % [io:format("~s on ~B; ", [Codec, Pid]) || #pmt_entry{codec = Codec, pid = Pid} <- Descriptors],
  % io:format("~n"),
  {pmt, [{Pid,Codec} || #pmt_entry{pid = Pid, codec = Codec} <- Descriptors]};

find_pmt(<<16#47, _:187/binary, Rest/binary>>, PMT) -> find_pmt(Rest, PMT);
find_pmt(<<>>, _) -> error(no_pmt_found_in_mpegts).



select_pid(<<16#47, _:1, Start:1, _:1, Pid:13, _:185/binary, Rest/binary>> = Packet, Pid) ->
  [{Start,ts_payload(Packet)}|select_pid(Rest, Pid)];

select_pid(<<16#47, _:187/binary, Rest/binary>>, Pid) -> select_pid(Rest, Pid);
select_pid(<<>>, _) -> [].




rewind_to_begin([{0,_}|Packets]) -> rewind_to_begin(Packets);
rewind_to_begin([{1,_}|_] = Packets) -> Packets.


collect_pes_packets([{1,TS}|Packets]) ->
  {TSList1, Rest} = choose_ts_till_start(Packets),
  PES = iolist_to_binary([TS|TSList1]),
  <<1:24, _StreamId, PESLength:16, _:8, PtsDts:2, _:6, HeaderLength, PESHeader:HeaderLength/binary, Data/binary>> = PES,
  case PESLength of
    0 -> ok;
    _ -> PESLength = size(PES) - 6
  end,
  {DTS, PTS} = pes_timestamp(PtsDts, PESHeader),
  [{DTS,PTS,Data}|collect_pes_packets(Rest)];

collect_pes_packets([]) ->
  [].


choose_ts_till_start([{0,TS}|Packets]) ->
  {TSList, Rest} = choose_ts_till_start(Packets),
  {[TS|TSList], Rest};

choose_ts_till_start([{1,_}|_] = Packets) ->  
  {[], Packets};

choose_ts_till_start([]) ->
  {[], []}.




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





unpack_pes_h264(PESPackets, Pid) -> 
  unpack_pes_h264(normalize_h264(PESPackets), Pid, false, []).

normalize_h264([Packet]) ->
  [Packet];

normalize_h264([Packet|NextPackets = [{_,_,<<1:32,_/binary>>}|_]]) ->
  [Packet|normalize_h264(NextPackets)];

normalize_h264([Packet|NextPackets = [{_,_,<<1:24,_/binary>>}|_]]) ->
  [Packet|normalize_h264(NextPackets)];

normalize_h264([{DTS,PTS,Pes}, {DTS1,PTS1,NextPes}|NextPackets]) ->
  case binary:split(NextPes, [<<1:32>>, <<1:24>>]) of
    [Append,_] ->
      Len = size(Append),
      <<Append:Len/binary, Rest/binary>> = NextPes,
      [{DTS,PTS,<<Pes/binary, Append/binary>>}|normalize_h264([{DTS1,PTS1,Rest}|NextPackets])];
    [NextPes] ->
      normalize_h264([{DTS,PTS,<<Pes/binary, NextPes/binary>>}|NextPackets])
  end;

normalize_h264([]) -> [].



unpack_pes_h264([{DTS,PTS,Pes}|Packets], Pid, SeenConfig, Acc) ->
  NALs = [NAL || NAL <- binary:split(Pes, [<<1:32>>, <<1:24>>], [global]), NAL =/= <<>>, h264:type(NAL) =/= delim],

  ConfigNals = [NAL || NAL <- NALs, lists:member(h264:type(NAL), [sps, pps])],
  
  Keyframe = length(ConfigNals) > 0 orelse length([NAL || NAL <- NALs, h264:type(NAL) == idr]) > 0, 
  
  ConfigFrame = case ConfigNals of
    [] -> [];
    _ when SeenConfig -> [];
    _ ->
      H264 = lists:foldl(fun(NAL, H264_) -> h264:parse_nal(NAL, H264_) end, h264:init(), ConfigNals),
      [(h264:video_config(H264))#video_frame{dts = DTS, pts = DTS, track_id = Pid}]
  end,

  BodyNals = [NAL || NAL <- NALs, not lists:member(h264:type(NAL), [sps, pps])],
  Body = [[<<(size(NAL)):32>>, NAL] || NAL <- BodyNals],

  Frame = #video_frame{
    content = video,
    track_id= Pid,
    body    = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- NALs, not lists:member(h264:type(NAL), [sps, pps])]),
    flavor  = if Keyframe -> keyframe; true -> frame end,
    codec   = h264,
    dts     = DTS,
    pts     = PTS
  },
  
  unpack_pes_h264(Packets, Pid, SeenConfig orelse length(ConfigFrame) > 0, ConfigFrame ++ [Frame|Acc]);

unpack_pes_h264([], _, _, Acc) ->
  Acc.



unpack_pes_aac([{DTS,PTS,Pes}|_] = Packets, Pid) -> 
  Config = aac:adts_to_config(Pes),
  #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
  AudioConfig = #video_frame{
    content = audio,
    flavor  = config,
    track_id= Pid,
    dts     = DTS,
    pts     = PTS,
    body    = Config,
    codec   = aac
  },

  [AudioConfig|unpack_pes_adts(Packets, 1024*1000 / SampleRate, Pid)].

unpack_pes_adts([{DTS,PTS,Pes}|Packets], Shift, Pid) ->
  case aac:unpack_adts(Pes) of
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
      [Frame|unpack_pes_adts([{DTS+Shift,PTS+Shift,Rest}|Packets], Shift, Pid)];
    {more, _NeedBytes} when length(Packets) > 0 ->
      [{DTS1,PTS1,NextPes}|NextPackets] = Packets,
      % ?debugFmt("ADTS lag: ~B, ~B, ~B, ~B ~p", [round(DTS),size(Pes), round(DTS1), NeedBytes, element(1,split_binary(Rest, 9) )]),
      unpack_pes_adts([{DTS1,PTS1,<<Pes/binary,NextPes/binary>>}|NextPackets], Shift, Pid);
    {more, _} ->
      [];
    {error, Bin} ->
      error({desync_adts,Bin})
  end;

unpack_pes_adts([], _, _) ->
  [].







