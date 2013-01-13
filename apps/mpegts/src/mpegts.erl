%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        MPEG TS stream module
%%% Links:
%%%  http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
%%%  http://en.wikipedia.org/wiki/MPEG-TS
%%%  http://en.wikipedia.org/wiki/Packetized_Elementary_Stream
%%%  http://en.wikipedia.org/wiki/Program_Specific_Information
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlang-mpegts.
%%% 
%%% erlang-mpegts is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-mpegts is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------

% 1. Take frame
% 2. If it is config, than save it
% 3. If there is audio/video frame in buffer, take buffered frame
% 4. put current frame into buffer
% 5. if buffered frame is audio, encode it to adts and save to second buffer
% 6. if audio buffer is overflowed or it is video frame, than pack it to PES packet
% 7. mux PES packet to MPEG-TS. If frame is marked as last one, than cut off last 16 bytes and normalize timestamp
% 
% 


-module(mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/h264.hrl").
-include("log.hrl").
-include("../include/mpegts.hrl").

-export([init/0, init/1, flush/1, encode/2]).

-export([read/2]).
-export([padder/0]).
-export([null/1]).


-define(TS_PACKET, 184). % 188 - 4 bytes of header
-define(PMT_PID, 4095).
-define(PID_OFFSET, 200). % add this to track_id for PID

  
read(URL, Options) ->
  Name = {proplists:get_value(name, Options), URL},
  Consumer = proplists:get_value(consumer,Options,self()),
  {ok, Reader} = mpegts_sup:start_reader(Name, [{url,URL},{consumer,Consumer}|Options]),
  case (catch gen_server:call(Reader, connect)) of
    ok -> {ok, Reader};
    {error, _} = Error -> Error;
    {'EXIT', Error} -> {error, Error}
  end.

-record(streamer, {
  counters = [],
  pad_counters = true,
  will_send_pat = true,
  media_info,
  pcr_pid,
  closing = false,
  resync_on_keyframe = false,
  last_dts,
  warning_count = 0
}).

-record(pes, {
  dts,
  keyframe,
  pid,
  last_frame,
  is_pcr,
  body
}).


-record(ts_psi, {
  pid,
  body
}).

-define(TS_HEADER(Start, Pid, Adaptation, HasPayload, Counter), 16#47, 0:1, Start:1, 0:1, Pid:13, 0:2, Adaptation:1, HasPayload:1, Counter:4).

-define(MAXPID, 4096).

init() -> init([]).

init(Options) -> 
  % Counters = hipe_bifs:bytearray(?MAXPID,0),
  Counters = [],
  ResyncOnKeyframe = proplists:get_value(resync_on_keyframe, Options, false),
  #streamer{pad_counters = proplists:get_value(pad_counters, Options, true), counters = Counters,
    resync_on_keyframe = ResyncOnKeyframe}.

-spec encode(#streamer{}, video_frame()) -> {#streamer{}, iolist()}.

encode(#streamer{} = Streamer, #video_frame{codec = empty}) ->
  {Streamer, <<>>};

encode(#streamer{} = Streamer, #media_info{} = MediaInfo) ->
  send_program_tables(save_media_info(Streamer, MediaInfo));

encode(#streamer{media_info = MediaInfo} = Streamer, #video_frame{flavor = config} = Frame) ->
  MI1 = video_frame:define_media_info(MediaInfo, Frame),
  MI2 = rewrite_track_ids(MI1),
  if MI2 =/= MediaInfo ->
    Streamer1 = save_media_info(Streamer, MI2),
    case Streamer1#streamer.will_send_pat of
      false ->
        send_program_tables(Streamer1#streamer{will_send_pat = true});
      true ->
        {Streamer1, <<>>}
    end;
  true ->
    {Streamer, <<>>}
  end;  
  
encode(#streamer{will_send_pat = WillSendPat, resync_on_keyframe = Resync} = Streamer, 
  #video_frame{dts = DTS, content = Content, flavor = Flavor} = Frame) 
  when Content == audio orelse Content == video ->
  % ?D({send_pat, SentPat, Flavor, SentPat andalso Flavor =/= keyframe, Frame#video_frame.dts}),
  {Streamer1, Tables} = send_program_tables(Streamer#streamer{will_send_pat = WillSendPat orelse (Resync andalso Flavor == keyframe)}), %  
  % StartTime = case get(start_time) of undefined -> put(start_time, erlang:now()), get(start_time); StartTime_ -> StartTime_ end,
  % StartDts = case get(start_dts) of undefined -> put(start_dts, DTS), get(start_dts); StartDts_ -> StartDts_ end,
  % RealDelta = timer:now_diff(erlang:now(), StartTime) div 1000,
  % StreamDelta = DTS - StartDts,
  % ?D({round(DTS), round(StreamDelta),round(RealDelta), round(RealDelta - StreamDelta)}),
  {Streamer2, TS} = encode_frame(Streamer1, Frame),
  % case empty_iolist(TS) of
  %   true when not WillSendPat -> %  (not SentPat) andalso Flavor =/= keyframe
  %     % ?D(rollback_pat),
  %     {Streamer3, _} = encode_frame(Streamer, Frame),
  %     {Streamer3, <<>>};
  %   true ->
  %     {Streamer2, <<>>};
  %   false ->
  %     {Streamer2#streamer{last_dts = DTS}, [Tables, TS]}
  % end;
  {Streamer2#streamer{last_dts = DTS}, [Tables, TS]};

encode(Streamer, #video_frame{} = _Frame) ->
  {Streamer, <<>>}.

rewrite_track_ids(MediaInfo) -> MediaInfo.


% empty_iolist(<<>>) -> true;
% empty_iolist([]) -> true;
% empty_iolist(Bin) when size(Bin) > 0 -> false;
% empty_iolist(Num) when is_number(Num) -> false;
% empty_iolist([Head|Tail]) -> empty_iolist(Head) andalso empty_iolist(Tail).

save_media_info(#streamer{} = Streamer, #media_info{streams = Streams} = MediaInfo) ->
  PcrPid = erlang:hd([TrackId || #stream_info{track_id = TrackId} <- Streams]),
  Streamer#streamer{media_info = MediaInfo, pcr_pid = PcrPid + ?PID_OFFSET}.

encode_frame(#streamer{media_info = #media_info{streams = Infos}, warning_count = WarningCount} = Streamer, 
             #video_frame{track_id = TrackId, next_id = NextId, content = Content} = Frame) when Content == audio orelse Content == video ->
  Closing = NextId == last_frame,
  case lists:keyfind(TrackId, #stream_info.track_id, Infos) of
    false ->
      % FIXME
      if WarningCount < 10 ->
        ?D({unknown_mpegts_track, TrackId, Infos, {video_frame, Frame#video_frame.track_id, Frame#video_frame.codec}, get(name)});
      true -> ok end, 
      {Streamer#streamer{warning_count = WarningCount}, <<>>};
    #stream_info{} = Info ->
      #pes{} = PES = pack_pes(Frame, Info),
      {Streamer1, TS} = mux(PES#pes{last_frame = Closing}, Streamer),
      {Streamer1, TS}
  end.


  
flush(#streamer{} = Streamer) ->
  % {Streamer1, Pad1} = pad_table_counters(Streamer),
  Streamer1 = Streamer,
  Pad1 = <<>>,
  % Video = [padding_ts(Counter, Streamer#streamer.video_codec, ?VIDEO_PID) || Counter <- lists:seq(Streamer#streamer.video_counter, 15)],
  % Audio = [padding_ts(Counter, Streamer#streamer.audio_codec, ?AUDIO_PID) || Counter <- lists:seq(Streamer#streamer.audio_counter, 15)],
  % {Streamer2, Video} = flush_video(Streamer1),
  % {Streamer3, Audio} = flush_video(Streamer2),
  Streamer3 = Streamer1,
  % Audio = Video = <<>>,
  {Streamer3#streamer{will_send_pat = true}, Pad1}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Stream configs %%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
% save_config(#streamer{} = Streamer, #video_frame{flavor = config, codec = h264, body = Config}) ->
%   {LengthSize, _} = h264:unpack_config(Config),
%   Streamer#streamer{video_codec = h264, video_config = {LengthSize, Config}};
%   
% save_config(#streamer{} = Streamer, #video_frame{flavor = config, codec = aac, body = AudioConfig}) ->
%   Streamer#streamer{audio_codec = aac, audio_config = aac:decode_config(AudioConfig)};
% 
% save_config(#streamer{audio_codec = undefined} = Streamer, #video_frame{content = audio, codec = Codec}) ->
%   Streamer#streamer{audio_codec = Codec};
% 
% save_config(#streamer{video_codec = undefined} = Streamer, #video_frame{content = video, codec = Codec}) ->
%   Streamer#streamer{video_codec = Codec};
% 
% save_config(Streamer, _) ->
%   Streamer.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% PES packing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamps(DTS, PTS) ->
  timestamps(DTS, PTS, undefined).

timestamps(DTS, PTS, Force) when DTS == PTS andalso Force =/= different ->
  PTS1 = round(PTS*90),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<PTS1:33>>,
  AddPesHeader = <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  {2#10, AddPesHeader};

timestamps(DTS, PTS, _Force) ->
  DTS1 = round(DTS*90),
  PTS1 = round(PTS*90),
  % ?D({"Video", PTS, DTS, "--", DTS1, PTS1}),
  % ?D({video, round(DTS), round(PTS), FrameType}),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<PTS1:33>>,
  <<Dts1:3, Dts2:15, Dts3:15>> = <<DTS1:33>>,
  AddPesHeader = <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
                   2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1>>,
  {2#11, AddPesHeader}.

pes_code(aac) -> ?MPEGTS_STREAMID_AAC;
pes_code(adts) -> ?MPEGTS_STREAMID_AAC;
pes_code(mp3) -> ?MPEGTS_STREAMID_MPGA1;
pes_code(mpeg2audio) -> ?MPEGTS_STREAMID_MPGA2;
pes_code(h264) -> ?MPEGTS_STREAMID_H264;
pes_code(mpeg2video) -> ?MPEGTS_STREAMID_MPEG2.


% padding_pes(h264) ->
%   PesHeader = <<2#10:2, 0:3, 1:1, 0:1, 1:1, 0, 0>>,
%   Body = <<1:32, 9, 16#F0>>,
%   #pes{body = [<<1:24, (pes_code(h264)), (size(PesHeader) + size(Body)):16, PesHeader/binary, Body/binary>>]};
% 
% padding_pes(Codec) ->
%   PesHeader = <<2#10:2, 0:14, 0>>,
%   #pes{body = [<<1:24, (pes_code(Codec)), (size(PesHeader)):16>>, PesHeader]}.

pack_pes(#video_frame{} = Frame, Info) ->
  pack_pes([Frame], Info);

pack_pes([#video_frame{content = Content}|_] = Frames, #stream_info{} = Info) ->
  case Content of
    video -> pack_video_pes(Frames, Info);
    audio -> pack_audio_pes(Frames, Info)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Audio encoding %%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_audio_pes([#video_frame{codec = Codec, dts = DTS}|_] = Audio, #stream_info{params = AudioParams, track_id = TrackId}) ->
  Marker = 2#10,
  Scrambling = Priority = Copyright = 0,
  Alignment = Original = 1,
  {PtsDts, AddPesHeader} = timestamps(DTS, DTS),
  PesHeader = <<Marker:2, Scrambling:2, Priority:1,
                Alignment:1, Copyright:1, Original:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  Packed = [case Codec of
    aac -> #audio_params{config = AudioConfig} = AudioParams, aac:pack_adts(Body, AudioConfig);
    adts -> Body;
    mpeg2audio -> Body;
    mp3 -> Body
  end || #video_frame{body = Body} <- Audio],
  PES = [<<1:24, (pes_code(Codec)), (size(PesHeader) + iolist_size(Packed)):16>>, PesHeader, Packed],
  % ?D({send_aac, length(Audio), Codec, iolist_size(PES)}),
  #pes{pid = TrackId + ?PID_OFFSET, dts = DTS, body = PES, keyframe = false}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Video encoding %%%%%%%%%%%%%%%%%%%%%%%%%%%


pack_video_pes([#video_frame{codec = Codec, dts = DTS, pts = PTS, flavor = Flavor, body = FrameBody}], #stream_info{track_id = TrackId} = Info) ->
  Marker = 2#10,
  Alignment = 1,
  Scrambling = Priority = Copyright = Original = 0,
  ESCR = ESRate = DSMTrick = CopyInfo = CRC = Extension = 0,
  
  {PtsDts, AddPesHeader} = timestamps(DTS, PTS, different),
  PesHeader = <<Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1, Original:1, PtsDts:2, 
                ESCR:1,ESRate:1,DSMTrick:1,CopyInfo:1,CRC:1,Extension:1,  % All these bits are usually zero
                (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Sending nal", Body}),
  % NALHeader = <<1:32>>,
  % PES = <<1:24, ?MPEGTS_STREAMID_H264, (size(PesHeader) + size(Body) + size(NALHeader) + 1):16, PesHeader/binary, NALHeader/binary, Body/binary, 0>>,
  % no PES size should be provided for video
  Body = case Codec of
    h264 -> annexb_nalu(FrameBody, Flavor, Info);
    _ -> FrameBody
  end,
  
  % Add proper NAL packing
  
  #pes{pid = TrackId + ?PID_OFFSET, body = [<<1:24, (pes_code(Codec)), 0:16>>, PesHeader, Body], dts = DTS, keyframe = Flavor == keyframe}.


unpack_nals(Bin, 4) ->
  [NAL || <<Len:32, NAL:Len/binary>> <= Bin];

unpack_nals(Bin, 2) ->
  [NAL || <<Len:16, NAL:Len/binary>> <= Bin].

annexb_nalu(Body, Flavor, #stream_info{params = #video_params{length_size = LengthSize, nals = CfgNALS}} = _Info) ->
  if length(CfgNALS) == 0 ->
    ?D({warning, mpegts_cfg_nals_missing});
  true -> ok
  end,
  NALS1 = unpack_nals(Body, LengthSize),
  NalTypes = [T || <<_:3, T:5, _/binary>> <- NALS1],
  NALS2 = case Flavor of
    keyframe ->
      case lists:member(?NAL_SPS, NalTypes) of
        true -> NALS1;
        false ->
          CfgNALS ++ NALS1
      end;
    _ ->
      NALS1
  end,
  NALS3 = case lists:member(?NAL_DELIM, NalTypes) of
    true -> NALS2;
    false -> [<<9, 16#F0>>|NALS2]
  end,
  Out = [[<<1:32>>, NAL] || NAL <- NALS3],
  % Out = [<<1:32, 9, 16#F0>>, ConfigNALS, [[<<1:24>>, NAL] || NAL <- BodyNALS]], % Need to add AU (9 NAL) for HLS
  % ?D({got, NalTypes, send, [T || <<_:3, T:5, _/binary>> <- mpegts_reader:read_annexb(iolist_to_binary(Out))]}),
  Out.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Packing  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


send_program_tables(#streamer{will_send_pat = true} = Streamer) -> %  andalso A =/= undefined andalso V =/= undefined
  {Streamer1, PATBin} = send_pat(Streamer),
  {Streamer2, PMTBin} = send_pmt(Streamer1),
  {Streamer2#streamer{will_send_pat = false}, [PATBin, PMTBin]};

send_program_tables(Streamer) ->
  {Streamer, <<>>}.



% pad_table_counters(#streamer{} = Streamer) ->
%   pad_pat_counter(Streamer, []).
% 
% 
% pad_pat_counter(#streamer{} = Streamer, Acc) ->
%   case counter(Streamer, ?PAT_PID) of
%     0 -> pad_pmt_counter(Streamer, Acc);
%     _C ->
%       {Streamer1, Bin} = send_pat(Streamer),
%       pad_pat_counter(Streamer1, [Bin|Acc])
%   end.
% 
% pad_pmt_counter(#streamer{} = Streamer, Acc) ->
%   case counter(Streamer, ?PMT_PID) of
%     0 ->
%       {Streamer, lists:reverse(Acc)};
%     _C ->
%       {Streamer1, Bin} = send_pmt(Streamer),
%       pad_pmt_counter(Streamer1, [Bin|Acc])
%   end.


send_pat(Streamer) ->
  PAT = mpegts_psi:encode(pat, [{programs, [{1, ?PMT_PID}]}]),
  mux(#ts_psi{pid = ?PAT_PID, body = PAT}, Streamer).

send_pmt(#streamer{media_info = #media_info{streams = Streams}, pcr_pid = PcrPid} = Streamer) ->
  Programs = [{Codec, TrackId + ?PID_OFFSET, [{lang,Language},{config,Config}]} || #stream_info{codec = Codec, track_id = TrackId, language = Language, config = Config} <- Streams],
  PMT = mpegts_psi:encode(pmt, [{program,1},{pcr_pid,PcrPid},{streams, Programs}]),
  mux(#ts_psi{pid = ?PMT_PID, body = PMT}, Streamer).


counter(#streamer{counters = Counters} = _Streamer, Pid) when Pid >= 0 andalso Pid < ?MAXPID ->
  % hipe_bifs:bytearray_sub(Counters, Pid).
  proplists:get_value(Pid, Counters, 0).

increment_counter(#streamer{} = Streamer, Pid) ->
  increment_counter(Streamer, Pid, 1).

increment_counter(#streamer{counters = Counters} = Streamer, Pid, N) when Pid >= 0 andalso Pid < ?MAXPID ->
  Counter = counter(Streamer, Pid),
  % hipe_bifs:bytearray_update(Counters, Pid, (Counter + N) rem 16),
  % {Counter, Streamer}.
  {Counter, Streamer#streamer{counters = lists:keystore(Pid, 1, Counters, {Pid, (Counter + N) rem 16})}}.
  
ts_header(Start, Pid, Adaptation, HasPayload, Counter) ->
  Scrambling = Priority = TEI = 0,
  <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adaptation:1, HasPayload:1, Counter:4>>.

% mux(Input, Streamer) ->
%   {_Streamer1, _Out1} = mux0(Input, Streamer).
%   % Output = iolist_to_binary(Out1),
%   % Packets1 = [Bin || <<Bin:188/binary>> <= Output],
%   % Packets2 = [Bin || <<16#47, _/binary>> = Bin <- Packets1],
%   % iolist_to_binary(Packets2) == Output orelse ?D({invalid_mux, iolist_size(Input#pes.body), Input}),
%   % {Streamer1, Packets2}.

mux(#ts_psi{pid = Pid, body = Data}, Streamer) ->
  {Counter, Streamer1} = increment_counter(Streamer, Pid),
  {Streamer1, [ts_header(1, Pid, 0, 1, Counter), padding(Data, 184)]};

mux(#pes{body = undefined}, Streamer) ->
  {Streamer, <<>>};

mux(#pes{pid = Pid, body = Body, last_frame = LastFrame} = PES, #streamer{pcr_pid = PcrPid} = Streamer) ->
  {Counter, Streamer1} = increment_counter(Streamer, Pid),
  
  % Adaptation field may be just zero padder, for this we have zero_adaptation
  % or it may be only 1 byte for non-pcr stream
  % or it may be 1+6 bytes for pcr stream
  % and don't forget about 1 byte for adaptation length
  
  Data = iolist_to_binary(Body),
  IsPCR = Pid == PcrPid,
  AdaptationLength = case IsPCR of
    true -> 1 + 1 + 6;
    _ -> 1 + 1
  end,
  MinDataLength = lists:min([?TS_PACKET - AdaptationLength, size(Data) - 16 - 1]),
  BodyLen = size(Data) - MinDataLength - 16,
  % ?D([{pid,Pid},{pes,round(PES#pes.dts*90)},{pcr,IsPCR},{last_frame,LastFrame},{min_data,MinDataLength},{body,BodyLen},{data,size(Data)}]),
  case Data of
    % This code can create padding segments in the end of file. It is temporarily disabled
    <<FirstData:MinDataLength/binary, Rest:BodyLen/binary, End:16/binary>> when LastFrame == true ->
      Acc = [ts_header(1, Pid, 1, 1, Counter), adaptation_field(PES#pes{body = FirstData, is_pcr = IsPCR}), FirstData],
      Parts = mux_parts(Rest, (Counter + 1) rem 16, Pid, LastFrame),
      {NewCounter1, Streamer2} = increment_counter(Streamer1, Pid, length(Parts)),
      NewCounter2 = (NewCounter1 + length(Parts)) rem 16,
      % ?D({counter, Pid, Counter, length(Parts), iolist_size(Parts) div 188, NewCounter}),
      Padding = pad_counters(End, Pid, NewCounter2),
      % ?D({pad, NewCounter, iolist_size(Padding) div 188}),
      {Streamer2, [Acc, Parts, Padding]};
    <<FirstData:MinDataLength/binary, Rest/binary>> -> % when LastFrame == false 
      Acc = [ts_header(1, Pid, 1, 1, Counter), adaptation_field(PES#pes{body = FirstData, is_pcr = IsPCR}), FirstData],
      Parts = mux_parts(Rest, (Counter + 1) rem 16, Pid, LastFrame),
      {_, Streamer2} = increment_counter(Streamer1, Pid, length(Parts)),
      {Streamer2, [Acc, Parts]};
    _ when size(Data) =< MinDataLength + 16 ->
      Acc = [ts_header(1, Pid, 1, 1, Counter), adaptation_field(PES#pes{is_pcr = IsPCR}), Data],
      if LastFrame == true -> ?D({have_not_padded_counter, Pid, size(Data), MinDataLength, BodyLen}); true -> ok end,
      {Streamer1, Acc}
  end.
  
mux_parts(<<Part:?TS_PACKET/binary, Rest/binary>>, Counter, Pid, LastFrame) ->
  [[<<?TS_HEADER(0, Pid, 0, 1, Counter)>>, Part] | mux_parts(Rest, (Counter + 1) rem 16, Pid, LastFrame)];

% Very special case, when it is impossible to add adaptation field to last PES part
% if data is 183 bytes left, than it will be impossible to add only one byte of adaptation field
mux_parts(<<Part:167/binary, Rest:16/binary>>, Counter, Pid, LastFrame) ->
  [[ts_header(0, Pid, 1, 1, Counter), zero_adaptation(Part), Part] | mux_parts(Rest, (Counter + 1) rem 16, Pid, LastFrame)];
  
mux_parts(Part, Counter, Pid, _LastFrame) when size(Part) =< ?TS_PACKET - 2 ->
  [[ts_header(0, Pid, 1, 1, Counter), zero_adaptation(Part), Part]].

zero_adaptation(Data) ->
  % iolist_size(Data) < ?TS_PACKET orelse erlang:error(too_big_for_zero_padding),
  Field = padding(<<0>>, ?TS_PACKET - iolist_size(Data) - 1),
  [<<(iolist_size(Field))>>, Field].

adaptation_field(#pes{body = Data, dts = Timestamp, keyframe = _Keyframe, is_pcr = IsPCR, pid = _Pid}) ->
  % RandomAccess = case Keyframe of
  %   true -> 1;
  %   _ -> 0
  % end,
  RandomAccess = 0,
  {HasPCR, PCR} = case IsPCR of
    true ->
      FullPCR = round(Timestamp * 27000),
      PCR1 = FullPCR div 300,
      PCR2 = FullPCR rem 300,
      PCRBin = <<PCR1:33, 2#111111:6, PCR2:9>>,
      % ?D({pcr,_Pid,Timestamp, PCR1}),
      {1, PCRBin};
    _ ->
      % ?D({dts,Timestamp}),
      {0, <<>>}
  end,
  HasOPCR = Splice = Private = Ext = Priority = Discontinuity = 0,

  Adaptation = <<Discontinuity:1, RandomAccess:1, Priority:1, HasPCR:1, HasOPCR:1, Splice:1, Private:1, Ext:1, PCR/bitstring>>,
  
  Field = padding(Adaptation, ?TS_PACKET - 1 - iolist_size(Data)),
  [<<(iolist_size(Field))>>, Field].


null(Counter1) ->
  Counter = Counter1 rem 16,
  <<Padding:182/binary, _/binary>> = padder(),
  [<<16#47, 0:3, 16#1FFF:13, 0:2, 1:1, 0:1, Counter:4, 182, 0>>, Padding].


  
padder() ->
  <<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255>>.
  

padding(Padding, Size) when size(Padding) >= Size -> Padding;
padding(Padding, Size) when size(Padding) < Size ->
  PadSize = Size - size(Padding),
  <<Pad:PadSize/binary, _/binary>> = padder(),
  [Padding, Pad].
  
pad_counters(Bin, Pid, Counter) ->
  pad_counters(Bin, Pid, Counter, []).
  
pad_counters(<<>>, _Pid, _Counter, Acc) ->
  lists:reverse(Acc);
  
pad_counters(Bin, Pid, 15, Acc) ->
  pad_counters(<<>>, Pid, 15, [pad_ts(Bin, Pid, 15)|Acc]);

pad_counters(<<Byte:1/binary, Bin/binary>>, Pid, Counter, Acc) ->
  pad_counters(Bin, Pid, (Counter + 1) rem 16, [pad_ts(Byte, Pid, Counter)|Acc]).

pad_ts(Bin, Pid, Counter) ->
  [ts_header(0, Pid, 1, 1, Counter), zero_adaptation(Bin), Bin].




-define(END_COUNTER, 15).



  
  
  
  
  
  
  