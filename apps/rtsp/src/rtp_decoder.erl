%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        RTP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/ertp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtp.
%%%
%%% erlang-rtp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtp_decoder).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("sdp.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rtp.hrl").

-define(RTCP_SR, 200).
-define(RTCP_RR, 201).
-define(RTCP_SD, 202).
-define(YEARS_70, 2208988800).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.
-define(YEARS_100, 3155673600).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.





-record(h264_buffer, {
  time,
  buffer
}).

-export([init/1, decode/2, sync/2]).

init(#stream_info{codec = Codec, timescale = Scale} = Stream) ->
  #rtp_channel{codec = Codec, stream_info = Stream, timescale = Scale}.

sync(#rtp_channel{} = RTP, Headers) ->
  Seq = proplists:get_value(seq, Headers),
  Time = proplists:get_value(rtptime, Headers),
  % ?D({sync, Headers}),
  RTP#rtp_channel{wall_clock = 0, timecode = Time, sequence = Seq}.

decode(_, #rtp_channel{timecode = TC, wall_clock = Clock} = RTP) when TC == undefined orelse Clock == undefined ->
  %% ?D({unsynced, RTP}),
  {ok, RTP#rtp_channel{timecode = 0, wall_clock = 0, sequence = 0}, []};

decode(<<_:16, Sequence:16, _/binary>> = Data, #rtp_channel{sequence = undefined} = RTP) ->
  decode(Data, RTP#rtp_channel{sequence = Sequence});

decode(<<_:16, OldSeq:16, _/binary>> = Data, #rtp_channel{sequence = Sequence, warning_count = WarningCount} = RTP) when OldSeq < Sequence ->
  if WarningCount < 10 -> ?D({drop_sequence, OldSeq, Sequence});
  true -> ok end,
  decode(Data, RTP#rtp_channel{sequence = undefined, warning_count = WarningCount + 1});
  % {ok, RTP, []};

decode(<<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>, #rtp_channel{} = RTP) ->
  decode(Data, RTP#rtp_channel{sequence = (Sequence + 1) rem 65536}, Timecode);

decode(<<2:2, 1:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, BigData/binary>>, #rtp_channel{} = RTP) ->
  SizeOffset = size(BigData) - 1,
  <<_:SizeOffset/binary, PaddingSize>> = BigData,
  DataLen = size(BigData) - PaddingSize,
  <<Data:DataLen/binary, _:PaddingSize/binary>> = BigData,
  decode(Data, RTP#rtp_channel{sequence = (Sequence + 1) rem 65536}, Timecode).


decode(<<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>>, #rtp_channel{codec = aac, 
  stream_info = #stream_info{track_id = TrackId}} = RTP, Timecode) ->
  Frames = decode_aac(AudioData, AUHeaders, RTP, Timecode),
  {ok, RTP, [F#video_frame{track_id = TrackId} || F <- Frames]};

decode(Body, #rtp_channel{codec = h264, buffer = Buffer, stream_info = #stream_info{track_id = TrackId}} = RTP, Timecode) ->
  DTS = timecode_to_dts(RTP, Timecode),
  {ok, Buffer1, Frames} = decode_h264(Body, Buffer, DTS),
  % ?D({decode,h264,Timecode,DTS, length(Frames), size(Body), size(Buffer1#h264_buffer.buffer)}),
  {ok, RTP#rtp_channel{buffer = Buffer1}, [F#video_frame{track_id = TrackId} || F <- Frames]};

decode(Body, #rtp_channel{codec = mpegts, buffer = undefined} = RTP, Timecode) ->
  {ok, Decoder} = mpegts_decoder:init(),
  decode(Body, RTP#rtp_channel{buffer = Decoder}, Timecode);

decode(Body, #rtp_channel{codec = mpegts, buffer = Decoder} = RTP, _Timecode) ->
  {ok, Decoder1, Frames} = mpegts_decoder:decode(Body, Decoder),
  {ok, RTP#rtp_channel{buffer = Decoder1}, Frames};

decode(Body, #rtp_channel{stream_info = #stream_info{codec = Codec, content = Content, track_id = TrackId}} = RTP, Timecode) ->
  DTS = timecode_to_dts(RTP, Timecode),
  Frame = #video_frame{
    content = Content,
    dts     = DTS,
    pts     = DTS,
    body    = Body,
	  codec	  = Codec,
	  flavor  = frame,
    track_id = TrackId
  },
  {ok, RTP, [Frame]}.



% FIXME:
% Тут надо по-другому. 
% Надо аккумулировать все RTP-пейлоады с одним Timecode
% Потом проходить по ним депакетизатором FUA. Отрефакторить это в h264 в depacketize
% Потом взять результирующие NAL-юниты, склеить их в один или два кадра.
% SPS, PPS оформить в конфиг, остальное положить в один кадр. Бевард зачем-то шлет два полукадра.
%
% decode_h264(Body, OldDts, OldDts) -> accumulate
% decode_h264(NewBody, OldDts, NewDts) ->
%   depacketize(Accum)
%   split_into_frames(NALS)
%   flush_buffer
%   accumulate(Body, NewDts)
%   return_frames_and_new_buffer
%

decode_h264(Body, undefined, DTS) ->
  {ok, #h264_buffer{time = DTS, buffer = [Body]}, []};

decode_h264(Body, #h264_buffer{time = DTS, buffer = Buffer} = H264, DTS) ->
  {ok, H264#h264_buffer{buffer = [Body|Buffer]}, []};

decode_h264(Body, #h264_buffer{time = OldDTS, buffer = Buffer}, DTS) when OldDTS =/= DTS ->
  {ok, #h264_buffer{time = DTS, buffer = [Body]}, h264:unpack_rtp_list(lists:reverse(Buffer), OldDTS)}.



decode_aac(<<>>, <<>>, _RTP, _) ->
  [];

decode_aac(AudioData, <<AUSize:13, _Delta:3, AUHeaders/bitstring>>, RTP, Timecode) ->
  <<Body:AUSize/binary, Rest/binary>> = AudioData,
  DTS = timecode_to_dts(RTP, Timecode),
  Frame = #video_frame{
    content = audio,
    dts     = DTS,
    pts     = DTS,
    body    = Body,
	  codec	  = aac,
	  flavor  = frame
  },
  [Frame|decode_aac(Rest, AUHeaders, RTP, Timecode + 1024)].

timecode_to_dts(#rtp_channel{timescale = Scale, timecode = BaseTimecode, wall_clock = WallClock}, Timecode) ->
  % ?D({tdts, WallClock, BaseTimecode, Scale, WallClock + (Timecode - BaseTimecode)/Scale, Timecode}),
  WallClock + (Timecode - BaseTimecode)/Scale.










