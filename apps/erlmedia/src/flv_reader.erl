%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        FLV reader for erlyvideo
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(flv_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/flv.hrl").
-include("../include/aac.hrl").
-include("../include/mp3.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([init/2, read_frame/2, read_gop/3, media_info/1, keyframes/1, keyframes/2]).

-record(flv_media, {
  reader,
  media_info,
  keyframes,
  header,
  metadata = [],
  bitrate,
  sample_rate,
  samples
}).





%%--------------------------------------------------------------------
%% @spec ({IoModule::atom(), IoDev::iodev()}, Options) -> {ok, State} | {error,Reason::atom()}
%% @doc Read flv file and load its frames in memory ETS
%% @end 
%%--------------------------------------------------------------------
init({_Module,_Device} = Reader, Options) ->
  Media1 = #flv_media{reader = Reader},
  ReadMeta = proplists:get_value(find_metadata, Options, true),
  case flv:read_header(Reader) of
    {#flv_header{} = Header, Offset} when ReadMeta -> 
      Media2 = init_media_info(Media1#flv_media{header = Header}, Offset),
      {ok, Media2};
    {#flv_header{} = Header, _} ->
      {ok, Media1#flv_media{header = Header}};
    eof -> 
      {error, unexpected_eof};
    {error, Reason} -> {error, Reason}           
  end.



media_info(#flv_media{media_info = MediaInfo}) -> MediaInfo.

init_media_info(#flv_media{reader = Reader} = Media, Offset) ->
  FirstFrames = read_frame_list(Reader, Offset, 10),
  MediaInfo1 = #media_info{streams = Streams1} = lists:foldl(fun(Frame, MI) ->
    video_frame:define_media_info(MI, Frame)
  end, #media_info{flow_type = file}, FirstFrames),

  MediaInfo = MediaInfo1#media_info{streams = lists:keysort(#stream_info.track_id, Streams1)},
    
  Media1 = Media#flv_media{media_info = MediaInfo},
  Media2 = case [Params || #stream_info{content = audio, params = #audio_params{} = Params} <- MediaInfo#media_info.streams] of
    [#audio_params{sample_rate = SampleRate, samples = Samples}|_] ->
      Media1#flv_media{sample_rate = SampleRate, samples = Samples};
    [] ->
      Media1
  end,  
  case [M || #video_frame{content = metadata, body = M} <- FirstFrames] of
    [Meta|_] ->
      parse_metadata(Meta, Media2);
    _ ->
      Media2
  end.  


read_frame_list(_Reader, _Offset, 0) -> [];

read_frame_list(Reader, Offset, Count) ->
  case flv:read_frame(Reader, Offset) of
    #video_frame{next_id = Next} = Frame ->
      [Frame|read_frame_list(Reader, Next, Count - 1)];
    eof ->
      []
  end.




b_to_atom(A) when is_atom(A) -> A;
b_to_atom(A) when is_binary(A) -> binary_to_atom(A, latin1).

parse_metadata([<<"onMetaData">>, {object, Meta}], #flv_media{} = Media) ->
  parse_metadata([<<"onMetaData">>, Meta], Media);

parse_metadata([<<"onMetaData">>, Meta], #flv_media{media_info = MI1} = Media) ->
  Meta2 = [{b_to_atom(K),V} || {K,V} <- Meta, K =/= <<"duration">> andalso K =/= <<"keyframes">> andalso K =/= <<"times">>],

  Duration = case proplists:get_value(<<"duration">>, Meta) of
    undefined -> undefined;
    D_ -> round(D_*1000)
  end,
  
  
  Media1 = Media#flv_media{
    bitrate = case proplists:get_value(<<"videodatarate">>, Meta) of
      undefined -> undefined;
      BR -> round(BR*1000)
    end,
    metadata = [{duration,Duration}|Meta2],
    media_info = MI1#media_info{duration = Duration}
  },
  case proplists:get_value(<<"keyframes">>, Meta) of
    {object, KF} ->
      Offsets = proplists:get_value(filepositions, KF),
      Times = proplists:get_value(times, KF),
      Keyframes = lists:zip([round(T*1000) || T <- Times], [round(O) || O <- Offsets]),
      Media1#flv_media{keyframes = Keyframes};
    _ -> 
      Media1
  end;

parse_metadata(Meta, #flv_media{} = Media) ->
  ?D({"Unknown metadata", Meta}),
  Media.


keyframes(#flv_media{keyframes = Keyframes}) -> Keyframes.
keyframes(Media, _) -> keyframes(Media).



read_gop(#flv_media{reader = {Module, Device}, keyframes = Keyframes} = Media, N, _Tracks) ->
  {Offset, Size} = case lists:nthtail(N-1, Keyframes) of
    [{_,Start_},{_,End_}|_] -> {Start_, End_ - Start_};
    [{_,Start_}] -> {Start_, 10000000}
  end,
  {ok, FLV} = Module:pread(Device, Offset, Size),
  Frames = [normalize_audio_dts(Media, F) || F <- flv:read_all_frames(FLV)],
  {ok, Frames}.























  
% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type

read_frame(#flv_media{media_info = MediaInfo} = Media, {ConfigType, Pos, DTS}) when ConfigType == audio orelse ConfigType == video ->
  Configs = video_frame:config_frames(MediaInfo),
  Next = case ConfigType of
    video -> Pos;
    audio -> {video, Pos, DTS}
  end,
  case [Frame || #video_frame{content = Content} = Frame <- Configs, Content == ConfigType] of
    [Frame] ->
      Frame#video_frame{next_id = Next, dts = DTS, pts = DTS};
    [] when ConfigType == audio ->
      read_frame(Media, {video, Pos, DTS});
    [] when ConfigType == video ->
      read_frame(Media, Pos)
  end;


read_frame(_, eof) ->
  eof;

read_frame(#flv_media{reader = Reader} = Media, Offset) ->
  normalize_audio_dts(Media, flv:read_frame(Reader, Offset)).

normalize_audio_dts(#flv_media{samples = Samples, sample_rate = SampleRate}, #video_frame{codec = mp3, dts = DTS} = Frame) ->
  Count = (DTS*SampleRate)/(Samples*1000),
  PureDTS = round(Count)*Samples*1000 / SampleRate,
  % ?D({mp3,Count,DTS, PureDTS}),
  Frame#video_frame{dts = PureDTS, pts = PureDTS};

normalize_audio_dts(#flv_media{samples = Samples, sample_rate = SampleRate}, #video_frame{codec = aac, dts = DTS} = Frame) ->
  % ?D({DTS, SampleRate, Samples}),
  Count = (DTS*SampleRate)/(Samples*1000),
  PureDTS = round(Count)*Samples*1000 / SampleRate,
  % ?D({aac,Count,DTS, PureDTS}),
  Frame#video_frame{dts = PureDTS, pts = PureDTS};

normalize_audio_dts(_Media, Frame) ->
  % ?D({Frame#video_frame.codec, Frame#video_frame.dts}),
  Frame.

