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
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").

-behaviour(gen_format).
-export([init/2, read_frame/2, media_info/1, properties/1, seek/3, can_open_file/1, write_frame/2]).
-export([keyframes/1]).

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


can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  filename:extension(Name) == ".flv".


write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).




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

first(Media) ->
  first(Media, flv:data_offset(), 0).

first(#flv_media{media_info = MediaInfo}, Id, DTS) ->
  Configs = video_frame:config_frames(MediaInfo),
  case length([true || #video_frame{content = audio} <- Configs]) of
    0 ->
      case length([true || #video_frame{content = video} <- Configs]) of
        0 ->
          Id;
        _ ->
          {video, Id, DTS}  
      end;
    _ ->
      {audio, Id, DTS}
  end.


properties(#flv_media{metadata = Meta}) -> Meta.


media_info(#flv_media{media_info = MediaInfo}) -> MediaInfo.

init_media_info(#flv_media{reader = Reader} = Media, Offset) ->
  Frames = read_frame_list(Reader, Offset, []),
  MediaInfo = lists:foldl(fun(Frame, MI) ->
    video_frame:define_media_info(MI, Frame)
  end, #media_info{flow_type = file}, Frames),
    
  Media1 = Media#flv_media{media_info = MediaInfo},
  Media2 = case [Params || #stream_info{content = audio, params = #audio_params{} = Params} <- MediaInfo#media_info.streams] of
    [#audio_params{sample_rate = SampleRate, samples = Samples}|_] ->
      Media1#flv_media{sample_rate = SampleRate, samples = Samples};
    [] ->
      Media1
  end,  
  case [M || #video_frame{content = metadata, body = M} <- Frames] of
    [Meta|_] ->
      parse_metadata(Meta, Media2);
    _ ->
      Media2
  end.  


read_frame_list(_Reader, _Offset, Frames) when length(Frames) == 10 ->
  lists:reverse(Frames);

read_frame_list(Reader, Offset, Frames) ->
  case flv:read_frame(Reader, Offset) of
    #video_frame{next_id = Next} = Frame ->
      read_frame_list(Reader, Next, [Frame|Frames]);
    eof ->
      lists:reverse(Frames)
  end.


get_int(Key, Meta, Coeff) ->
  case {proplists:get_value(Key, Meta), Coeff} of
    {undefined, _} -> undefined;
    {{object, []}, _} -> undefined;
    {Value, Coeff} when is_number(Coeff) andalso is_number(Value) -> Value*Coeff;
    {Value, {M, F}} -> 
      try M:F(round(Value)) of
        Int -> Int
      catch
        _:_ -> undefined
      end
  end.

b_to_atom(A) when is_atom(A) -> A;
b_to_atom(A) when is_binary(A) -> binary_to_atom(A, latin1).

parse_metadata([<<"onMetaData">>, {object, Meta}], #flv_media{} = Media) ->
  parse_metadata([<<"onMetaData">>, Meta], Media);

parse_metadata([<<"onMetaData">>, Meta], #flv_media{media_info = MI1} = Media) ->
  Meta1 = [{b_to_atom(K),V} || {K,V} <- Meta],
  
  Meta2 = [{b_to_atom(K),V} || {K,V} <- Meta1, K =/= duration andalso K =/= keyframes andalso K =/= times],

  Duration = get_int(duration, Meta1, 1000),
  
  _Width = case get_int(width, Meta1, 1) of
    undefined -> undefined;
    ElseW -> round(ElseW)
  end,
  _Height = case get_int(height, Meta1, 1) of
    undefined -> undefined;
    ElseH -> round(ElseH)
  end,
  
  
  Media1 = Media#flv_media{
    bitrate = case proplists:get_value(videodatarate, Meta1) of
      undefined -> undefined;
      BR -> round(BR*1000)
    end,
    metadata = [{duration,Duration}|Meta2],
    media_info = MI1#media_info{duration = Duration}
  },
  case proplists:get_value(keyframes, Meta1) of
    {object, Keyframes} ->
      Offsets = proplists:get_value(filepositions, Keyframes),
      Times = proplists:get_value(times, Keyframes),
      Shifts = lists:ukeysort(1, lists:zip(Times, Offsets)),
      % ?D(lists:zip(Times,Offsets)),
      Frames = iolist_to_binary([<<(round(Time*1000)):64, (round(Offset)):64>> || {Time, Offset} <- Shifts]),
      Media1#flv_media{keyframes = Frames};
    _ -> 
      Media1
  end;

parse_metadata(Meta, #flv_media{} = Media) ->
  ?D({"Unknown metadata", Meta}),
  Media.


keyframes(#flv_media{keyframes = Frames}) ->
  [{Time, Offset} || <<Time:64, Offset:64>> <= Frames].


seek(#flv_media{} = Media, TS, _Options) when TS == 0 orelse TS == undefined ->
  {{audio, first(Media), 0}, 0};

seek(#flv_media{keyframes = undefined} = Media, Timestamp, _Options) ->
  find_frame_in_file(Media, Timestamp);

seek(#flv_media{} = Media, Timestamp, _Options) ->
  find_keyframe_in_table(Media, Timestamp).
  
find_keyframe_in_table(Media, Timestamp) ->  
  _TimestampInt = round(Timestamp),
  Ids = [{Time,Offset} || {Time,Offset} <- keyframes(Media), Time =< Timestamp],
  
  % ?D({zz, ets:tab2list(FrameTable)}),
  
  case lists:reverse(Ids) of
    [{DTS, Offset} | _] -> {{audio,Offset,DTS}, DTS};
    _ -> undefined
  end.

find_frame_in_file(Media, Timestamp) ->
  find_frame_in_file(Media, Timestamp, keyframe).

find_frame_in_file(Media, Timestamp, SeekMode) ->
  find_frame_in_file(Media, Timestamp, 0, first(Media), first(Media), SeekMode).


find_frame_in_file(Media, Timestamp, PrevTS, PrevOffset, Offset, SeekMode) ->
  case read_frame(Media, Offset) of
    
    #video_frame{dts = DTS} when DTS >= Timestamp andalso SeekMode == frame -> 
      {{audio, Offset, DTS}, DTS};
    #video_frame{flavor = keyframe, dts = DTS} when DTS > Timestamp andalso SeekMode == keyframe -> 
      {{audio, PrevOffset, PrevTS}, PrevTS};
      
      
    #video_frame{flavor = keyframe, dts = DTS, next_id = Next} -> 
      find_frame_in_file(Media, Timestamp, DTS, Offset, Next, SeekMode);

    #video_frame{dts = DTS, next_id = Next} when SeekMode == frame -> 
      find_frame_in_file(Media, Timestamp, DTS, Offset, Next, SeekMode);
      
    #video_frame{next_id = Next} ->
      find_frame_in_file(Media, Timestamp, PrevTS, PrevOffset, Next, SeekMode);
    eof when PrevTS == undefined -> 
      undefined;
    eof ->  
      {{audio, PrevOffset, PrevTS}, PrevTS}
  end.
  
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

read_frame(Media, undefined) ->
  read_frame(Media, first(Media));

% read_frame(#flv_media{metadata_offset = Offset, reader = Reader} = Media, Offset) ->
%   % ?D({"Skip metadata", Offset}),
%   case (catch flv:read_frame(Reader, Offset)) of
%     #video_frame{next_id = Next} -> read_frame(Media, Next);
%     {'EXIT', Error} -> ?D({error_reading_flv,Offset,Error, erlang:get_stacktrace()}), {M,D} = Reader, ?D({flv_dump, M:pread(D,Offset - 300, 600)}), erlang:exit(Error);
%     Else -> Else
%   end;
% 
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

