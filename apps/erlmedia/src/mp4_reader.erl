%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        ISO Media mp4 reader
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(mp4_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include("../include/video_frame.hrl").
-include("../include/mp4.hrl").
-include("../include/aac.hrl").
-include("../include/media_info.hrl").
-include("log.hrl").
-define(TRACK_OFFSET, 255).


-export([init/2, media_info/1, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).
-export([tracks_for/2]).
-export([keyframes/1]).

-define(FRAMESIZE, 8).

can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  lists:member(filename:extension(Name), [".mp4", ".m4a", ".f4v", ".mov"]).

write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).




init(Reader, Options) -> 
  {ok, MP4Media} = mp4:open(Reader, Options),
  
  %Tracks = tuple_to_list(MP4Media#mp4_media.tracks) ++ SrtFrames,
  % Tracks = tuple_to_list(MP4Media#mp4_media.tracks),

  % Bitrates = [Bitrate || #mp4_track{bitrate = Bitrate, content = Content} <- Tracks, Content == video],
  % Languages = [Lang || #mp4_track{language = Lang, content = Content} <- Tracks, Content == audio],
  % ?D({"MP4", Options, [Track#mp4_track{frames = frames} || Track <- Tracks]}),

  {ok, MP4Media#mp4_media{options = Options}}.





% track_by_language([], _) -> {undefined, 0};
% track_by_language([Track|_], undefined) -> {Track, mp4:frame_count(Track)};
% track_by_language([Track|_] = Tracks, Language) -> track_by_language(Tracks, Language, Track).
% 
% track_by_language([#mp4_track{language = Lang} = Track|_], Lang, _Default) -> ?D({"Selected track", Lang}), {Track, mp4:frame_count(Track)};
% track_by_language([_|Tracks], Lang, Default) -> track_by_language(Tracks, Lang, Default);
% track_by_language([], _Lang, Default) -> {Default, mp4:frame_count(Default)}.
% 



properties(#mp4_media{additional = Additional, width = Width, height = Height, duration = Duration} = MP4Media) -> 
  Tracks = tuple_to_list(MP4Media#mp4_media.tracks),
  TrackInfo = [[{id,Id},{content,Content},{bitrate,Bitrate},{language, Language},{codec,Codec}] || 
                #mp4_track{language = Language, content = Content, bitrate = Bitrate, track_id = Id, codec = Codec} <- Tracks],
  Bitrates = [Bitrate || #mp4_track{bitrate = Bitrate, content = Content} <- Tracks, Content == video],
  Languages = [Language || #mp4_track{language = Language, content = Content} <- Tracks, Content == audio],
  
  Opt1 = case {Width, Height} of
    {undefined, undefined} -> [];
    _ -> [{width, Width},{height, Height}]
  end,
  Opt1 ++ [{type, file},
   {duration, Duration},
   {tracks, TrackInfo},
   {bitrates, Bitrates},
   {languages, Languages}] ++ Additional.



media_info(#mp4_media{additional = Additional, duration = Duration, tracks = Tracks}) -> 
  Streams = lists:map(fun(#mp4_track{content = Content} = Track) ->
    Params = case Content of
      video -> 
        {LengthSize, CfgNALS} = h264:unpack_config(Track#mp4_track.decoder_config),
        #video_params{
          length_size = LengthSize,
          nals = CfgNALS,
          width = Track#mp4_track.width,
          height = Track#mp4_track.height
        };
      audio when Track#mp4_track.codec == aac andalso Track#mp4_track.decoder_config =/= undefined -> 
        AudioConfig = #aac_config{channel_count = Channels, sample_rate = SampleRate} = aac:decode_config(Track#mp4_track.decoder_config),
        #audio_params{channels = Channels, sample_rate = SampleRate, config = AudioConfig};
      audio -> #audio_params{};
      _ -> undefined
    end,
    #stream_info{
      content   = Track#mp4_track.content,
      track_id  = Track#mp4_track.track_id + ?TRACK_OFFSET,
      codec     = Track#mp4_track.codec,
      config    = Track#mp4_track.decoder_config,
      bitrate   = Track#mp4_track.bitrate,
      language  = Track#mp4_track.language,
      params    = Params
    }
  end, tuple_to_list(Tracks)),
  
  #media_info{
    flow_type = file,
    streams   = Streams,
    duration  = Duration,
    options   = Additional 
  }.
  



find_track(#mp4_media{}, _, false, _) -> [];
find_track(#mp4_media{tracks = Tracks}, _, all, Content) -> [Id || #mp4_track{content = C, track_id = Id} <- tuple_to_list(Tracks), C == Content];
find_track(#mp4_media{}, _, Number, _) when is_number(Number) ->  [Number];
find_track(#mp4_media{}, _, Numbers, _) when is_list(Numbers) ->  Numbers;

find_track(#mp4_media{tracks = Tracks}, Pos, Value, Content) ->
  find_track(Tracks, Pos, Value, Content);

find_track(Tracks, Pos, Value, Content) ->
  find_track(Tracks, Pos, Value, 1, Content, []).
  
find_track(Tracks, _Pos, _Value, Index, _Content, Default) when Index > size(Tracks) ->
  Default;
  
find_track(Tracks, Pos, Value, Index, Content, _Default) when element(Pos,element(Index,Tracks)) == Value andalso (element(Index,Tracks))#mp4_track.content == Content ->
  % ?D({got,Pos,Value,Content,Index}),
  [Index];

find_track(Tracks, Pos, Value, Index, Content, _Default) when (element(Index,Tracks))#mp4_track.content == Content ->
  % ?D({default,Content,Index}),
  find_track(Tracks, Pos, Value, Index+1, Content, case _Default of [] -> [Index]; _ -> _Default end);

find_track(Tracks, Pos, Value, Index, Content, Default) ->
  find_track(Tracks, Pos, Value, Index + 1, Content, Default).


first(Media, Options) ->
  first(Media, Options, 0, 0).


tracks_for(#mp4_media{} = Media, Options) ->
  Audio = find_track(Media, #mp4_track.language, proplists:get_value(language, Options), audio),
  Video = find_track(Media, #mp4_track.bitrate, proplists:get_value(bitrate, Options), video),
  Subtitle = find_track(Media, #mp4_track.language, proplists:get_value(subtitle, Options), text),
  Video ++ Audio ++ Subtitle.

first(#mp4_media{} = Media, Options, Id, DTS) when is_number(Id) ->
  first(Media, Options, #frame_id{id = Id, tracks = tracks_for(Media, Options)}, DTS);

first(#mp4_media{tracks = Tracks}, _Options, #frame_id{tracks = TrackIds} = Id, DTS) ->
  
  Configs = [{Content,TrackId} || #mp4_track{decoder_config = C, content = Content, track_id = TrackId} <- [element(I,Tracks) ||  I <- TrackIds], C =/= undefined],
  
  ProperId = lists:foldr(fun({Content,TrackId}, Prev) ->
    {config,Content,TrackId,DTS, Prev}
  end, Id, Configs),
  
  ProperId.

codec_config({_Type, undefined}, _Media) ->
  undefined;

codec_config({video,TrackID}, #mp4_media{tracks = Tracks}) when is_number(TrackID) ->
  #mp4_track{codec = Codec, decoder_config = Config} = element(TrackID, Tracks),
  #video_frame{
   	content = video,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
		codec   = Codec
	};

codec_config({audio,TrackID}, #mp4_media{tracks = Tracks}) when is_number(TrackID) ->
  #mp4_track{codec = Codec, decoder_config = Config} = element(TrackID, Tracks),
  case Config of
    undefined -> undefined;
    _ ->
      #video_frame{       
       	content = audio,
       	flavor  = config,
    		dts     = 0,
    		pts     = 0,
    		body    = Config,
    	  codec	  = Codec,
    	  sound   = {stereo, bit16, rate44}
    	}
  end.



read_frame(MediaInfo, undefined) ->
  read_frame(MediaInfo, first(MediaInfo, []));

read_frame(#mp4_media{} = Media, {config, Content, TrackId, DTS, Pos}) ->
  Frame = codec_config({Content,TrackId}, Media),
  Frame#video_frame{next_id = Pos, dts = DTS, pts = DTS, track_id = TrackId + ?TRACK_OFFSET};

read_frame(_, eof) ->
  eof;

read_frame(#mp4_media{} = Media, Id) ->
  case mp4:read_frame(Media, Id) of
    eof ->
      eof;
    #mp4_frame{content = text, next_id = Next, body = Data, track_id = TrackId} = Frame ->
		  VideoFrame = video_frame(text, Frame, Data),
		  VideoFrame#video_frame{next_id = Next, track_id = TrackId + ?TRACK_OFFSET};
    #mp4_frame{offset = Offset, size = Size, content = Content, next_id = Next, track_id = TrackId} = Frame ->
      % ?D({"read frame", Id, Offset, Size,Content}),
    	case read_data(Media, Offset, Size) of
    		{ok, Data, _} ->
    		  VideoFrame = video_frame(Content, Frame, Data),
    		  VideoFrame#video_frame{next_id = Next, track_id = TrackId + ?TRACK_OFFSET};
        eof -> eof;
        {error, Reason} -> {error, Reason}
      end
  end.
  

read_data(#mp4_media{reader = {M, Dev}} = Media, Offset, Size) ->
  case M:pread(Dev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, Media};
    Else -> Else
  end.
  

video_frame(video, #mp4_frame{dts = DTS, keyframe = Keyframe, pts = PTS, codec = Codec}, Data) ->
  #video_frame{
   	content = video,
		dts     = DTS,
		pts     = PTS,
		body    = Data,
		flavor  = case Keyframe of
		  true ->	keyframe;
		  _ -> frame
	  end,
		codec   = Codec
  };  

video_frame(text, #mp4_frame{dts = DTS, pts = PTS, codec = Codec}, Data) ->
  #video_frame{
   	content = metadata,
		dts     = DTS,
		pts     = DTS,
		flavor  = frame,
		codec   = Codec,
		body    = [<<"onTextData">>, {object, [
		  {name, onCuePoint},
		  {type, event},
		  {'begin', DTS},
  		{'end', PTS},
		  {text, Data}
		]}]
  };  

video_frame(audio, #mp4_frame{dts = DTS, codec = Codec}, Data) ->
  #video_frame{       
   	content = audio,
		dts     = DTS,
		pts     = DTS,
  	body    = Data,
  	flavor  = frame,
	  codec	  = Codec,
	  sound	  = {stereo, bit16, rate44}
  }.



seek(#mp4_media{} = Media, Timestamp, Options) when Timestamp =< 0 orelse Timestamp == undefined ->
  {first(Media,Options), 0};

seek(#mp4_media{duration = Duration}, Timestamp, _Options) when Timestamp > Duration ->
  undefined;

seek(#mp4_media{} = Media, Timestamp, Options) ->
  % TODO: insert here ability to seek in options
  Tracks = tracks_for(Media, Options),
  % ?D({mp4_seek, Timestamp, mp4:seek(Media, Video, Timestamp, proplists:get_value(seek_mode, Options, keyframe))}),
  case mp4:seek(Media, Tracks, Timestamp, proplists:get_value(seek_mode, Options, keyframe)) of
    undefined -> undefined;
    {Id, DTS} ->
      first(Media, Options, Id, DTS)
  end.

keyframes(#mp4_media{} = Media) ->
  Options = [],
  Tracks = tracks_for(Media, Options),
  Keyframes = mp4:keyframes(Media, Tracks),
  [{DTS, #frame_id{id = Id, tracks = Tracks}} || {DTS, Id} <- Keyframes].


