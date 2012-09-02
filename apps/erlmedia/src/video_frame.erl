%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        Erlyvideo video_frame issues
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
%%%
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(video_frame).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/aac.hrl").
-include("../include/mp3.hrl").


-export([config_frame/1, config_frames/1, define_media_info/2, has_media_info/1, frame_sound/1]).
-export([sort/1]).
-export([meta_frame/1, meta_frame/2]).


config_frame(#stream_info{codec = aac, config = Config, track_id = TrackId}) ->
  #video_frame{
   	content = audio,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44},
	  track_id = TrackId
	};

config_frame(#stream_info{codec = h264, config = Config, track_id = TrackId}) ->
  #video_frame{
   	content = video,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = h264,
	  track_id = TrackId
	};

config_frame(_) ->
  undefined.

config_frames(#media_info{streams = Streams} = MediaInfo) ->
  Frames = [config_frame(S) || S <- Streams],
  [meta_frame(MediaInfo) | [F || F <- Frames, F =/= undefined]].


has_media_info(undefined) ->
  false;

has_media_info(#media_info{streams = Streams}) ->
  length([true || #stream_info{config = undefined, codec = Codec} <- Streams, Codec == h264 orelse Codec == aac]) == 0.
  

frame_sound(#stream_info{content = audio, codec = Codec, params = #audio_params{channels = 1, sample_rate = Rate}}) when Codec == pcma orelse Codec == pcmu ->
  {mono, bit8, Rate};

frame_sound(#stream_info{content = audio, codec = Codec, params = #audio_params{channels = 2, sample_rate = Rate}}) when Codec == pcma orelse Codec == pcmu ->
  {stereo, bit8, Rate};

frame_sound(#stream_info{content = audio, codec = speex}) ->
  {mono, bit16, rate44};

frame_sound(#stream_info{content = audio}) ->
  {stereo, bit16, rate44};

frame_sound(#stream_info{}) ->
  undefined.


find_or_add_stream(TrackId, Content, #media_info{streams = Streams}) ->
  find_or_add_stream(TrackId, Content, Streams);

find_or_add_stream(undefined, Content, Streams) ->
  case lists:keytake(Content, #stream_info.content, Streams) of
    false ->
      {#stream_info{content = Content, track_id = Content}, Streams};
    {value, Stream, Streams1} ->
      {Stream, Streams1}
  end;

find_or_add_stream(TrackId, Content, Streams) ->
  case lists:keytake(TrackId, #stream_info.track_id, Streams) of
    false ->
      {#stream_info{content = Content, track_id = TrackId}, Streams};
    {value, #stream_info{content = Content} = Stream, Streams1} ->
      {Stream, Streams1}
  end.

update_stream(#stream_info{} = Stream, #media_info{streams = Streams} = MediaInfo) ->
  MediaInfo#media_info{streams = lists:ukeymerge(#stream_info.track_id, [Stream], lists:keysort(#stream_info.track_id, Streams))}.


define_media_info(undefined, #video_frame{} = Frame) ->
  define_media_info(#media_info{flow_type = stream}, Frame);

define_media_info(#media_info{} = Media, #video_frame{content = audio, codec = empty}) ->
  Media;

define_media_info(#media_info{} = Media, #video_frame{codec = mp3, body = Body, track_id = TrackId}) ->
  {ok, Config1 = #mp3_frame{samples = Samples, sample_rate = SampleRate, channels = Channels}, _} = mp3:read(Body),
  Config = Config1#mp3_frame{body = undefined},
  {Audio, _Streams1} = find_or_add_stream(TrackId, audio, Media),
  Info = Audio#stream_info{
    codec = mp3,
    config = Config,
    params = #audio_params{channels = Channels, sample_rate = SampleRate, config = Config, samples = Samples}
  },
  update_stream(Info, Media);

define_media_info(#media_info{} = Media, #video_frame{codec = aac, flavor = config, body = Body, track_id = TrackId}) ->
  Config = #aac_config{channel_count = Channels, sample_rate = Rate, samples_per_frame = Samples} = aac:decode_config(Body),
  {Audio, _Streams1} = find_or_add_stream(TrackId, audio, Media),
  Info = Audio#stream_info{
    codec = aac,
    config = Body,
    params = #audio_params{channels = Channels, sample_rate = Rate, config = Config, samples = Samples}
  },
  update_stream(Info, Media);

define_media_info(#media_info{} = Media, #video_frame{codec = aac}) ->
  Media;

define_media_info(#media_info{} = Media, #video_frame{codec = h264, flavor = config, body = Body, track_id = TrackId}) when Body =/= undefined ->
  Metadata = h264:metadata(Body),
  {LengthSize, CfgNALS} = h264:unpack_config(Body),
  {Video, _Streams1} = find_or_add_stream(TrackId, video, Media),
  Info = Video#stream_info{
    codec = h264,
    config = Body,
    params = #video_params{
      length_size = LengthSize,
      nals = CfgNALS,
      width = proplists:get_value(width, Metadata),
      height = proplists:get_value(height, Metadata)
    }
  },
  update_stream(Info, Media);

define_media_info(#media_info{} = Media, #video_frame{codec = h264}) ->
  Media;


define_media_info(#media_info{} = Media, #video_frame{content = video, codec = Codec, body = Body, track_id = TrackId}) ->
  {Width, Height} = case flv:video_size(Body, Codec) of
     {ok, {W, H}} -> {W, H};
     {more, _} -> {undefined, undefined};
     {error, unknown} -> {undefined, undefined}
  end,
  {Video, _Streams1} = find_or_add_stream(TrackId, video, Media),
  Info = Video#stream_info{
    codec = Codec,
    params = #video_params{width = Width, height = Height}
  },
  update_stream(Info, Media);

define_media_info(#media_info{} = Media, #video_frame{content = audio, codec = Codec, sound = {Channels, _, Rate}, track_id = TrackId}) ->
  {Audio, _Streams1} = find_or_add_stream(TrackId, audio, Media),
  Info = Audio#stream_info{
    codec = Codec,
    params = #audio_params{channels = Channels, sample_rate = Rate}
  },
  update_stream(Info, Media);

define_media_info(#media_info{} = Media, _) ->
  Media.


frame_sorter(#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) when DTS1 < DTS2 -> true;
frame_sorter(#video_frame{dts = DTS, flavor = config}, #video_frame{dts = DTS, flavor = Flavor}) when Flavor =/= config -> true;
frame_sorter(#video_frame{dts = DTS, flavor = config, content = video}, #video_frame{dts = DTS, flavor = config, content = Content}) when Content=/= video -> true;
frame_sorter(#video_frame{}, #video_frame{}) -> false.

sort(Frames) ->
  lists:sort(fun frame_sorter/2, Frames).


meta_frame(#media_info{} = MediaInfo) -> meta_frame(MediaInfo, []).

meta_frame(#media_info{streams = Streams, duration = Duration}, Additional) ->
  Opts1 = case [StreamInfo || #stream_info{content = video} = StreamInfo <- Streams] of
    [] -> [];
    [#stream_info{params = #video_params{width = Width, height = Height}}|_] -> [{height, Height}, {width, Width}]
  end,

  DurationMeta = case Duration of
    undefined -> [];
    _ -> [{duration, Duration / 1000}]
  end,
  Opts2 = lists:ukeymerge(1, DurationMeta, lists:keysort(1, Opts1)),

  Add = lists:map(fun({K,V}) when is_atom(V) -> {K, atom_to_binary(V,latin1)};
                      ({K,V}) when is_tuple(V) -> {K, iolist_to_binary(io_lib:format("~p", [V]))};
                      (Else) -> Else end, Additional),

  Opts = lists:ukeymerge(1, lists:keysort(1,Add), Opts2),
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, Opts}], stream_id = 0, dts = 0, pts = 0}.
  
  

