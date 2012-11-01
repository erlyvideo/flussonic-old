-module(flu_publish_proxy).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, terminate/2]).


start_link(RTMP, Stream) ->
  gen_server:start_link(?MODULE, [RTMP, Stream], []).

-record(proxy, {
  rtmp,
  stream,
  media_info,
  start_spec,
  delaying = true,
  delayed = []
}).

-define(VIDEO, 200).
-define(AUDIO, 201).
-define(META, 202).

-define(LIMIT, 100).

-define(START_FRAMES, 5).

init([StartSpec, Stream]) ->
  put(flu_name, publish_proxy),
  self() ! init,
  erlang:monitor(process, Stream),
  {ok, #proxy{start_spec = StartSpec, stream = Stream}}.

handle_info(init, #proxy{start_spec = StartSpec} = Proxy) ->
  RTMP = if
    is_pid(StartSpec) ->
      erlang:monitor(process, StartSpec),
      StartSpec;
    is_function(StartSpec) ->
      case StartSpec() of
        {ok, Pid} ->
          erlang:monitor(process, Pid),
          Pid;
        {error, Error} ->
          ?ERR("Failed to connect to upstream: ~p", [Error]),
          throw({stop, normal, Proxy})
      end;
    is_list(StartSpec) orelse is_binary(StartSpec) ->
      case rtmp_lib:play(StartSpec) of
        {ok, Pid} ->
          erlang:monitor(process, Pid),
          Pid;
        {error, Error} ->
          ?ERR("Failed to connect to \"~s\": ~p", [StartSpec, Error]),
          throw({stop, normal, Proxy})
      end;
    StartSpec == undefined ->
      undefined
  end,
  {noreply, Proxy#proxy{rtmp = RTMP}};

handle_info({set_source, RTMP}, #proxy{rtmp = undefined} = Proxy) ->
  erlang:monitor(process, RTMP),
  {noreply, Proxy#proxy{rtmp = RTMP}};

handle_info(#video_frame{} = Frame, #proxy{} = Proxy) ->
  Proxy1 = handle_frame(Frame, Proxy),
  {noreply, Proxy1};

handle_info(#media_info{streams = Streams} = MI1, #proxy{media_info = undefined, stream = Stream} = Proxy) ->
  MediaInfo = MI1#media_info{streams = [S#stream_info{track_id = rewrite_track_id(Content)} || #stream_info{content = Content} = S <- Streams]},
  case video_frame:has_media_info(MediaInfo) of
    true -> 
      Stream ! MediaInfo,
      {noreply, Proxy#proxy{media_info = MediaInfo, delaying = false}};
    false ->
      {noreply, Proxy#proxy{media_info = MediaInfo, delaying = true}}
  end;

% handle_info({rtmp, _RTMP, #rtmp_message{type = video, body = <<23,2,0,0,0>>}}, Stream) ->
%   {noreply, Stream};
% 
handle_info({rtmp, _RTMP, #rtmp_message{type = Type, timestamp = Timestamp, body = Body}}, Proxy) when (Type == audio orelse Type == video) andalso size(Body) > 0 ->
  case flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, content = Type}, Body) of
    #video_frame{flavor = Flavor} = Frame when Flavor == keyframe orelse Flavor == frame orelse Flavor == config ->
      handle_info(Frame, Proxy);
    _ ->
      {noreply, Proxy}
  end;

handle_info({rtmp, _RTMP, #rtmp_message{type = Type}}, Proxy) when Type == burst_start orelse Type == burst_stop ->
  {noreply, Proxy};

handle_info({rtmp, _RTMP, #rtmp_message{type = metadata}}, Proxy) ->
  {noreply, Proxy};

handle_info({rtmp, _RTMP, disconnect, _}, Proxy) ->
  {stop, normal, Proxy};

handle_info({rtmp, _RTMP, Message}, Proxy) ->
  ?D(Message),
  {noreply, Proxy};



handle_info({'DOWN', _, _, _, _}, #proxy{} = Proxy) ->
  {stop, normal, Proxy}.


handle_frame(#video_frame{} = Frame, #proxy{} = Proxy) ->
  Frame1 = rewrite_track_id(Frame),
  Proxy1 = #proxy{delayed = Delayed} = handle_frame1(Frame1, Proxy),
  if length(Delayed) > ?LIMIT -> throw({stop, too_much_delayed_frames, Proxy1#proxy{delayed = []}});
    true -> Proxy1
  end.

rewrite_track_id(#video_frame{content = Content} = Frame) -> Frame#video_frame{track_id = rewrite_track_id(Content)};
rewrite_track_id(video) -> ?VIDEO;
rewrite_track_id(audio) -> ?AUDIO;
rewrite_track_id(_) -> ?META.

handle_frame1(#video_frame{flavor = command}, Proxy) ->
  Proxy;

handle_frame1(#video_frame{body = <<2,0,0,0>>}, Proxy) ->
  Proxy;

handle_frame1(#video_frame{content = metadata, body = [<<"@setDataFrame">>,<<"onMetaData">>, {object, Meta}]} = Frame, 
            #proxy{media_info = undefined} = Proxy) ->
  handle_frame1({metadata, Meta, Frame}, Proxy);

handle_frame1(#video_frame{content = metadata, body = [<<"@setDataFrame">>,<<"onMetaData">>, {object, _, Meta}]} = Frame, 
            #proxy{media_info = undefined} = Proxy) ->
  handle_frame1({metadata, Meta, Frame}, Proxy);

handle_frame1(#video_frame{content = metadata, body = [<<"@setDataFrame">>,<<"onMetaData">>, Meta]} = Frame, 
            #proxy{media_info = undefined} = Proxy) ->
  handle_frame1({metadata, Meta, Frame}, Proxy);

handle_frame1({metadata, Meta1, Frame}, #proxy{media_info = undefined, delayed = Delayed} = Proxy) ->
  Meta = [{to_b(Key),Value} || {Key, Value} <- Meta1],

  FPS = proplists:get_value(<<"framerate">>, Meta),
  Width = proplists:get_value(<<"width">>, Meta),
  Height = proplists:get_value(<<"height">>, Meta),
  VideoCodec = case proplists:get_value(<<"videocodecid">>, Meta) of
    <<"avc1">> -> h264;
    <<"vp6">> -> vp6;
    7.0 -> h264;
    ElseV -> ElseV
  end,
  Video = case VideoCodec of
    undefined -> [];
    _ -> [#stream_info{codec = VideoCodec, content = video, track_id = ?VIDEO, params = #video_params{fps = FPS, width = Width, height = Height}}]
  end,
  AudioCodec = case proplists:get_value(<<"audiocodecid">>, Meta) of
    <<"mp4a">> -> aac;
    <<"mp3">> -> mp3;
    <<".mp3">> -> mp3;
    <<"nmos">> -> nellymoser;
    10.0 -> aac;
    ElseA -> ElseA
  end,
  Audio = case AudioCodec of
    undefined -> [];
    _ -> [#stream_info{codec = AudioCodec, content = audio, track_id = ?AUDIO}]
  end,
  MediaInfo = #media_info{flow_type = stream, streams = Video ++ Audio},
  Proxy#proxy{media_info = MediaInfo, delaying = true, delayed = Delayed ++ [Frame]};

handle_frame1(#video_frame{} = Frame, #proxy{media_info = MI1, delaying = true, delayed = Delayed, stream = Stream} = Proxy) ->

  MI2 = video_frame:define_media_info(MI1, Frame),
  Delaying = not video_frame:has_media_info(MI2) orelse length(Delayed) =< ?START_FRAMES,
  
  Delayed1 = Delayed ++ [Frame],
  case Delaying of
    true ->
      Proxy#proxy{media_info = MI2, delayed = Delayed1};
    false ->
      Stream ! MI2,
      [Stream ! F || F <- Delayed1],
      Proxy#proxy{media_info = MI2, delaying = false, delayed = []}
  end;

handle_frame1(#video_frame{} = Frame, #proxy{delaying = false, media_info = MI1, stream = Stream} = Proxy) ->
  MI2 = video_frame:define_media_info(MI1, Frame),
  Frame1 = normalize_aac(Frame, MI2),
  case MI2 of
    MI1 ->
      Stream ! Frame1,
      Proxy;
    _ ->
      gen_server:call(Stream, {set, MI2}),
      Stream ! Frame1,
      Proxy#proxy{media_info = MI2}
  end.

to_b(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
to_b(Bin) when is_binary(Bin) -> Bin.


normalize_aac(#video_frame{codec = aac, flavor = frame, dts = DTS, track_id = TrackId} = Frame, #media_info{streams = Streams}) ->
  case get(start_dts) of 
    undefined -> 
      put(start_dts, DTS), 
      put(sound_count, 0),
      #stream_info{params = #audio_params{sample_rate = SampleRate}} = lists:keyfind(TrackId, #stream_info.track_id, Streams),
      put(sound_rate, SampleRate),
      ok;
   _ -> ok
  end,
  Count = get(sound_count),
  Rate = get(sound_rate),
  GoodDTS = 1024*Count*1000 / Rate + get(start_dts),
  
  ProperDTS = if abs(GoodDTS - DTS) < 2 -> put(sound_count, Count + 1), GoodDTS;
    true -> erase(start_dts), erase(sound_count), erase(sound_rate), DTS
  end,
  
  Frame#video_frame{dts = ProperDTS};

normalize_aac(Frame, _) ->
  Frame.

  


terminate(_,_) -> ok.
