%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        Module to generate Adobe HTTP manifest
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%---------------------------------------------------------------------------------------
-module(hds).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/mp4.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([file_manifest/1, file_manifest/2]).
-export([stream_manifest/2,stream_manifest/3, segment/3]).
-export([stream_bootstrap/2]).
-export([metadata/1, abst_info/2, asrt_info/1]).

-export([lang_frag_duration/0]).


file_manifest(Path) when is_list(Path) ->
  {ok, F} = file:open(Path, [binary,read,raw]),
  {ok, Mp4} = mp4_reader:init({file,F}, []),
  {ok, Manifest} = file_manifest(mp4_reader, Mp4),
  file:close(F),
  {ok, Manifest}.

file_manifest(Format, Reader) ->
  MediaInfo = #media_info{duration = Duration, streams = Streams} = Format:media_info(Reader),
  CommonKeyframes = video_frame:reduce_keyframes(Format:keyframes(Reader)),

  Audio = [Stream || #stream_info{content = audio} = Stream <- Streams],
  FirstLanguage = case Audio of
    [#stream_info{track_id = FirstLanguageId} | _] -> [FirstLanguageId];
    [] -> []
  end,

  Videos = [TrackId || #stream_info{content = video, track_id = TrackId} <- Streams],
  Audios = case Audio of
    [] -> [];
    [_|A_] -> [TrackId || #stream_info{track_id = TrackId} <- A_]
  end,


  Manifest = iolist_to_binary([
<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">
  <id>file</id>
  <streamType>recorded</streamType>
  <duration>">>, io_lib:format("~.2f", [Duration / 1000]), <<"</duration>
">>, 
  lists:map(fun(VideoId) ->
    BitrateTrackIds = [VideoId|FirstLanguage],
    BitrateStreams = [Stream || #stream_info{track_id = TrackId} = Stream <- Streams, lists:member(TrackId,BitrateTrackIds)],
    #stream_info{bitrate = Bitrate} = lists:keyfind(VideoId, #stream_info.track_id, BitrateStreams),
    MI = MediaInfo#media_info{streams = BitrateStreams},
    Keyframes = video_frame:reduce_keyframes(Format:keyframes(Reader, [{tracks,BitrateTrackIds}])),

    % ?DBG("~B ~w -> ~B~n~50p",[round(Duration), BitrateTrackIds,length(Keyframes), Keyframes]),

  [io_lib:format("  <bootstrapInfo profile=\"named\" id=\"bootstrap~B\">\n", [VideoId]),
    base64:encode_to_string(generate_bootstrap(Duration, Keyframes, [{duration,Duration}])),
  "\n  </bootstrapInfo>\n",
  io_lib:format("  <media streamId=\"stream~B\" ", [VideoId]),
  io_lib:format(case FirstLanguage of
    [] -> "url=\"hds/tracks-~B/\" ";
    _ -> "url=\"hds/tracks-~B,~B/\" "
  end, [VideoId|FirstLanguage]),
  case Bitrate of undefined -> ""; _ ->
    io_lib:format("bitrate=\"~B\" ", [Bitrate])
  end,
  io_lib:format("bootstrapInfoId=\"bootstrap~B\">\n", [VideoId]),
  "    <metadata>\n",
    metadata(MI),
  "\n    </metadata>\n",
  "  </media>\n\n"
  ]
  end, Videos),

  lists:map(fun(AudioId) ->
    MI = MediaInfo#media_info{streams = [lists:keyfind(AudioId,#stream_info.track_id,Streams)]},
    [io_lib:format("  <bootstrapInfo profile=\"named\" id=\"bootstrap~B\">\n", [AudioId]),
      base64:encode_to_string(generate_bootstrap(Duration, CommonKeyframes, [{duration,Duration}])),
    "\n  </bootstrapInfo>\n",
    io_lib:format("  <media streamId=\"stream~B\" url=\"hds/tracks-~B/\" lang=\"~B\" bootstrapInfoId=\"bootstrap~B\""
      " type=\"audio\" alternate=\"true\" bitrate=\"128\">\n",
        [AudioId,AudioId,AudioId,AudioId]),
    "    <metadata>\n",
      metadata(MI),
    "\n    </metadata>\n",
    "  </media>\n\n"
    ]
  end, Audios),

<<"</manifest>
">>
  ]),
  {ok, Manifest}.


lang_frag_duration() ->
  10000.

stream_bootstrap(Keyframes, Options) -> 
  BootStrap = generate_bootstrap(proplists:get_value(duration,Options),Keyframes,Options ++ [{type,live}]),
  % mp4:dump(BootStrap),
  {ok,iolist_to_binary(BootStrap)}.

stream_manifest(Format,Reader,Options) when is_atom(Format) ->
  MediaInfo = Format:media_info(Reader),
  stream_manifest(MediaInfo,Options).

stream_manifest(#media_info{streams = _Streams, duration = Duration} = MediaInfo, Options) ->
  Bitrates = proplists:get_value(bitrates, Options, [0]),
  #media_info{duration = Duration} = MediaInfo,   
  StreamType = atom_to_list(proplists:get_value(stream_type, Options, recorded)),  
  Meta64 = metadata(MediaInfo),

  
  M1 = [
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">\n"
  "  <id>livestream</id>\n"
  % "<dvrInfo beginOffset=\"0\" endOffset=\"0\" />\n",
  "  <streamType>", StreamType,"</streamType>\n",
  "  <duration>", io_lib:format("~p", [round(Duration / 1000)]), "</duration>\n",
  lists:map(fun(Bitrate) ->
    Qs = integer_to_list(Bitrate),
    BitrateS = if Bitrate > 0 -> [" bitrate=\"", Qs,"\" "] ; true -> " bitrate=\"\" " end,
    [
     "  <bootstrapInfo id=\"bootstrap",Qs,"\" profile=\"named\" url=\"bootstrap\" >",
     "</bootstrapInfo>\n",
     "  <media streamId=\"video_",Qs, "\" url=\"hds/", Qs, "/\" bootstrapInfoId=\"bootstrap", Qs,"\" ", BitrateS," >\n",
    "    <metadata>\n",
    Meta64,"\n",
    "    </metadata>\n",
    "  </media>\n"
    ]
  end, Bitrates),
  "</manifest>\n"
  ],
  HdsManifest = iolist_to_binary(M1),
  
  {ok, HdsManifest}.

metadata(MediaInfo) ->
  base64:encode_to_string(generate_metadata(MediaInfo)).

generate_metadata(#media_info{} = MediaInfo) ->
  #video_frame{body = Body} = video_frame:meta_frame(MediaInfo),
  iolist_to_binary([amf0:encode(Part) || Part <- Body]).

generate_bootstrap(Duration, Keyframes,Options) when is_float(Duration) ->
  generate_bootstrap(round(Duration), Keyframes,Options);

generate_bootstrap(Duration, [{_Time, _Id}|_] = Keyframes, Options) ->
  generate_bootstrap(Duration, [Time || {Time,_} <- Keyframes], Options);

generate_bootstrap(Duration, Keyframes,Options) ->
  CurrentMediaTime = case proplists:get_value(type, Options) of
    live -> lists:last(Keyframes);
    _ -> proplists:get_value(duration, Options, Duration)
  end,
  Bootstrap = [
  {abst, [abst_info(CurrentMediaTime, Options),
    <<1>>,
    {asrt, asrt_info(length(Keyframes))},
    <<1>>,
    {afrt, afrt_info(Duration, Keyframes,Options)}
  ]}],
  iolist_to_binary(mp4_writer:mp4_serialize(Bootstrap)).


% or_(undefined, B) -> B;
% or_(A, _) -> A.

-define(NAMED_ACCESS, 0).
-define(RANGE_ACCESS, 1).

abst_info(CurrentMediaTime, Options) ->
  BootstrapVersion = 14,
  Live = case proplists:get_value(type, Options) of
    live -> 1;
    _ -> 0
  end,
  Update = case proplists:get_value(update, Options) of
    true -> 1;
    _ -> 0
  end,
  Timescale = proplists:get_value(timescale, Options, 1000),
  <<0:32, BootstrapVersion:32, ?NAMED_ACCESS:2, Live:1, Update:1, 0:4, Timescale:32, (round(CurrentMediaTime)):64, 0:64, 0,0,0,0,0>>.

asrt_info(Count) ->
  <<0:32, 0, 1:32, 1:32, Count:32>>.


afrt_info(Duration, Keyframes,Options) ->
  Timescale = 1000,
  [<<0:32, Timescale:32, 0, (length(Keyframes)):32>>, afrt_segments_info_init(Duration, Keyframes,Options)].

afrt_segments_info_init(Duration, Keyframes,Options) ->
  StartFragment = proplists:get_value(start_fragment,Options,1),  
  afrt_segments_info(Duration, Keyframes, StartFragment, []).

afrt_segments_info(Duration, [DTS], I, Acc) when Duration > DTS ->
  lists:reverse([<<I:32, (round(DTS)):64, (trunc(Duration-DTS)):32>>|Acc]);

afrt_segments_info(Duration, [DTS], I, Acc) when Duration =< DTS ->
  lists:reverse([<<I:32, (round(DTS)):64, 0:32>>|Acc]);

afrt_segments_info(Duration, [DTS1,DTS2|Keyframes], I, Acc) ->
  afrt_segments_info(Duration, [DTS2|Keyframes], I+1, [<<I:32, (round(DTS1)):64, (trunc(DTS2 - DTS1)):32>>|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Segment generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


segment(Frames, #media_info{} = MediaInfo, Options) when is_list(Frames), is_list(Options) ->
  segment0(Frames, MediaInfo, Options).

  
segment0(Frames0, #media_info{streams = Streams1} = MI, Options) -> 
  SkipMetadata = proplists:get_value(no_metadata, Options, false),
  ShiftDts = proplists:get_value(shift_dts, Options, 0),

  Streams = case proplists:get_value(tracks, Options) of
    undefined -> Streams1;
    TrackIds -> [Stream || #stream_info{track_id = TId} = Stream <- Streams1, lists:member(TId, TrackIds)]
  end,

  MediaInfo = MI#media_info{streams = Streams},


  FirstDts = case Frames0 of
    [#video_frame{dts = DTS}|_] -> DTS + ShiftDts;
    _ -> ShiftDts
  end,
  
  Frames = lists:map(fun
    (#video_frame{dts = DTS, pts = PTS} = F) -> flv_video_frame:to_tag(F#video_frame{dts = DTS + ShiftDts, pts = PTS + ShiftDts});
    (Bin) when is_binary(Bin) -> Bin
  end, Frames0),
  
  Configs1 = [Frame#video_frame{dts = FirstDts, pts = FirstDts} || Frame <- video_frame:config_frames(MediaInfo)],
  Configs2 = case SkipMetadata of
    true -> [Config || #video_frame{content = Content} = Config <- Configs1, Content =/= metadata];
    false -> Configs1
  end,
  % ?debugFmt("configs: ~p, ~p, ~p, ~p", [MediaInfo, video_frame:config_frames(MediaInfo), Configs1, Configs2]),
  Configs = [flv_video_frame:to_tag(Config) || Config <- Configs2],
  % MetaFrame = (video_frame:meta_frame(MediaInfo))#video_frame{dts = DTS, pts = DTS},
  % Metadata = flv_video_frame:to_tag(MetaFrame),
  Blocks = [Configs, Frames],
  Segment = [<<(iolist_size(Blocks) + 8):32, "mdat">>, Blocks],
  {ok, Segment}.
  
