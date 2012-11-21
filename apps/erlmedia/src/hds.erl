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
-export([stream_manifest/2,stream_manifest/3, segment/3, segment/4]).
-export([stream_bootstrap/2]).
-export([metadata/1, abst_info/2, asrt_info/1]).

-export([lang_frag_duration/0]).

-record(info, {
  first_dts,
  last_dts,
  shift_dts,
  stop_dts,
  hardstop,
  duration
}).

file_manifest(Path) when is_list(Path) ->
  {ok, F} = file:open(Path, [binary,read,raw]),
  {ok, Mp4} = mp4_reader:init({file,F}, []),
  {ok, Manifest} = file_manifest(mp4_reader, Mp4),
  file:close(F),
  {ok, Manifest}.

file_manifest(Format, Reader) ->
  MediaInfo = #media_info{duration = Duration, streams = Streams} = Format:media_info(Reader),
  CommonKeyframes = flu_file:reduce_keyframes(Format:keyframes(Reader)),

  Audio = [Stream || #stream_info{content = audio} = Stream <- Streams],
  FirstLanguage = case Audio of
    [#stream_info{track_id = FirstLanguageId} | _] -> FirstLanguageId;
    [] -> undefined
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
    BitrateTrackIds = [VideoId,FirstLanguage],
    BitrateStreams = [Stream || #stream_info{track_id = TrackId} = Stream <- Streams, lists:member(TrackId,BitrateTrackIds)],
    [#stream_info{content = video, bitrate = Bitrate}|_] = BitrateStreams,
    MI = MediaInfo#media_info{streams = BitrateStreams},
    Keyframes = flu_file:reduce_keyframes(Format:keyframes(Reader, [{tracks,BitrateTrackIds}])),

    % ?DBG("~B -> ~240p",[VideoId,Keyframes]),

  [io_lib:format("  <bootstrapInfo profile=\"named\" id=\"bootstrap~B\">\n", [VideoId]),
    base64:encode_to_string(generate_bootstrap(Duration, Keyframes, [])),
  "\n  </bootstrapInfo>\n",
  io_lib:format("  <media streamId=\"stream~B\" url=\"hds/tracks-~B,~B/\" bitrate=\"~B\" bootstrapInfoId=\"bootstrap~B\">\n",
      [VideoId,VideoId,FirstLanguage,Bitrate div 1000, VideoId]),
  "    <metadata>\n",
    metadata(MI),
  "\n    </metadata>\n",
  "  </media>\n\n"
  ]
  end, Videos),

  lists:map(fun(AudioId) ->
    MI = MediaInfo#media_info{streams = [lists:keyfind(AudioId,#stream_info.track_id,Streams)]},
    [io_lib:format("  <bootstrapInfo profile=\"named\" id=\"bootstrap~B\">\n", [AudioId]),
      base64:encode_to_string(generate_bootstrap(Duration, CommonKeyframes, [])),
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
  BootStrap = generate_bootstrap(0,Keyframes,[{type,live}|Options]),
  {ok,iolist_to_binary(BootStrap)}.

stream_manifest(Format,Reader,Options) when is_atom(Format) ->
  MediaInfo = Format:media_info(Reader),
  stream_manifest(MediaInfo,Options).

stream_manifest(#media_info{streams = _Streams, duration = Duration} = MediaInfo, Options) ->
  Bitrates = proplists:get_value(bitrates, Options, [0]),
  #media_info{duration = Duration} = MediaInfo,   
  StreamType = atom_to_list(proplists:get_value(stream_type, Options, recorded)),  
  Meta64 = metadata(MediaInfo),

  
  % Streams1 = lists:zip(Streams, lists:seq(1, length(Streams))),
  % AlternativeAudio = case [{Stream,Num} || {#stream_info{content = audio} = Stream, Num} <- Streams1] of
  %   [_Head|AudioStreams] when StreamType == "recorded" ->
  %     % {ok, BS} = file_bootstrap([{N*lang_frag_duration(),N} || N <- lists:seq(0,round(Duration) div lang_frag_duration())], [{duration,Duration}]),
  %     BS = proplists:get_value(bootstrap, Options),
  %     AlternativeBS = base64:encode(BS),
  %     L = fun(undefined) -> "none"; (Num) when is_number(Num) -> integer_to_list(Num); (Lang_) -> Lang_ end,
  %     lists:map(fun({#stream_info{}, Num}) -> [
  %     "  <bootstrapInfo profile=\"named\" id=\"bootstrap-",L(Num),"\">",AlternativeBS,"</bootstrapInfo>\n"
  %     "  <media alternate=\"true\" type=\"audio\" lang=\"",L(Num),"\" bootstrapInfoId=\"bootstrap-",L(Num),"\" streamId=\"audio",L(Num),"\" url=\"hds/lang-",L(Num),"/\" bitrate=\"500\">\n",
  %     "    <metadata>",Meta64,"</metadata>\n"
  %     "  </media>\n"]
  %     end, AudioStreams);
  %   _ ->
  %     []
  % end,
  AlternativeAudio = [],

  M1 = [
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
  "<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">\n",
  "  <id>livestream</id>\n",
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
  AlternativeAudio,
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
  CurrentMediaTime = lists:last(Keyframes),
  Bin = mp4_writer:mp4_serialize([
  {abst, [abst_info(CurrentMediaTime, Options),
    <<1>>,
    {asrt, asrt_info(length(Keyframes))},
    <<1>>,
    {afrt, afrt_info(Duration, Keyframes,Options)}
  ]}]),
  iolist_to_binary(Bin).

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

afrt_segments_info(Duration, [DTS], I, Acc) ->
  lists:reverse([<<I:32, (round(DTS)):64, (trunc(Duration-DTS)):32>>|Acc]);

afrt_segments_info(Duration, [DTS1,DTS2|Keyframes], I, Acc) ->
  afrt_segments_info(Duration, [DTS2|Keyframes], I+1, [<<I:32, (round(DTS1)):64, (trunc(DTS2 - DTS1)):32>>|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Segment generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

segment(Format, Reader, Id, Options) ->
  #media_info{duration = FileDuration} = MediaInfo = Format:media_info(Reader),
  ShiftDts = proplists:get_value(shift_dts, Options, 0),
  StopDts = proplists:get_value(stop_dts, Options, 0),
  Hardstop = proplists:get_value(hardstop, Options, false),
  {Frames, #info{first_dts = DTS}} = collect_frames(fun(Key) -> Format:read_frame(Reader, Key) end, Id, ShiftDts, StopDts, Hardstop),
  Duration = proplists:get_value(duration, Options, FileDuration),

  Streams = case Id of
    #frame_id{tracks = Tracks} ->
      [Stream || #stream_info{track_id = TId} = Stream <- MediaInfo#media_info.streams, lists:member(TId, Tracks)];
    _ ->
      MediaInfo#media_info.streams
  end,
  segment0(Frames, MediaInfo#media_info{duration = Duration, streams = Streams}, DTS, Options).

segment(Frames0, #media_info{} = MediaInfo, DTS) ->
  segment0(Frames0, MediaInfo, DTS, []).

  
segment0(Frames0, #media_info{} = MediaInfo, DTS, Options) -> 
  SkipMetadata = proplists:get_value(no_metadata, Options, false),
  
  Frames = lists:map(fun
    (#video_frame{} = F) -> flv_video_frame:to_tag(F);
    (Bin) when is_binary(Bin) -> Bin
  end, Frames0),
  
  Configs1 = [Frame#video_frame{dts = DTS, pts = DTS} || Frame <- video_frame:config_frames(MediaInfo)],
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
  {ok, iolist_to_binary(Segment)}.
  
collect_frames(ReadFun, FrameId, ShiftDts, StopDts, Hardstop) when is_number(ShiftDts) ->
  {List, Info} = collect_frames(ReadFun, FrameId, [], #info{shift_dts = ShiftDts, stop_dts = StopDts, hardstop = Hardstop}),
  #info{first_dts = FirstDTS, last_dts = LastDTS} = Info,
  % ?D({segment,FrameId, ShiftDts,round(FirstDTS),round(LastDTS)}),
  {List, Info#info{duration = round(LastDTS - FirstDTS)}}.

collect_frames(ReadFun, FrameId, List, #info{first_dts = FDTS, shift_dts = ShiftDts, stop_dts = StopDts, hardstop = Hardstop} = Info) ->
  case ReadFun(FrameId) of
    #video_frame{flavor = keyframe, dts = DTS} when length(List) > 2 andalso DTS >= StopDts ->
      % ?D({stop,keyframe,round(DTS)}),
      {lists:reverse(List), Info#info{last_dts = DTS + ShiftDts}};
    #video_frame{dts = DTS} when Hardstop andalso DTS >= StopDts ->
      % ?D({stop,hardstop,round(DTS)}),
      {lists:reverse(List), Info#info{last_dts = DTS + ShiftDts}};
    eof ->
      {lists:reverse(List), Info};
    #video_frame{next_id=NextId, dts = DTS, pts = PTS} = Frame ->
      % ?D({collect, Frame#video_frame.codec, Frame#video_frame.flavor, round(Frame#video_frame.dts), StopDts, Hardstop}),
      FirstDTS = case FDTS of
        undefined -> DTS + ShiftDts;
        _ -> FDTS
      end, 
      collect_frames(ReadFun, NextId,[flv_video_frame:to_tag(Frame#video_frame{dts = DTS + ShiftDts, pts = PTS + ShiftDts})|List], Info#info{first_dts = FirstDTS, last_dts = DTS + ShiftDts})
  end.

