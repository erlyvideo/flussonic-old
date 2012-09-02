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
-include("log.hrl").

-export([manifest/2,manifest/3, segment/3, segment/4]).
-export([stream_bootstrap/2, file_bootstrap/3, file_bootstrap/2]).
-export([metadata/1, abst_info/2, asrt_info/1]).

-record(info, {
  first_dts,
  last_dts,
  shift_dts,
  stop_dts,
  duration
}).

file_bootstrap(Format,Reader,Options) ->
  #media_info{duration = Duration} = Format:media_info(Reader),
  file_bootstrap(Format:keyframes(Reader), [{duration,Duration}|Options]).

file_bootstrap(Keyframes, Options) ->
  Timestamps = [DTS || {DTS, _Key} <- Keyframes],
  Duration = proplists:get_value(duration, Options, lists:max(Timestamps)),
  BootStrap = generate_bootstrap(Duration,Timestamps,[{type,recorded}|Options]),
  {ok,iolist_to_binary(BootStrap)}.

stream_bootstrap(Keyframes, Options) -> 
  BootStrap = generate_bootstrap(0,Keyframes,[{type,live}|Options]),
  {ok,iolist_to_binary(BootStrap)}.

manifest(Format,Reader,Options) when is_atom(Format) ->
  MediaInfo = Format:media_info(Reader),
  manifest(MediaInfo,Options).

manifest(#media_info{streams = Streams, duration = Duration} = MediaInfo, Options) ->
  Bitrates = proplists:get_value(bitrates, Options, [0]),
  #media_info{duration = Duration} = MediaInfo,   
  StreamType = atom_to_list(proplists:get_value(stream_type, Options, recorded)),  
  Meta64 = metadata(MediaInfo),

  
  AlternativeAudio = case [Stream || #stream_info{content = audio} = Stream <- Streams] of
    [_Head|AudioStreams] when StreamType == "recorded" ->
      {ok, BS} = file_bootstrap([{N*10000,N} || N <- lists:seq(0,round(Duration) div 10000)], [{duration,Duration}]),
      AlternativeBS = base64:encode(BS),
      L = fun(undefined) -> "none"; (Lang_) -> Lang_ end,
      lists:map(fun(#stream_info{language = Lang}) -> [
      "  <bootstrapInfo profile=\"named\" id=\"bootstrap-",L(Lang),"\">",AlternativeBS,"</bootstrapInfo>\n"
      "  <media alternate=\"true\" type=\"audio\" lang=\"",L(Lang),"\" bootstrapInfoId=\"bootstrap-",L(Lang),"\" streamId=\"audio",L(Lang),"\" url=\"hds/lang-",L(Lang),"/\" bitrate=\"500\">\n",
      "    <metadata>",Meta64,"</metadata>\n"
      "  </media>\n"]
      end, AudioStreams);
    _ ->
      []
  end,

  Bootstrap64 = case proplists:get_value(bootstrap, Options) of
    undefined -> "";
    Bootstrap -> base64:encode_to_string(Bootstrap)
  end,
  BootstrapUrl = case Bootstrap64 of
    "" -> "url=\"bootstrap\"";
    _ -> ""
  end,
  
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
     "  <bootstrapInfo id=\"bootstrap",Qs,"\" profile=\"named\" ",BootstrapUrl," >",
     Bootstrap64, 
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
  {Frames, #info{first_dts = DTS}} = collect_frames(fun(Key) -> Format:read_frame(Reader, Key) end, Id, ShiftDts, StopDts),
  Duration = proplists:get_value(duration, Options, FileDuration),
  segment(Frames, MediaInfo#media_info{duration = Duration}, DTS).
  
segment(Frames0, #media_info{} = MediaInfo, DTS) ->
  
  Frames = lists:map(fun
    (#video_frame{} = F) -> flv_video_frame:to_tag(F);
    (Bin) when is_binary(Bin) -> Bin
  end, Frames0),
  MediaInfo,
  
  
  Configs = [flv_video_frame:to_tag(Frame#video_frame{dts = DTS, pts = DTS}) || Frame <- video_frame:config_frames(MediaInfo)],
  MetaFrame = (video_frame:meta_frame(MediaInfo))#video_frame{dts = DTS, pts = DTS},
  Metadata = flv_video_frame:to_tag(MetaFrame),
  Blocks = [Metadata, Configs, Frames],
  Segment = [<<(iolist_size(Blocks) + 8):32, "mdat">>, Blocks],
  {ok, iolist_to_binary(Segment)}.
  
collect_frames(ReadFun, FrameId, ShiftDts, StopDts) when is_number(ShiftDts) ->
  {List, Info} = collect_frames(ReadFun, FrameId, [], #info{shift_dts = ShiftDts, stop_dts = StopDts}),
  #info{first_dts = FirstDTS, last_dts = LastDTS} = Info,
  % ?D({segment,ShiftDts,round(FirstDTS),round(LastDTS)}),
  {List, Info#info{duration = round(LastDTS - FirstDTS)}};

collect_frames(ReadFun, FrameId, List, #info{first_dts = FDTS, shift_dts = ShiftDts, stop_dts = StopDts} = Info) ->
  case ReadFun(FrameId) of
    #video_frame{flavor = keyframe, dts = DTS} when length(List) > 2 andalso DTS >= StopDts ->
      {lists:reverse(List), Info#info{last_dts = DTS + ShiftDts}};
    eof ->
      {lists:reverse(List), Info};
    #video_frame{next_id=NextId, dts = DTS, pts = PTS} = Frame ->
      % ?D({collect, Frame#video_frame.codec, Frame#video_frame.flavor, round(Frame#video_frame.dts)}),
      FirstDTS = case FDTS of
        undefined -> DTS + ShiftDts;
        _ -> FDTS
      end, 
      collect_frames(ReadFun, NextId,[flv_video_frame:to_tag(Frame#video_frame{dts = DTS + ShiftDts, pts = PTS + ShiftDts})|List], Info#info{first_dts = FirstDTS, last_dts = DTS + ShiftDts})
  end.

