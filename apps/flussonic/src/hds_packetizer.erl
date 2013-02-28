-module(hds_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').

-define(FRAGMENTS_COUNT, 6).
-define(SEGMENT_START,1).
-define(FRAGMENT_START,1).
-define(FLUSH_TIMEOUT, 30000).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").


-export([init/1, handle_info/2, terminate/2]).


-record(hds, {
  name,
  options = [],
  start_dts,
  last_dts,
  gop = [],
  force_flush,
  fragments,
  media_info,
  fragments_count = ?FRAGMENTS_COUNT,
  segment=?SEGMENT_START,
  fragment=?FRAGMENT_START
}).

-record(fragment, {
  number,
  dts
}).

init(Options) ->
  Name = proplists:get_value(name, Options),
  gen_tracker:setattr(flu_streams, Name, [{hds,true}]),
  FragmentsCount = proplists:get_value(hds_count, Options, ?FRAGMENTS_COUNT),
  ForceFlush = erlang:send_after(?FLUSH_TIMEOUT, self(), force_hds_flush),
  {ok, #hds{
    name = Name,
    force_flush = ForceFlush,
    fragments_count = FragmentsCount,
    options = Options,
    fragments = queue:new()
  }}.


handle_info(#media_info{} = MediaInfo, #hds{} = State) ->
  {noreply, State#hds{media_info = MediaInfo}};

handle_info(#video_frame{codec = Codec, content = Content}, #hds{} = HDS) when 
  Content =/= metadata andalso Codec =/= h264 andalso Codec =/= aac andalso Codec =/= mp3 ->
  {noreply, HDS};

handle_info(#video_frame{flavor = config} = Frame, #hds{media_info = MI1} = HDS) ->
  MI2 = video_frame:define_media_info(MI1, Frame),
  {noreply, HDS#hds{media_info = MI2}};

handle_info(force_hds_flush, #hds{force_flush = OldTimer} = HDS) ->
  erlang:cancel_timer(OldTimer),
  HDS1 = flush_fragment(HDS),
  ForceFlush = erlang:send_after(?FLUSH_TIMEOUT, self(), force_hds_flush),
  {noreply, HDS1#hds{force_flush = ForceFlush}};


handle_info(#video_frame{flavor = keyframe, dts = DTS} = Frame, #hds{force_flush = OldTimer} = HDS) ->
  erlang:cancel_timer(OldTimer),
  HDS1 = flush_fragment(HDS, DTS),
  ForceFlush = erlang:send_after(?FLUSH_TIMEOUT, self(), force_hds_flush),
  {noreply, HDS1#hds{force_flush = ForceFlush, gop = [flv_video_frame:to_tag(Frame)], start_dts = DTS, last_dts = DTS}};

handle_info(#video_frame{dts = DTS} = Frame, #hds{gop = GOP} = HDS) ->
  {noreply, HDS#hds{gop = [flv_video_frame:to_tag(Frame) | GOP], last_dts = DTS}};

handle_info(_Else, State) ->
  {noreply, State}.

terminate(_,#hds{name = Name, segment = Segment, fragments = Fragments}) ->
  Nums = [N || #fragment{number = N} <- queue:to_list(Fragments)],
  [flu_stream_data:erase(Name, {hds_segment, Segment, N}) || N <- Nums],
  flu_stream_data:erase(Name, hds_manifest),
  flu_stream_data:erase(Name, bootstrap),
  gen_tracker:setattr(flu_streams, Name, [{hds,false}]),
  ok.


flush_fragment(#hds{start_dts = undefined} = HDS) ->
  HDS#hds{gop = []};

flush_fragment(#hds{last_dts = DTS} = HDS) ->
  flush_fragment(HDS, DTS + 5).

flush_fragment(#hds{media_info = undefined} = HDS, _NextDTS) ->
  HDS#hds{gop = []};

flush_fragment(#hds{start_dts = undefined} = HDS, _NextDTS) ->
  HDS#hds{gop = []};

flush_fragment(#hds{start_dts = StartDTS} = HDS, NextDTS) ->
  Duration = NextDTS - StartDTS,
  HDS1 = create_new_fragment(HDS),
  HDS2 = delete_old_fragment(HDS1),
  HDS3 = regenerate_bootstrap(HDS2, Duration),
  HDS3.



create_new_fragment(#hds{gop = GOP, start_dts = DTS, fragment = Fragment, segment = Segment, fragments = Fragments, name = Name} = HDS) ->
  Configs = make_config(HDS, DTS),

  Bin1 = [[flv_video_frame:to_tag(F) || F <- Configs], lists:reverse(GOP)],

  Bin = iolist_to_binary([<<(iolist_size(Bin1) + 8):32, "mdat">>, Bin1]),
  flu_stream_data:set(Name, {hds_segment, Segment,Fragment}, Bin),
  % erlang:put({hds_segment, Segment,Fragment}, Bin),
  HDS#hds{fragment = Fragment + 1, fragments = queue:in(#fragment{number = Fragment, dts = DTS}, Fragments), gop = []}.

delete_old_fragment(#hds{segment = Segment, fragments_count = FragmentsCount, fragments = Fragments, name = Name} = HDS) ->
  NewFragments = case queue:len(Fragments) of
    Count when Count > FragmentsCount ->
      {{value, #fragment{number = Fragment}}, Leaving} = queue:out(Fragments),
      flu_stream_data:erase(Name, {hds_segment, Segment, Fragment}),
      Leaving;
    _ -> Fragments
  end,
  HDS#hds{fragments = NewFragments}.

regenerate_bootstrap(#hds{fragments = Fragments, options = Options, name = Name, media_info = MediaInfo} = HDS, Duration) ->
  #fragment{number = FirstNumber} = queue:get(Fragments),
  Timestamps = [D || #fragment{dts = D} <- queue:to_list(Fragments)],
  {ok,Bootstrap} = hds:stream_bootstrap(Timestamps,[{start_fragment, FirstNumber},{duration,Duration}|Options]),
  flu_stream_data:set(Name,bootstrap,Bootstrap),
  % erlang:put(bootstrap,Bootstrap),
  {ok,Manifest} = hds:stream_manifest(MediaInfo#media_info{duration=0}, [{stream_type,live}|Options]),
  flu_stream_data:set(Name,hds_manifest,Manifest),
  % erlang:put(hds_manifest,Manifest),

  HDS.



make_config(#hds{media_info = MI}, DTS)->
  [F#video_frame{dts = DTS, pts = DTS} || #video_frame{} = F <- video_frame:config_frames(MI)].

