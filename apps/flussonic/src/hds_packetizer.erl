-module(hds_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').

-define(FRAGMENTS_COUNT, 20).
-define(SEGMENT_START,1).
-define(FRAGMENT_START,1).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").


-export([init/1, handle_info/2, terminate/2]).


-record(hds, {
  name,
  options = [],
  fragments = [],
  media_info,
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
  {ok, #hds{
    name = Name, 
    options = Options
  }}.


handle_info(#media_info{} = MediaInfo, #hds{} = State) ->
  {noreply, State#hds{media_info = MediaInfo}};

handle_info({gop, GOP}, #hds{media_info = MediaInfo} = HDS) when MediaInfo =/= undefined ->
  
  GOP1 = filter_flv_frames(GOP),
  HDS1 = create_new_fragment(GOP1, HDS),
  HDS2 = delete_old_fragment(HDS1),
  HDS3 = regenerate_bootstrap(HDS2),
  {noreply, HDS3};
  
handle_info(_Else, State) ->
  {noreply, State}.

terminate(_,_) -> ok.


good_codecs() ->
  [h264, mp3, aac, vp6, sorenson, pcma].

filter_flv_frames(GOP) ->
  [F || #video_frame{codec = Codec} = F <- GOP, lists:member(Codec, good_codecs())].

create_new_fragment([#video_frame{dts = DTS}|_] = GOP, #hds{fragment = Fragment, segment = Segment, fragments = Fragments, name = Name} = HDS) ->
  Configs = make_config(HDS, DTS),

  Bin1 = [[flv_video_frame:to_tag(F) || F <- Configs], [flv_video_frame:to_tag(F) || F <- GOP]],
  Bin = iolist_to_binary([<<(iolist_size(Bin1) + 8):32, "mdat">>, Bin1]),
  flu_stream_data:set(Name, {hds_segment, Segment,Fragment}, Bin),
  % erlang:put({hds_segment, Segment,Fragment}, Bin),
  HDS#hds{fragment = Fragment + 1, fragments = [#fragment{number = Fragment, dts = DTS}|Fragments]}.

delete_old_fragment(#hds{segment = Segment, fragments = Fragments, name = Name} = HDS) ->
  NewFragments = if length(Fragments) > ?FRAGMENTS_COUNT ->
    {Leaving, Deleting} = lists:split(?FRAGMENTS_COUNT, Fragments),
    % [erlang:erase({hds_segment, Segment, Frag}) || #fragment{number = Frag} <- Deleting],
    [flu_stream_data:erase(Name, {hds_segment, Segment, Frag}) || #fragment{number = Frag} <- Deleting],
    Leaving;
  true -> Fragments
  end,
  HDS#hds{fragments = NewFragments}.

regenerate_bootstrap(#hds{fragments = Fragments, options = Options, name = Name, media_info = MediaInfo} = HDS) ->
  #fragment{number = FirstNumber} = lists:nth(length(Fragments), Fragments),
  Timestamps = lists:reverse([D || #fragment{dts = D} <- Fragments]),
  {ok,Bootstrap} = hds:stream_bootstrap(Timestamps,[{start_fragment, FirstNumber}|Options]),
  flu_stream_data:set(Name,bootstrap,Bootstrap),
  % erlang:put(bootstrap,Bootstrap),
  {ok,Manifest} = hds:manifest(MediaInfo#media_info{duration=0}, [{stream_type,live}|Options]),
  flu_stream_data:set(Name,hds_manifest,Manifest),
  % erlang:put(hds_manifest,Manifest),

  HDS.



make_config(#hds{media_info = MI}, DTS)->
  [F#video_frame{dts = DTS, pts = DTS} || #video_frame{} = F <- video_frame:config_frames(MI)].

