-module(hds_packetizer_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).


hds_packetizer_test_() ->
  TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))],
  {foreach, 
    fun setup/0,
    fun teardown/1,
    [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions]
  }.


setup() ->
  error_logger:delete_report_handler(error_logger_tty_h),
  application:start(gen_tracker),
  gen_tracker_sup:start_tracker(flu_streams),
  {ok, Pid} = flu_stream_data:start_link(),
  unlink(Pid),
  {ok, Pid}.

teardown({ok, Pid}) ->
  erlang:exit(Pid, shutdown),
  application:stop(gen_tracker),
  ok.


test_init() ->
  {ok, _HDS} = hds_packetizer:init([{name,<<"stream1">>}]),
  ?assertEqual({ok, true}, gen_tracker:getattr(flu_streams, <<"stream1">>, hds)),
  ok.

test_terminate() ->
  {ok, HDS} = hds_packetizer:init([{name,<<"stream1">>}]),
  ok = hds_packetizer:terminate(normal, HDS),
  ?assertEqual({ok, false}, gen_tracker:getattr(flu_streams, <<"stream1">>, hds)),
  ok.


test_gop_with_empty_media_info() ->
  {ok, HDS1} = hds_packetizer:init([{name,<<"stream1">>}]),
  {noreply, _HDS2} = hds_packetizer:handle_info({gop, gop()}, HDS1),
  ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, hds_manifest)),
  ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, {hds_segment, 1, 1})),
  ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, bootstrap)),
  ok.



test_gop_with_media_info() ->
  {ok, HDS1} = hds_packetizer:init([{name,<<"stream1">>}]),
  {noreply, HDS2} = hds_packetizer:handle_info(media_info(), HDS1),
  Gop = gop(),
  {noreply, _HDS3} = hds_packetizer:handle_info({gop, Gop}, HDS2),

  ?assertMatch({ok, Fragment} when is_binary(Fragment),
    flu_stream_data:get(<<"stream1">>, {hds_segment, 1, 1})),
  ?assertMatch({ok, Manifest} when is_binary(Manifest),
    flu_stream_data:get(<<"stream1">>, hds_manifest)),
  ?assertMatch({ok, Bootstrap} when is_binary(Bootstrap),
    flu_stream_data:get(<<"stream1">>, bootstrap)),

  {ok, <<_:32, "mdat", FLV/binary>>} = flu_stream_data:get(<<"stream1">>, {hds_segment, 1, 1}),
  [#video_frame{content = metadata}|Frames] = flv:read_all_frames(FLV),
  % ?debugFmt("gop: ",[]),
  % [?debugFmt("~4s ~8s ~B", [Codec, Flavor, round(DTS)]) || #video_frame{codec = Codec, flavor = Flavor, dts = DTS} <- Gop],

  % ?debugFmt("~nframes: ",[]),
  % [?debugFmt("~4s ~8s ~B", [Codec, Flavor, round(DTS)]) || #video_frame{codec = Codec, flavor = Flavor, dts = DTS} <- Frames],
  GopLen = length(Gop),
  ?assertEqual(GopLen, length(Frames)),

  ok.











media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  MediaInfo = mp4_reader:media_info(R),
  file:close(F),
  MediaInfo.  


gop() ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  Gop = gop(R, true, undefined),
  file:close(F),
  Gop.

gop(R, First, Key) ->
  case mp4_reader:read_frame(R, Key) of
    #video_frame{flavor = keyframe} when not First ->
      [];
    #video_frame{flavor = keyframe, next_id = Next} = F when First ->
      [F|gop(R,false,Next)];
    #video_frame{next_id = Next} = F ->
      [F|gop(R,First,Next)]
  end.


















