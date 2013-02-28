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
  application:start(gen_tracker),
  gen_tracker_sup:start_tracker(flu_streams),
  application:start(flussonic),
  ok.

teardown(_) ->
  error_logger:delete_report_handler(error_logger_tty_h),
  application:stop(flussonic),
  application:stop(gen_tracker),
  error_logger:add_report_handler(error_logger_tty_h),
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


% test_gop_with_empty_media_info() ->
%   {ok, HDS1} = hds_packetizer:init([{name,<<"stream1">>}]),
%   {noreply, _HDS2} = hds_packetizer:handle_info({gop, gop(2)}, HDS1),
%   ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, hds_manifest)),
%   ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, {hds_segment, 1, 1})),
%   ?assertMatch(undefined, flu_stream_data:get(<<"stream1">>, bootstrap)),
%   ok.


wait(_,_,0) -> undefined;

wait(K,V,Count) ->
  case gen_tracker:getattr(flu_streams, K, V) of
    undefined ->
      timer:sleep(50),
      wait(K,V,Count-1);
    Else ->
      Else
  end.


test_full_cycle() ->
  {ok, S} = flu_stream:autostart(<<"stream1">>, [{url,<<"passive://ok">>}]),
  S ! media_info(),

  ?assertEqual({ok, true}, wait(<<"stream1">>, hds, 10)),

  ?assertEqual(undefined, flu_stream_data:get(<<"stream1">>, hds_manifest)),

  Gop = gop(1),
  [flu_stream:send_frame(S, Frame) || Frame <- Gop],
  [flu_stream:send_frame(S, Frame) || Frame <- gop(2)],
  [flu_stream:send_frame(S, Frame) || Frame <- gop(3)],

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
  ?assertEqual(GopLen, length(Frames)-2),

  ok.











media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  MediaInfo = mp4_reader:media_info(R),
  file:close(F),
  MediaInfo.  

gop(N) ->
  {ok, F} = file:open("../../../priv/bunny.mp4",[read,binary,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  {ok, Gop} = mp4_reader:read_gop(R, N),
  file:close(F),
  Gop.













