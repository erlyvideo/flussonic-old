-module(flu_stream_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


rewrite_stream_manifest_test() ->
  Manifest1 = <<"<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">
<id>livestream</id>
<streamType>live</streamType>
<duration>0</duration>
<bootstrapInfo id=\"bootstrap0\" profile=\"named\" url=\"bootstrap\"></bootstrapInfo>
<media streamId=\"video_0\" url=\"hds/0/\" bootstrapInfoId=\"bootstrap0\" bitrate=\"\">
<metadata> AgAKb25NZXRhRGF0YQMADWF1ZGlvY2hhbm5lbHMAQAAAAAAAAAAADGF1ZGlvY29kZWNpZABAJAAAAAAAAAAPYXVkaW9zYW1wbGVyYXRlAEDncAAAAAAAAAhkdXJhdGlvbgAAAAAAAAAAAAAGaGVpZ2h0AEByAAAAAAAAAAx2aWRlb2NvZGVjaWQAQBwAAAAAAAAABXdpZHRoAECAAAAAAAAAAAAJ </metadata>
</media>
</manifest>">>,
  {ok, Manifest2} = flu_stream:rewrite_manifest(Manifest1, <<"token1">>),
  ?assertMatch({_,_}, binary:match(Manifest2, <<"url=\"bootstrap?token=token1\"">>)),
  ok.


no_rewrite_stream_manifest_test() ->
  Manifest1 = <<"<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">
<id>livestream</id>
<streamType>live</streamType>
<duration>0</duration>
<bootstrapInfo id=\"bootstrap0\" profile=\"named\">bootstrap</bootstrapInfo>
<media streamId=\"video_0\" url=\"hds/0/\" bootstrapInfoId=\"bootstrap0\" bitrate=\"\">
<metadata> AgAKb25NZXRhRGF0YQMADWF1ZGlvY2hhbm5lbHMAQAAAAAAAAAAADGF1ZGlvY29kZWNpZABAJAAAAAAAAAAPYXVkaW9zYW1wbGVyYXRlAEDncAAAAAAAAAhkdXJhdGlvbgAAAAAAAAAAAAAGaGVpZ2h0AEByAAAAAAAAAAx2aWRlb2NvZGVjaWQAQBwAAAAAAAAABXdpZHRoAECAAAAAAAAAAAAJ </metadata>
</media>
</manifest>">>,
  {ok, Manifest2} = flu_stream:rewrite_manifest(Manifest1, <<"token1">>),
  ?assertEqual(Manifest1,Manifest2),
  ok.



stop_stream_test_() ->
  {foreach,
  fun() ->
    application:start(gen_tracker),
    application:start(flussonic),
    gen_tracker_sup:start_tracker(flu_streams),
    ok
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(gen_tracker),
    application:stop(flussonic),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end,
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))]
  }.

test_stream_initialized() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream">>,
  {ok, _Pid} = flu_stream:autostart(Stream, [{clients_timeout, 10},{url, "passive://url"}]),
  ?assertMatch({ok, _}, gen_tracker:getattr(flu_streams, Stream, last_access_at)),
  ?assertMatch(undefined, gen_tracker:getattr(flu_streams, Stream, last_dts)),
  ?assertMatch(undefined, gen_tracker:getattr(flu_streams, Stream, last_dts_at)),
  ok.


test_stop_clients_timeout() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{clients_timeout, 10},{url, "passive://url"}]),
  erlang:monitor(process, Pid),
  gen_tracker:setattr(flu_streams, Stream, [{last_access_at, {100,100,100}}]),
  Pid ! check_timeout,
  receive
    {'DOWN', _, _, Pid, normal} -> ok;
    {'DOWN', _, _, Pid, Reason} -> error({stream_died,Reason})
  after
    100 -> error(stream_not_stopped)
  end.




test_no_fail_source_timeout() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  erlang:monitor(process, Pid),
  Pid ! check_timeout,
  receive
    {'DOWN', _, _, Pid, normal} -> error(stream_stopped);
    {'DOWN', _, _, Pid, Reason} -> error({stream_died,Reason})
  after
    100 -> ok
  end.


test_monotone_is_started_with_media_info() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream1">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  ?assertEqual({error, no_child}, flussonic_sup:find_stream_helper(Stream, monotone)),
  Frames = flu_rtmp_tests:h264_aac_frames(),
  [Pid ! F || F <- Frames],
  ?assertEqual(ok, flu_stream:subscribe(Stream)),
  {ok, M} = flussonic_sup:find_stream_helper(Stream, monotone),
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264},#stream_info{codec = aac}]},
    gen_server:call(M, media_info)),
  ok.




test_subscribe_proper_timestamps() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  Self = self(),
  spawn_link(fun() ->
    flu_stream:subscribe(Stream),
    Self ! go
  end),
  receive
    go -> ok
  after 50 -> error(subscribe_timeout) end,
  AllFrames = flu_rtmp_tests:h264_aac_frames(),
  MI = video_frame:define_media_info(undefined, AllFrames),
  gen_server:call(Pid, {set, MI}),

  [Pid ! F || F <- lists:sublist(AllFrames,1,500)],
  ?assertEqual(ok, flu_stream:subscribe(Stream)),
  {ok, M} = flussonic_sup:find_stream_helper(Stream, monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),


  receive
    #video_frame{content = metadata, dts = DTS1} -> ?assertMatch(_ when DTS1 < 5000, DTS1)
  after 500 -> error(no_metadata) end,
  receive 
    #video_frame{content = video, flavor = config, dts = DTS2} -> ?assertMatch(_ when DTS2 < 5000, DTS2)
  after 100 -> error(no_metadata) end,
  ok.




test_monotone_proper_timestamps_in_middle_of_stream() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream1">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  ?assertEqual({error, no_child}, flussonic_sup:find_stream_helper(Stream, monotone)),

  gen_server:call(Pid, {set,flu_test:media_info()}),
  [Pid ! F || F <- flu_test:gop(10)],
  ?assertEqual(ok, flu_stream:subscribe(Stream)),
  {ok, M} = flussonic_sup:find_stream_helper(Stream, monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  [Pid ! F || F <- flu_test:gop(11)],

  receive
    #video_frame{content = video, flavor = Flavor1, dts = DTS1} ->
      ?assertEqual(config, Flavor1),
      ?assertMatch(_ when DTS1 >= 4000, DTS1)
  after 4000 -> error(no_config) end,

  receive
    #video_frame{content = video, flavor = Flavor2, dts = DTS2} -> 
      ?assertEqual(keyframe,Flavor2),
      ?assertMatch(_ when DTS2 >= 4000, DTS2)
  after 100 -> error(no_keyframe) end,

  ok.



test_pushers_are_starting() ->
  ?assertEqual([], flu_stream:list()),
  Stream = <<"livestream1">>,
  {ok, _Pid} = flu_stream:autostart(Stream, [{push,"tshttp://localhost:5670/live2"},publish_enabled,{url, "passive://url"}]),
  ok.















