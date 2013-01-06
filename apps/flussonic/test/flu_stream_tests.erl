-module(flu_stream_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


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





















