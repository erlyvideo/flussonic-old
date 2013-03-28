-module(flu_stream_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").



all() ->
  [
    {group, stream}
  ].


groups() ->
  [
    {stream, [parallel], [
      start_helper,
      restart_helper,
      big_gop,
      double_media_info,
      rewrite_stream_manifest,
      no_rewrite_stream_manifest,
      stream_initialized,
      stop_clients_timeout,
      no_fail_source_timeout,
      monotone_is_started_with_media_info,
      subscribe_proper_timestamps,
      monotone_proper_timestamps_in_middle_of_stream,
      pushers_are_starting
    ]}
  ].


init_per_group(stream, Config) ->
  application:start(flussonic),
  Config;

init_per_group(_, Config) ->
  Config.


end_per_group(stream, Config) ->
  application:stop(flussonic),
  lists:keydelete(group, 1, Config);

end_per_group(_, Config) ->
  Config.





start_helper(_Config) ->
  {ok, S} = flu_stream:autostart(<<"ustream1">>, [{url,<<"passive://">>}]),
  {ok, H} = flu_stream:start_helper(<<"ustream1">>, helper1, {?MODULE, process1, []}),
  {ok, H} = flu_stream:find_helper(<<"ustream1">>, helper1),
  [{helper1, H, worker, []}] = supervisor:which_children(S),
  flu_stream:stop_helper(<<"ustream1">>, helper1),
  undefined = flu_stream:find_helper(<<"ustream1">>, helper1),
  [] = supervisor:which_children(S),
  ok.


process1() ->
  Self = self(),
  Pid = proc_lib:spawn_link(fun() ->
    Self ! ready,
    receive
      stop -> ok;
      bad_stop -> error(bad_stop)
    end
  end),
  receive ready -> ok end,
  {ok, Pid}.



restart_helper(_Config) ->
  {ok, S} = flu_stream:autostart(<<"ustream2">>, [{url,<<"passive://">>}]),
  {ok, H} = flu_stream:start_helper(<<"ustream2">>, helper1, {?MODULE, process1, []}),
  erlang:monitor(process, H),

  H ! stop,
  receive {'DOWN',_,_,H,_} -> ok after 100 -> error(1) end,

  {ok, H1} = flu_stream:find_helper(<<"ustream2">>, helper1),
  H1 =/= H orelse error(not_restarted_helper),
  [{helper1, H1, worker, []}] = supervisor:which_children(S),
  ok.






big_gop(_Config) ->
  {ok,S} = flu_stream:autostart(<<"chan1">>, [{url,"passive://"}]),
  {ok, M} = gen_server:call(S, start_monotone),
  [ok = flu_monotone:send_frame(M, F) || F <- flu_test:gop(1)],
  flu_monotone:stop(M),
  ok.


double_media_info(_Config) ->
  {ok,S} = flu_stream:autostart(<<"chan2">>, [{url,"passive://"}]),
  {ok, M} = gen_server:call(S, start_monotone),

  MI = flu_test:media_info(),
  flu_monotone:send_media_info(M, MI),
  flu_monotone:send_media_info(M, MI),
  flu_monotone:send_media_info(M, MI),
  flu_monotone:send_media_info(M, MI),
  [ok = flu_monotone:send_frame(M, F) || F <- flu_test:gop(1)],
  
  flu_monotone:stop(M),
  ok.



rewrite_stream_manifest(_Config) ->
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
  {_,_} = binary:match(Manifest2, <<"url=\"bootstrap?token=token1\"">>),
  ok.

no_rewrite_stream_manifest(_Config) ->
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
  Manifest1 = Manifest2,
  ok.



stream_initialized(_Config) ->
  Stream = <<"ustream3">>,
  {ok, _Pid} = flu_stream:autostart(Stream, [{clients_timeout, 10},{url, "passive://url"}]),
  {ok, _} = gen_tracker:getattr(flu_streams, Stream, last_access_at),
  undefined = gen_tracker:getattr(flu_streams, Stream, last_dts),
  undefined = gen_tracker:getattr(flu_streams, Stream, last_dts_at),
  ok.




stop_clients_timeout(_Config) ->
  Stream = <<"ustream4">>,
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





no_fail_source_timeout(_Config) ->
  Stream = <<"ustream5">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  erlang:monitor(process, Pid),
  Pid ! check_timeout,
  receive
    {'DOWN', _, _, Pid, normal} -> error(stream_stopped);
    {'DOWN', _, _, Pid, Reason} -> error({stream_died,Reason})
  after
    100 -> ok
  end.




monotone_is_started_with_media_info(_Config) ->
  Stream = <<"ustream6">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  undefined = flu_stream:find_helper(Stream, monotone),
  Pid ! flu_test:media_info(),
  [Pid ! F || F <- flu_test:gop(1)],
  ok = flu_stream:subscribe(Stream),
  {ok, M} = flu_stream:find_helper(Stream, monotone),
  #media_info{streams = [#stream_info{codec = h264},#stream_info{codec = aac}]} = gen_server:call(M, media_info),
  ok.






subscribe_proper_timestamps(_Config) ->
  Stream = <<"ustream7">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  Self = self(),
  spawn_link(fun() ->
    flu_stream:subscribe(Stream),
    Self ! go
  end),
  receive
    go -> ok
  after 50 -> error(subscribe_timeout) end,

  Pid ! flu_test:media_info(),

  [Pid ! F || F <- flu_test:gop(1)],
  ok = flu_stream:subscribe(Stream),
  {ok, M} = flu_stream:find_helper(Stream, monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),


  receive
    #video_frame{content = metadata, dts = DTS1} -> DTS1 < 5000 orelse error({bad_dts1,DTS1})
  after 500 -> error(no_metadata) end,
  receive 
    #video_frame{content = video, flavor = config, dts = DTS2} -> DTS2 < 5000 orelse error({bad_dts2,DTS2})
  after 100 -> error(no_metadata) end,
  ok.






monotone_proper_timestamps_in_middle_of_stream(_Config) ->
  Stream = <<"ustream8">>,
  {ok, Pid} = flu_stream:autostart(Stream, [{source_timeout, false},{url, "passive://url"}]),
  undefined = flu_stream:find_helper(Stream, monotone),

  gen_server:call(Pid, {set,flu_test:media_info()}),
  [Pid ! F || F <- flu_test:gop(10)],
  ok = flu_stream:subscribe(Stream),
  {ok, M} = flu_stream:find_helper(Stream, monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  [Pid ! F || F <- flu_test:gop(11)],

  receive
    #video_frame{content = video, flavor = Flavor1, dts = DTS1} ->
      config = Flavor1,
      DTS1 >= 4000 orelse error({bad_dts1,DTS1})
  after 4000 -> error(no_config) end,

  receive
    #video_frame{content = video, flavor = Flavor2, dts = DTS2} -> 
      keyframe = Flavor2,
      DTS2 >= 4000 orelse error({bad_dts2,DTS2})
  after 100 -> error(no_keyframe) end,

  ok.






pushers_are_starting(_Config) ->
  Stream = <<"ustream9">>,
  {ok, _Pid} = flu_stream:autostart(Stream, [{push,"tshttp://localhost:5670/live2"},publish_enabled,{url, "passive://url"}]),
  ok.


