-module(flu_rtmp_SUITE).
-compile(export_all).


-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("common_test/include/ct.hrl").
-include("../src/flu_event.hrl").

all() ->
  [
    {group, rtmp}
  ].


groups() ->
  [
    {rtmp, [parallel,shuffle], [
      play_static_stream,
      play_live_stream,
      play_autostart_rewrite_stream,
      play_file,
      seek_file,
      publish_stream_level_password_ok,
      publish_stream_level_password_rejected,
      publish_bad_app_rejected,
      publish_dvr,
      record
    ]}
  ].


init_per_suite(Config) ->
  R = flu_test:setup([{log,info},{apps,[rtmp]}]),
  flu_test:set_config([
    {rtmp,1938},
    {live,"live"},
    {stream,"mystream", "passive://ok", [{sessions, "http://127.0.0.1:5671/auth/token_unique_number"}]},
    {rewrite,"ondemand", "passive://ok"},
    {live, "secure", [{password, "passw0rt"}]},
    {live, <<"livedvr">>, [{dvr, "dvr2:movies"}]},
    {live, <<"records">>, [{path, "flv"},{source_timeout,1},{retry_limit,0},{clients_timeout,1}]},
    {file,"vod","../../priv"}
  ]),

  {ok, Static} = flu_stream:autostart(<<"mystream">>),
  Static ! flu_test:media_info(),
  [Static ! Frame || Frame <- flu_test:gop(1)],
  gen_server:call(Static, {get,media_info}),

  [{r,R}|Config].


end_per_suite(Config) ->
  {value,{r,R},Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1.





play_static_stream(_Config) ->
  {ok, S} = flu_stream:find(<<"mystream">>),

  {error, [403.0|_]} = rtmp_lib:play("rtmp://localhost:1938/rtmp/mystream"),

  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/rtmp/mystream?token=15"),
  rtmp_lib:sync_call(RTMP, 0, flu_stats, []),

  [S ! Frame || Frame <- flu_test:gop(2)],

  rtmp_socket:setopts(RTMP, [{active,true}]),
  receive
    {rtmp, RTMP, #rtmp_message{type = video, body = Body}} when size(Body) > 40 -> ok
  after
    100 -> error(no_video)
  end,
  rtmp_socket:close(RTMP),
  ok.



play_auth_rejected(_Config) ->
  ok.






play_live_stream(_Config) ->
  {ok, Pid} = flu_stream:autostart(<<"live/ustream">>),
  {ok, M} = gen_server:call(Pid, start_monotone),

  % AllFrames = h264_aac_frames(),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  % Frames1 = lists:sublist(AllFrames,1,400),
  Pid ! flu_test:media_info(),
  [Pid ! F || F <- flu_test:gop(1)],
  gen_server:call(Pid, {get, media_info}),


  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/live/ustream"),
  rtmp_socket:setopts(RTMP, [{active,true}]),

  receive {rtmp,RTMP,#rtmp_message{type = stream_begin, stream_id = 1}} -> ok after 10 -> error(1) end,

  Start = receive {rtmp,RTMP,#rtmp_message{type = metadata, stream_id = 1, body = [<<"onStatus">>|_]} = M1} -> M1 after 10 -> error(2) end,
  #rtmp_message{timestamp = 0} = Start,

  Meta = receive 
    {rtmp,RTMP,#rtmp_message{type = metadata, stream_id = 1, body = [<<"onMetaData">>|_]} = M2} -> M2 
  after 
    100 -> error(4) 
  end,
  #rtmp_message{timestamp = 0} = Meta,


  receive {rtmp,RTMP,#rtmp_message{type=audio,body = <<>>,timestamp = 0}} -> ok after 100 -> error(3) end,
  receive {rtmp,RTMP,#rtmp_message{type=audio,timestamp = 0,body = Body}} when size(Body) > 10 -> ok after 100 -> error(6) end,
  receive {rtmp,RTMP,#rtmp_message{type=video,body = <<87,0>>,timestamp = 0}} -> ok after 100 -> error(4) end,
  receive {rtmp,RTMP,#rtmp_message{type=video,body = <<23,0,_/binary>>,timestamp = 0}} -> ok after 100 -> error(5) end,
  flush_rtmp_messages(RTMP),

  [Pid ! F || F <- flu_test:gop(2)],
  gen_server:call(Pid, {get, media_info}),

  rtmp_socket:close(RTMP),
  ok.








play_autostart_rewrite_stream(_Config) ->
  W = spawn(fun() ->
    Pid = wait_for_stream(<<"ondemand">>, 100),
    timer:sleep(1000),
    io:format("pwd: ~p\n", [os:cmd("pwd")]),
    Pid ! flu_test:media_info(),
    % {ok, M} = gen_server:call(Pid, start_monotone),
    % gen_server:call(M, {set_start_at,{0,0,0}}),
    [Pid ! Frame || Frame <- flu_test:gop(1)],
    [Pid ! Frame || Frame <- flu_test:gop(2)],
    receive _ -> ok after 100 -> ok end,
    [Pid ! Frame || Frame <- flu_test:gop(3)],
    ok
  end),

  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/rtmp/ondemand"),
  rtmp_lib:sync_call(RTMP, 0, flu_stats, []),
  rtmp_socket:setopts(RTMP, [{active,true}]),
  receive
    {rtmp, _, #rtmp_message{type = video, body = Body}} when size(Body) > 40 -> ok
  after
    500 -> error(no_video)
  end,

  SessionId = receive
    {rtmp, RTMP, #rtmp_message{type = metadata, body = [<<"|SessionId">>, Ssid]}} -> round(Ssid)
  after 100 -> error(no_session_id) end,

  [Session] = [S || S <- flu_session:list(), proplists:get_value(id,S) == SessionId],
  <<"rtmp">> = proplists:get_value(type, Session),


  W ! next,
  rtmp_socket:close(RTMP),
  ok.






wait_for_stream(Stream, 0) -> error({not_started,Stream});
wait_for_stream(Stream, Count) ->
  case flu_stream:find(Stream) of
    {ok, Pid} -> Pid;
    undefined -> timer:sleep(10), wait_for_stream(Stream, Count - 1)
  end.








flush_rtmp_messages(RTMP) ->
  receive
    {rtmp,RTMP,#rtmp_message{}} -> flush_rtmp_messages(RTMP)
  after
    0 -> ok
  end.



play_file(_Config) ->
  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4"),
  SessionId = receive
    {rtmp, RTMP, #rtmp_message{type = metadata, body = [<<"|SessionId">>, Ssid]}} -> round(Ssid)
  after 100 -> error(no_session_id) end,
  receive
    {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
    when size(H264) > 20 andalso D1 > 20 -> ok
  after 100 -> error(no_h264) end,
  receive
    {rtmp, RTMP, #rtmp_message{type = audio, body = AAC, timestamp = D2}} 
    when size(AAC) > 20 andalso D2 > 20 -> ok
  after 100 -> error(no_aac) end,

  [Session] = [S || S <- flu_session:list(), proplists:get_value(id,S) == SessionId],
  <<"rtmp">> = proplists:get_value(type, Session),
  rtmp_socket:close(RTMP),
  ok.






seek_file(_Config) ->
  {ok, RTMP, Stream} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4"),
  rtmp_lib:seek(RTMP, Stream, 30000),
  receive
    {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
    when size(H264) > 20 andalso D1 > 30020 -> ok
  after 100 -> error(havent_seeked_h264) end,
  receive
    {rtmp, RTMP, #rtmp_message{type = audio, body = AAC, timestamp = D2}} 
    when size(AAC) > 20 andalso D2 > 30020 -> ok
  after 100 -> error(havent_seeked_aac) end,
  rtmp_socket:close(RTMP),
  ok.







publish_stream_level_password_ok(_Config) ->
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/secure"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"stream_ok?password=passw0rt">>),
  {ok, _} = flu_stream:find(<<"secure/stream_ok">>),
  rtmp_socket:close(RTMP),
  ok.



publish_stream_level_password_rejected(_Config) ->
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/secure"),
  Stream = rtmp_lib:createStream(RTMP),
  {rtmp_error, _, _} = rtmp_lib:publish(RTMP, Stream, <<"stream_rejected">>),
  undefined = flu_stream:find(<<"secure/stream_rejected">>),
  rtmp_socket:close(RTMP),
  ok.


publish_bad_app_rejected(_Config) ->
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/badapp"),
  Stream = rtmp_lib:createStream(RTMP),
  {rtmp_error, _, _} = rtmp_lib:publish(RTMP, Stream, <<"stream_rejected">>),
  rtmp_socket:close(RTMP),
  ok.




publish_dvr(_Config) ->
  undefined = flu_stream:find(<<"livedvr/stream0">>),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/livedvr"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"stream0">>),

  {ok, _Pid} = flu_stream:find(<<"livedvr/stream0">>),
  {<<"livedvr/stream0">>, Attrs} = lists:keyfind(<<"livedvr/stream0">>, 1, flu_stream:list()),
  true = proplists:get_value(dvr, Attrs),

  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- video_frame:config_frames(flu_test:media_info())],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(1)],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(2)],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(3)],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(4)],

  {ok, {{200, _}, _Headers, _Manifest}} = flu_test:request("http://127.0.0.1:5670/livedvr/stream0/manifest.f4m", get, [], 1000),
  {ok, {{200, _}, _, _Segment1}} = flu_test:request("http://127.0.0.1:5670/livedvr/stream0/hds/0/Seg0-Frag1",get,[],1000),

  rtmp_socket:close(RTMP),
  ok.


record(_Config) ->
  undefined = flu_stream:find("records/rec1"),

  flu_event:subscribe_to_events(self()),
  file:delete("flv/rec1.flv"),
  {error, enoent} = file:read_file_info("flv/rec1.flv"),

  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/records"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"rec1">>, record),

  receive #flu_event{stream = <<"records/rec1">>, event = 'stream.started'} -> ok after 10 -> error(not_notified_start) end,

  {ok, Pid} = flu_stream:find(<<"records/rec1">>),
  erlang:monitor(process, Pid),
  {<<"records/rec1">>, _Attrs} = lists:keyfind(<<"records/rec1">>, 1, flu_stream:list()),

  {ok, _} = file:read_file_info("flv/rec1.flv"),

  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- video_frame:config_frames(flu_test:media_info())],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(1)],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(2)],
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- flu_test:gop(3)],
  rtmp_socket:getopts(RTMP, []),


  {ok, P} = flu_stream:find_helper(<<"records/rec1">>, publish_proxy),
  gen_server:call(P, sync),


  Pid ! reconnect_source,
  Pid ! check_timeout,
  try gen_server:call(Pid, {get, media_info})
  catch
    exit:{normal,_} -> ok
  end,
  rtmp_socket:close(RTMP),
  receive #flu_event{stream = <<"records/rec1">>, event = 'stream.stopped'} -> ok after 1000 -> error(not_notified_stop) end,


  {ok, Bin} = file:read_file("flv/rec1.flv"),
  Frames = flv:read_all_frames(Bin),
  length(Frames) > 40 orelse error({{too_few_frames,length(Frames)}, error}),

  undefined = flu_stream:find(<<"records/rec1">>),

  {ok, RTMP2, _Stream} = rtmp_lib:play("rtmp://localhost:1938/records/rec1"),
  rtmp_socket:close(RTMP2),
  ok.



% FIXME
% test_stream_level_password_publish_and_sessions() ->
%   fake_auth:start_http(),
%   meck:new(fake_auth,[{passthrough,true}]),
%   Self = self(),
%   meck:expect(fake_auth, reply, fun(Req) ->
%     {QsVals, _} = cowboy_req:qs_vals(Req),
%     % ?debugFmt("qs_vals: ~p", [QsVals]),
%     Self ! {backend_request, QsVals},
%     {200,[{<<"X-UserId">>,<<"15">>},{<<"X-AuthDuration">>, <<"5">>}], <<"">>} 
%   end),

%   Conf = [{live,"live"},{live, "secure", [{password, "passw0rt"},{sessions,"http://localhost:6070/auth"}]}],
%   {ok, Cnf} = flu_config:parse_config(Conf, undefined),
%   flu:start_webserver([{http,9090}|Cnf]),


%   set_config(Conf),
%   {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/secure"),
%   Stream = rtmp_lib:createStream(RTMP),
%   Result = rtmp_lib:publish(RTMP, Stream, <<"teststream1?password=passw0rt">>),
%   ?assertEqual(ok, Result),

%   Frames = h264_aac_frames(),

%   ManifestReply = lhttpc:request("http://127.0.0.1:9090/secure/teststream1/manifest.f4m?token=mytoken", "GET", [], 10000),
%   ?assertMatch({ok, {{200, _}, _Headers, _Manifest}}, ManifestReply),
%   % {ok, {{200, _}, _Headers, Manifest}} = Result,

%   Qs = receive
%     {backend_request, Qs_} -> Qs_
%   after
%     50 -> error(timeout_backend)
%   end,

%   % ?debugFmt("qs: ~p", [Qs]),
%   ?assertEqual(<<"hds">>, proplists:get_value(<<"type">>, Qs)),
%   ?assertEqual(<<"secure/teststream1">>, proplists:get_value(<<"name">>, Qs)),


%   fake_auth:stop_http(),
%   ok.





