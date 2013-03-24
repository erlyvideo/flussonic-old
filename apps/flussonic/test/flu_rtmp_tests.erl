-module(flu_rtmp_tests).
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).




play_stream_test_() ->
  {setup, flu_test:setup_([{apps,[rtmp]}],fun() ->
    % flu_test:set_config([{stream, <<"ustream">>, "passive://", []}])
    ok
  end),
  flu_test:teardown_(),
  [{foreach, fun() ->
    [flussonic_sup:stop_stream(Name) || {Name,_} <- flu_stream:list()]
  end, fun(_) -> ok end,[
  % {"test_stream_is_starting_properly", fun test_stream_is_starting_properly/0}
  {"playtest_live_stream", fun playtest_live_stream/0}
  ,{"playtest_static_stream", fun playtest_static_stream/0}
  ,{"playtest_autostart_rewrite_stream", fun playtest_autostart_rewrite_stream/0}
  ,{"test_play_file", fun test_play_file/0}
  ,{"test_seek_file", fun test_seek_file/0}
  ]}]}.


% test_stream_is_starting_properly() ->
%   Stream = <<"ustream">>,
%   {ok, Pid} = flu_stream:find(Stream),
%   {ok, M} = flussonic_sup:find_stream_helper(Stream, monotone),
%   gen_server:call(M, {set_start_at,{0,0,0}}),
%   [Pid ! F || F <- Frames1],
%   _MI = gen_server:call(Pid, {get, media_info}),
%   ok.



playtest_live_stream() ->
  ?assertEqual([], flu_stream:list()),
  flu_test:set_config([{rtmp,1938},{live,"live"}]),
  {ok, Pid} = flu_stream:autostart(<<"live/ustream">>),
  {ok, M} = gen_server:call(Pid, start_monotone),

  ?assertMatch([{<<"live/ustream">>, _}], flu_stream:list()),

  AllFrames = h264_aac_frames(),
  gen_server:call(M, {set_start_at,{0,0,0}}),

  Frames1 = lists:sublist(AllFrames,1,400),
  [Pid ! F || F <- Frames1],
  gen_server:call(Pid, {get, media_info}),


  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/live/ustream"),
  rtmp_socket:setopts(RTMP, [{active,true}]),
  ?assertMatch([{<<"live/ustream">>, _}], flu_stream:list()),

  receive {rtmp,RTMP,#rtmp_message{type = stream_begin, stream_id = 1}} -> ok after 10 -> error(1) end,

  Start = receive {rtmp,RTMP,#rtmp_message{type = metadata, stream_id = 1, body = [<<"onStatus">>|_]} = M1} -> M1 after 10 -> error(2) end,
  ?assertMatch(#rtmp_message{timestamp = 0}, Start),

  receive {rtmp,RTMP,#rtmp_message{type=audio,body = <<>>,timestamp = 0}} -> ok after 10 -> error(3) end,
  receive {rtmp,RTMP,#rtmp_message{type=video,body = <<87,0>>,timestamp = 0}} -> ok after 100 -> error(4) end,
  flush_rtmp_messages(RTMP),


  Frames2 = lists:sublist(AllFrames,400,200),
  [Pid ! F || F <- Frames2],
  gen_server:call(Pid, {get, media_info}),

  Meta = receive 
    {rtmp,RTMP,#rtmp_message{type = metadata, stream_id = 1, body = [<<"onMetaData">>|_]} = M2} -> M2 
  after 
    100 -> error(4) 
  end,
  ?assertMatch(#rtmp_message{timestamp = 0}, Meta),

  receive {rtmp,RTMP,#rtmp_message{type=video,body = <<23,0,_/binary>>,timestamp = 0}} -> ok after 10 -> error(5) end,
  receive {rtmp,RTMP,#rtmp_message{type=audio,timestamp = 0}} -> ok after 10 -> error(6) end,

  % rtmp_socket:close(RTMP),
  erlang:exit(Pid,shutdown),
  ok.

flush_rtmp_messages(RTMP) ->
  receive
    {rtmp,RTMP,#rtmp_message{}} -> flush_rtmp_messages(RTMP)
  after
    0 -> ok
  end.

playtest_static_stream() ->
  ?assertEqual([], flu_stream:list()),
  flu_test:set_config([{rtmp,1938},{stream,"mystream", "passive://ok"}]),
  {ok, Pid} = flu_stream:autostart(<<"mystream">>),
  {ok, M} = gen_server:call(Pid, start_monotone),
  Pid ! flu_test:media_info(),

  gen_server:call(M, {set_start_at,{0,0,0}}),
  [Pid ! Frame || Frame <- flu_test:gop(1)],

  ?assertMatch([{<<"mystream">>, _}], flu_stream:list()),

  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/live/mystream"),
  rtmp_lib:sync_call(RTMP, 0, flu_stats, []),

  [Pid ! Frame || Frame <- flu_test:gop(2)],

  rtmp_socket:setopts(RTMP, [{active,true}]),
  receive
    {rtmp, RTMP, #rtmp_message{type = video, body = Body}} when size(Body) > 40 -> ok
  after
    100 -> error(no_video)
  end,
  ?assertMatch([{<<"mystream">>, _}], flu_stream:list()),
  rtmp_socket:close(RTMP),
  erlang:exit(Pid,shutdown),
  ok.


wait_for_stream(Stream, 0) -> error({not_started,Stream});
wait_for_stream(Stream, Count) ->
  case flu_stream:find(Stream) of
    {ok, Pid} -> Pid;
    undefined -> timer:sleep(10), wait_for_stream(Stream, Count - 1)
  end.

playtest_autostart_rewrite_stream() ->
  ?assertEqual([], flu_stream:list()),
  flu_test:set_config([{rtmp,1938},{rewrite,"mystream", "passive://ok"}]),
  W = spawn(fun() ->
    Pid = wait_for_stream(<<"mystream">>, 10),
    Pid ! flu_test:media_info(),
    {ok, M} = gen_server:call(Pid, start_monotone),
    gen_server:call(M, {set_start_at,{0,0,0}}),
    [Pid ! Frame || Frame <- flu_test:gop(1)],
    [Pid ! Frame || Frame <- flu_test:gop(2)],
    receive _ -> ok after 100 -> ok end,
    [Pid ! Frame || Frame <- flu_test:gop(3)],
    ok
  end),

  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/rtmp/mystream"),
  rtmp_lib:sync_call(RTMP, 0, flu_stats, []),
  rtmp_socket:setopts(RTMP, [{active,true}]),
  receive
    {rtmp, _, #rtmp_message{type = video, body = Body}} when size(Body) > 40 -> ok
  after
    500 -> error(no_video)
  end,
  ?assertMatch([{<<"mystream">>, _}], flu_stream:list()),
  W ! next,
  rtmp_socket:close(RTMP),
  {ok, Pid} = flu_stream:find(<<"mystream">>),
  erlang:exit(Pid, shutdown),
  ok.





rtmp_file_test_() ->
  {foreach,
  fun setup_file/0,
  fun teardown_publish/1,[
    % {"test_play_file", fun test_play_file/0}
    % ,{"test_seek_file", fun test_seek_file/0}
  ]}.


test_play_file() ->
  ?assertEqual([], flu_session:list()),
  flu_test:set_config([{rtmp,1938},{file,"vod","../../../priv"}]),

  {ok, RTMP, _Stream} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4"),
  receive
    {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
    when size(H264) > 20 andalso D1 > 20 -> ok
  after 100 -> error(no_h264) end,
  receive
    {rtmp, RTMP, #rtmp_message{type = audio, body = AAC, timestamp = D2}} 
    when size(AAC) > 20 andalso D2 > 20 -> ok
  after 100 -> error(no_aac) end,
  % flush_rtmp(),
  [Session] = flu_session:list(),
  ?assertEqual(<<"rtmp">>, proplists:get_value(type, Session)),
  Bytes = proplists:get_value(bytes, Session),
  ?assertMatch(Bytes when Bytes > 0, Bytes),
  ok.

test_seek_file() ->
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
  % flush_rtmp(),
  ok.




% test_clients_count_on_rtmp_file() ->
%   ?assertEqual([], flu_session:list()),
%   meck:expect(fake_auth, reply, fun(_Req) ->
%     {200, [], "ok\n"}
%   end),
%   set_config([{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6071/"}]}]),
%   {ok, RTMP, _} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4?token=123",
%     [{pageUrl, <<"http://ya.ru/">>}]),

%   receive
%     {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
%     when size(H264) > 20 andalso D1 > 20 -> ok
%   after 100 -> error(no_h264) end,

%   ?assertMatch(Sessions when length(Sessions) == 1, flu_session:list()),
%   [Session] = flu_session:list(),
%   ?assertEqual(<<"rtmp">>, proplists:get_value(type, Session)),
%   % ?assertEqual(<<"http://ya.ru">>, proplists:get_value(referer, Session)),

%   ok.




% Everything below is based on old infrastructure, not flu_test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










































-record(env, {
  rtmp,
  stream,
  name,
  env
}).

h264_config() ->
  Body = <<1,66,192,21,253,225,0,23,103,66,192,21,146,68,15,4,127,88,8,128,0,1,244,0,0,97,161,71,139,23,80,1,0,4,104,206,50,200>>,
  #video_frame{content = video, codec = h264, flavor = config, track_id = 100, dts = 0, pts = 0, body = Body}.

aac_config() ->
  #video_frame{content = audio, codec = aac, flavor = config, track_id = 101, dts = 0, pts = 0, body = <<17,144>>}.

mp3_frame(DTS) ->
  Body = <<255,243,96,192,88,25,185,54,65,108,98,70,144,79,60,68,175,98,168,136,90,227,131,22,229,33,196,150,106,173,87,234,
  183,134,115,54,144,195,76,253,120,92,98,101,80,198,219,49,177,8,48,201,133,91,67,134,158,251,67,143,147,252,165,147,93,218,
  115,120,209,73,233,205,170,35,181,225,226,245,242,76,127,226,238,230,91,188,55,201,203,126,116,145,201,172,68,170,72,136,
  10,164,55,1,135,133,168,38,24,54,86,182,159,66,15,60,54,72,99,146,116,64,57,225,13,98,198,92,137,25,39,138,227,6,179,90,39,
  238,99,169,233,108,194,204,56,234,215,204,0,105,165,91,226,183,97,36,40,145>>,
  #video_frame{content = audio, codec = mp3, flavor = frame, dts = DTS, pts = DTS, body = Body}.


init_all() ->
  application:start(ranch),
  application:start(crypto),
  ok = application:start(flussonic),
  application:start(cowboy),
  application:start(rtmp),
  application:start(public_key),
  application:start(pulse),
  application:start(ssl),
  application:start(lhttpc),
  rtmp_socket:start_server(1938, test_rtmp_listener1, flu_rtmp),
  ok.  


stop_all() ->
  error_logger:delete_report_handler(error_logger_tty_h),
  application:stop(flussonic),
  application:stop(pulse),
  application:stop(rtmp),
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(crypto),
  application:stop(inets),
  application:stop(lhttpc),
  application:stop(ssl),
  application:stop(public_key),
  error_logger:add_report_handler(error_logger_tty_h),
  ok.

setup_publish() ->
  setup_publish([]).

setup_publish(Options) ->
  init_all(),

  set_config([{http,9090},{live,"live"},{live, "secure", [{password, "passw0rt"}]}]),
  Env = flu_config:get_config(),
  Auth = case proplists:get_value(auth, Options) of
    undefined -> 
      Env1 = lists:keydelete(publish_password, 1, Env),
      application:set_env(flussonic, config, Env1),
      case proplists:get_value(password, Options) of
        undefined -> <<>>;
        Password -> <<"?password=", (list_to_binary(Password))/binary>>
      end;
    A ->
      [Login,Password] = string:tokens(A, ":"),
      Env1 = lists:keystore(publish_password, 1, Env, {publish_password, A}),
      application:set_env(flussonic, config, Env1),
      <<"?login=", (list_to_binary(Login))/binary, "&password=", (list_to_binary(Password))/binary>>
  end,
  StreamName = iolist_to_binary([<<"testlivecam">>, integer_to_list(random:uniform(100))]),

  % flussonic_sup:stop_stream(StreamName),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/live"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<StreamName/binary, Auth/binary>>),
  #env{rtmp = RTMP, stream=Stream, name = <<"live/", StreamName/binary>>, env=Env}.


set_config(Env) ->
  {ok, Conf} = flu_config:parse_config(Env,undefined),
  application:set_env(flussonic, config, Conf),
  ok.


setup_file() ->
  init_all(),
  set_config([{file, "vod", "../../../priv"}]),
  % {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/live"),
  % Stream = rtmp_lib:createStream(RTMP),
  % #env{rtmp = RTMP, stream=Stream}.
  #env{}.


teardown_publish(#env{}) ->
  % rtmp_socket:close(RTMP),
  stop_all(),
  ok.



flu_rtmp_test_() ->
  {foreach, 
  fun setup_publish/0,
  fun teardown_publish/1,
  [ {with, [fun(R) -> publish(h264_aac, R) end]},
    {with, [fun(R) -> publish(h264_mp3, R) end]}
  ]}.

publish_with_global_password_test_() ->
  {setup,
  fun() -> setup_publish([{auth, "l0gin:passw"}]) end,
  fun teardown_publish/1,
  {with,[
    fun(R) -> publish(h264_aac, R) end
  ]}}.
  
publish_with_stream_level_password_test_() ->
  {foreach,
  fun() -> init_all() end,
  fun(_) -> stop_all() end,
  [
    {"test_stream_level_password_publish_ok", fun test_stream_level_password_publish_ok/0}
    ,{"test_stream_level_password_publish_rejected", fun test_stream_level_password_publish_rejected/0}
    % ,{"test_stream_level_password_publish_and_sessions", fun test_stream_level_password_publish_and_sessions/0}
  ]
  }.

test_stream_level_password_publish_ok() ->
  set_config([{live,"live"},{live, "secure", [{password, "passw0rt"}]}]),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/secure"),
  Stream = rtmp_lib:createStream(RTMP),
  Result = rtmp_lib:publish(RTMP, Stream, <<"teststream1?password=passw0rt">>),
  ?assertEqual(ok, Result),
  ok.

test_stream_level_password_publish_rejected() ->
  set_config([{live,"live"},{live, "secure", [{password, "passw0rt"}]}]),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/secure"),
  Stream = rtmp_lib:createStream(RTMP),
  Result = rtmp_lib:publish(RTMP, Stream, <<"teststream1">>),
  ?assertMatch({rtmp_error, _, _}, Result),
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
%   [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- Frames],

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



h264_aac_frames() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  Configs = video_frame:config_frames(mp4_reader:media_info(R)),
  Frames = Configs ++ read_frames(R, 1),
  file:close(F),
  Frames.

read_frames(R, N) ->
  case mp4_reader:read_gop(R, N) of
    {ok, Gop} ->
      Gop ++ read_frames(R, N+1);
    {error, _} ->
      []
  end.


h264_aac_media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  MI = mp4_reader:media_info(R),
  file:close(F),
  MI.

publish(h264_aac, #env{rtmp= RTMP, name = StreamName, stream=Stream}) ->
  [rtmp_publish:send_frame(RTMP, Stream, Frame) || Frame <- h264_aac_frames()],
  
  timer:sleep(300),
  
  MediaInfo = flu_stream:media_info(StreamName),
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264}, #stream_info{codec = aac}]}, MediaInfo),
  ok;

publish(h264_mp3, #env{rtmp= RTMP, name = StreamName, stream=Stream}) ->
  rtmp_publish:send_frame(RTMP, Stream, h264_config()),

  rtmp_publish:send_frame(RTMP, Stream, #video_frame{content = video, codec = h264, flavor = keyframe, dts = 0, pts = 0, body = <<"keyframe">>}),
  rtmp_publish:send_frame(RTMP, Stream, mp3_frame(0)),
  rtmp_publish:send_frame(RTMP, Stream, mp3_frame(23)),
  rtmp_publish:send_frame(RTMP, Stream, #video_frame{content = video, codec = h264, flavor = frame, dts = 40, pts = 40, body = <<"frame">>}),
  rtmp_publish:send_frame(RTMP, Stream, mp3_frame(46)),
  rtmp_publish:send_frame(RTMP, Stream, #video_frame{content = video, codec = h264, flavor = frame, dts = 80, pts = 80, body = <<"frame">>}),
  rtmp_publish:send_frame(RTMP, Stream, mp3_frame(69)),
  rtmp_publish:send_frame(RTMP, Stream, mp3_frame(92)),
  rtmp_publish:send_frame(RTMP, Stream, #video_frame{content = video, codec = h264, flavor = frame, dts = 120, pts = 120, body = <<"frame">>}),
 
  timer:sleep(300),

  MediaInfo = flu_stream:media_info(StreamName),
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264}, #stream_info{codec = mp3}]}, MediaInfo),
  ok.


  
run(Suite) ->
  try run0(Suite) of
    R -> R
  catch
    Class:Error -> 
      ?debugFmt("~s:~p ~240p~n", [Class, Error, erlang:get_stacktrace()])
  end.

run0(Suite) ->
  R = setup_publish(),
  publish(Suite, R),
  teardown_publish(R).


play_rejected_test_() ->
  {foreach, fun() ->
    init_all(),
    Modules = [flu_session],
    meck:new(Modules, [{passthrough,true}]),
    Modules
  end,
  fun(Modules) ->
    meck:unload(Modules),
    ok
  end,
  [
    {"test_forbidden_password", fun test_forbidden_password/0}
  ]}.

test_forbidden_password() ->
  meck:expect(flu_session, timeout, fun() -> 200 end),
  application:set_env(flussonic, config, [{stream, <<"ort">>, <<"null">>, [{sessions, "http://127.0.0.5/"}]}]),
  ?assertMatch({error,[403.0|_]}, rtmp_lib:play("rtmp://127.0.0.1:1938/live/ort", [{timeout,500}])),
  ok.





rtmp_session_test_() ->
  {foreach, 
  fun() ->
    init_all(),
    Modules = [fake_rtmp, flu_config],
    meck:new(Modules, [{passthrough,true}]),
    RTMP = rtmp,
    {RTMP, Modules}
  end,
  fun({_RTMP, Modules}) ->
    stop_all(),
    meck:unload(Modules)
  end,
  [case code:load_file(dvr) of
    {error, nofile} -> [];
    _ -> [{"test_publish_catch_dvr", fun test_publish_catch_dvr/0}]
    end ++
    [{"test_refuse_non_application_publish", fun test_refuse_non_application_publish/0}]
  ]}.



test_publish_catch_dvr() ->
  meck:expect(flu_config, get_config, fun() -> [{live, <<"livedvr">>, [{dvr, "movies"}]}] end),
  Self = self(),
  meck:expect(fake_rtmp, publish, fun(Session, #rtmp_funcall{stream_id = StreamId, args = [null, Name|_]} = AMF) ->
    Socket = rtmp_session:get(Session, socket),
    rtmp_lib:notify_publish_start(Socket, StreamId, 0, Name),
    Self ! {publish, Session, AMF},
    Session 
  end),
  ?assertEqual(undefined, flu_stream:find(<<"livedvr/stream0">>)),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/livedvr"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"stream0">>),

  ?assertMatch({ok, Pid} when is_pid(Pid), flu_stream:find(<<"livedvr/stream0">>)),
  [{<<"livedvr/stream0">>, Attrs}] = flu_stream:list(),
  ?assertEqual(true, proplists:get_value(dvr, Attrs)),

  rtmp_socket:close(RTMP),
  ok.


test_refuse_non_application_publish() ->
  meck:expect(flu_config, get_config, fun() -> [{live, <<"live">>, []}] end),
  Self = self(),
  meck:expect(fake_rtmp, publish, fun(Session, #rtmp_funcall{stream_id = StreamId, args = [null, Name|_]} = AMF) ->
    Socket = rtmp_session:get(Session, socket),
    rtmp_lib:notify_publish_start(Socket, StreamId, 0, Name),
    Self ! {publish, Session, AMF},
    Session 
  end),
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1938/livedvr"),
  Stream = rtmp_lib:createStream(RTMP),
  ?assertMatch({rtmp_error, {publish,<<"stream0">>}, _}, rtmp_lib:publish(RTMP, Stream, <<"stream0">>)),
  erlang:exit(RTMP, shutdown),
  ok.


% flush_rtmp() ->
%   receive
%     A -> ?debugFmt("~240p",[A])
%   end,
%   flush_rtmp().

rtmp_session_auth_test_() ->
  {foreach,
  fun() ->
    init_all(),
    meck:new(fake_auth, [{passthrough,true}]),
    Dispatch = [{'_', [{"/[...]", fake_auth, []}]}],
    {ok, _} = cowboy:start_http(fake_http, 1, [{port, 6071}],
      [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}] ),
    ok
  end,
  fun(_) ->
    meck:unload(fake_auth),
    stop_all() 
  end, [
    % {"test_rtmp_play_protected_stream", fun test_rtmp_play_protected_stream/0}
    % ,{"test_clients_count_on_rtmp_file", fun test_clients_count_on_rtmp_file/0}
  ]}.


% FIXME
% test_rtmp_play_protected_stream() ->
%   Self = self(),
%   meck:expect(fake_auth, reply, fun(Req) ->
%     {QsVals, _} = cowboy_req:qs_vals(Req),
%     Self ! {backend_request, QsVals},
%     {200, [], "ok\n"}
%   end),
%   set_config([{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6071/"}]}]),
%   {ok, RTMP, _} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4?token=123",
%     [{pageUrl, <<"http://ya.ru/">>}]),


%   Qs = receive
%     {backend_request, QsV} -> QsV
%   after
%     100 -> error(backend_wasnt_requested)
%   end,
%   ?assertEqual(<<"123">>, proplists:get_value(<<"token">>, Qs)),
%   ?assertEqual(<<"bunny.mp4">>, proplists:get_value(<<"name">>, Qs)),
%   ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(<<"referer">>, Qs)),

%   ?assertEqual(<<"rtmp">>, proplists:get_value(<<"type">>, Qs)),


%   receive
%     {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
%     when size(H264) > 20 andalso D1 > 20 -> ok
%   after 100 -> error(no_h264) end,

%   ok.







  