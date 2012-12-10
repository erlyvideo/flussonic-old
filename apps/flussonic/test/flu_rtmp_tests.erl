-module(flu_rtmp_tests).
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

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
  error_logger:delete_report_handler(error_logger_tty_h),
  stop_all(),
  ok = application:start(ranch),
  ok = application:start(gen_tracker),
  ok = application:start(flussonic),
  ok = application:start(cowboy),
  ok = application:start(rtmp),
  gen_tracker_sup:start_tracker(flu_streams),
  gen_tracker_sup:start_tracker(flu_files),
  rtmp_socket:start_server(1938, test_rtmp_listener1, flu_rtmp),
  ok.  


stop_all() ->
  application:stop(gen_tracker),
  application:stop(flussonic),
  application:stop(rtmp),
  application:stop(cowboy),
  application:stop(ranch),
  ok.

setup_publish() ->
  setup_publish([]).

setup_publish(Options) ->
  init_all(),

  Env = flu_config:get_config(),
  Auth = case proplists:get_value(auth, Options) of
    undefined -> 
      Env1 = lists:keydelete(publish_password, 1, Env),
      application:set_env(flussonic, config, Env1),
      <<>>;
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
  rtmp_lib:publish(RTMP, Stream, <<StreamName/binary, Auth/binary>>),
  #env{rtmp = RTMP, stream=Stream, name = StreamName, env=Env}.


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

publish_with_password_test_() ->
  {setup,
  fun() -> setup_publish([{auth, "l0gin:passw"}]) end,
  fun teardown_publish/1,
  {with,[
    fun(R) -> publish(h264_aac, R) end
  ]}}.
  

h264_aac_frames() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  Frames = read_frames(R, undefined),
  file:close(F),
  Frames.

read_frames(R, Key) ->
  case mp4_reader:read_frame(R, Key) of
    #video_frame{next_id = Next} = F ->
      [F|read_frames(R, Next)];
    eof ->
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
  ?assertThrow({rtmp_error,{play,<<"ort">>}, [403.0|_]},rtmp_lib:play("rtmp://127.0.0.1:1938/live/ort", [{timeout,500}])),
  ok.





rtmp_session_test_() ->
  {foreach, 
  fun() ->
    init_all(),
    Modules = [fake_rtmp, flu_config],
    meck:new(Modules, [{passthrough,true}]),
    meck:expect(fake_rtmp, create_client, fun(Socket) ->
      {ok, Sess} = supervisor:start_child(rtmp_session_sup, [fake_rtmp]),
      rtmp_session:set_socket(Sess, Socket),
      {ok, Sess}
    end),
    meck:expect(fake_rtmp, init, fun(Session) -> {ok, Session} end),
    meck:expect(fake_rtmp, handle_control, fun(_Msg, Session) -> {ok, Session} end),
    meck:expect(fake_rtmp, handle_rtmp_call, fun(Session, #rtmp_funcall{command = Command} = AMF) ->
      fake_rtmp:Command(Session, AMF)
    end),
    meck:expect(fake_rtmp, connect, fun(Session, AMF) -> {unhandled, Session, AMF} end),
    meck:expect(fake_rtmp, createStream, fun(Session, AMF) -> {unhandled, Session, AMF} end),
    {ok, RTMP} = rtmp_socket:start_server(5556,fake_rtmp,fake_rtmp,[]),
    unlink(RTMP),
    {RTMP, Modules}
  end,
  fun({_RTMP, Modules}) ->
    stop_all(),
    meck:unload(Modules)
  end,
  [
    fun test_publish_catch_dvr/0
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
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:5556/livedvr"),
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_lib:publish(RTMP, Stream, <<"stream0">>),
  receive
    {publish, Session, _AMF} ->
      ?assertEqual([{dvr,"movies"}], flu_rtmp:publish_options(Session))
  after
    1000 -> ?assert(false)
  end,
  erlang:exit(RTMP, shutdown),
  ok.




rtmp_file_test_() ->
  {foreach,
  fun setup_file/0,
  fun teardown_publish/1,[
    {"test_play_file", fun test_play_file/0}
    ,{"test_seek_file", fun test_seek_file/0}
  ]}.


test_play_file() ->
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

% flush_rtmp() ->
%   receive
%     A -> ?debugFmt("~240p",[A])
%   end,
%   flush_rtmp().

rtmp_session_auth_test_() ->
  {foreach,
  fun() ->
    init_all(),
    meck:new(fake_auth),
    meck:expect(fake_auth, init, fun(_, Req, _) -> {ok, Req, state} end),
    meck:expect(fake_auth, terminate, fun(_,_) -> ok end),
    Dispatch = [{'_', [{['...'], fake_auth, []}]}],
    {ok, _} = cowboy:start_http(fake_http, 1, [{port, 6071}],
      [{dispatch, Dispatch}]),
    ok
  end,
  fun(_) ->
    meck:unload(fake_auth),
    stop_all() 
  end, [
    {"test_rtmp_play_protected_stream", fun test_rtmp_play_protected_stream/0}
    ,{"test_clients_count_on_rtmp_file", fun test_clients_count_on_rtmp_file/0}
  ]}.


test_rtmp_play_protected_stream() ->
  Self = self(),
  meck:expect(fake_auth, handle, fun(Req, _) ->
    {QsVals, _} = cowboy_req:qs_vals(Req),
    Self ! {backend_request, QsVals},
    {ok, R1} = cowboy_req:reply(200, [], "ok\n", Req),
    {ok, R1, undefined}
  end),
  set_config([{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6071/"}]}]),
  {ok, RTMP, _} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4?token=123",
    [{pageUrl, <<"http://ya.ru/">>}]),


  Qs = receive
    {backend_request, QsV} -> QsV
  after
    100 -> error(backend_wasnt_requested)
  end,
  ?assertEqual(<<"123">>, proplists:get_value(<<"token">>, Qs)),
  ?assertEqual(<<"bunny.mp4">>, proplists:get_value(<<"name">>, Qs)),
  ?assertEqual(<<"http://ya.ru/">>, proplists:get_value(<<"referer">>, Qs)),

  ?assertEqual(<<"rtmp">>, proplists:get_value(<<"type">>, Qs)),


  receive
    {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
    when size(H264) > 20 andalso D1 > 20 -> ok
  after 100 -> error(no_h264) end,

  ok.



test_clients_count_on_rtmp_file() ->
  ?assertEqual([], flu_session:list()),
  meck:expect(fake_auth, handle, fun(Req, _) ->
    {ok, R1} = cowboy_req:reply(200, [], "ok\n", Req),
    {ok, R1, undefined}
  end),
  set_config([{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:6071/"}]}]),
  {ok, RTMP, _} = rtmp_lib:play("rtmp://localhost:1938/vod/bunny.mp4?token=123",
    [{pageUrl, <<"http://ya.ru/">>}]),

  receive
    {rtmp, RTMP, #rtmp_message{type = video, timestamp = D1, body = <<23,1,_/binary>> = H264}}
    when size(H264) > 20 andalso D1 > 20 -> ok
  after 100 -> error(no_h264) end,

  ?assertMatch(Sessions when length(Sessions) == 1, flu_session:list()),
  [Session] = flu_session:list(),
  ?assertEqual(<<"rtmp">>, proplists:get_value(type, Session)),
  % ?assertEqual(<<"http://ya.ru">>, proplists:get_value(referer, Session)),

  ok.



rtmp_source_test_() ->
  {foreach,
  fun setup_source/0,
  fun teardown_source/1, [
    {"test_play_flu_publish_proxy", fun test_play_flu_publish_proxy/0}
  ]}.

setup_source() ->
  init_all(),
  {ok,Pid} = flu_stream:autostart(<<"chan0">>, []),
  Pid ! h264_aac_media_info(),
  ok.

teardown_source(_) ->
  stop_all().


test_play_flu_publish_proxy() ->
  set_config([{rewrite, "chan0", "dev/null"}]),
  {ok, Proxy1} = flu_publish_proxy:init(["rtmp://127.0.0.1:1938/live/chan0", self(), []]),
  {noreply, Proxy2} = flu_publish_proxy:handle_info(init, Proxy1),
  flu_publish_proxy:terminate(normal, Proxy2).












  