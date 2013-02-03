-module(mpegts_handler_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").



mpegts_test_() ->
  {foreach,
  fun() ->
    Apps = [crypto, ranch, gen_tracker, cowboy, flussonic],
    [application:start(App) || App <- Apps],
    Conf = [
      {rewrite, "testlivestream", "/dev/null"},
      {stream, "channel0", "passive://ok"},
      {stream, "channel1", "passive://ok"},
      {stream, "channel2", "passive://ok", [{sessions, "http://127.0.0.1:5555/auth"}]},
      {stream, "channel3", "passive://ok", [{password,"user1:pass1"}]},
      {mpegts, "mpegts"},
      {live, "live"},
      {live, "liveauth", [{password, "user2:pass2"}]}
    ],
    {ok, Config} = flu_config:parse_config(Conf, undefined),
    application:set_env(flussonic, config, Config),
    % application:load(lager),
    % application:set_env(lager,handlers,[{lager_console_backend,info}]),
    % application:set_env(lager,error_logger_redirect,true),
    % application:set_env(lager,crash_log,undefined),
    % lager:start(),

    cowboy:start_http(fake_http, 3, 
      [{port,5555}],
      [{dispatch,[{'_',[{[<<"auth">>], fake_auth, [unique_user_id]}] ++ flu_config:parse_routes(Config)}]}]
    ),
    {ok, _Pid} = flu_stream:autostart(<<"channel0">>),
    % [Pid ! F || F <- lists:sublist(flu_rtmp_tests:h264_aac_frames(), 1, 50)],
    ok
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(cowboy),
    application:stop(gen_tracker),
    application:stop(inets),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end,
  [
    {"test_404_if_not_started", fun test_404_if_not_started/0}
    ,{"test_null_packets_if_no_media_info", fun test_null_packets_if_no_media_info/0}
    ,{"test_mpegts", fun test_mpegts/0}
    ,{"test_mpegts2", fun test_mpegts2/0}
    ,{"test_null_packets_when_frames_delay", fun test_null_packets_when_frames_delay/0}
    ,{"test_change_media_info", fun test_change_media_info/0}
    ,{"test_unauthorized_access", fun test_unauthorized_access/0}
    ,{"test_authorized_access_with_unique_user_id", fun test_authorized_access_with_unique_user_id/0}
    % ,{"test_publish_mpegts", fun test_publish_mpegts/0}
  ]
  }.

test_mpegts() ->
  capture_mpegts_url("/mpegts/testlivestream"),
  ok.

test_mpegts2() ->
  capture_mpegts_url("/testlivestream/mpegts"),
  ok.


test_404_if_not_started() ->
  % {ok, _Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel4/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  ?assertEqual(404, Code),
  gen_tcp:close(Sock).



test_null_packets_if_no_media_info() ->
  % {ok, _Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel1/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  ?assertEqual(200, Code),
  ok = read_headers(Sock),
  {ok, Bin} = gen_tcp:recv(Sock, 188),
  ?assertMatch(<<16#47, _:3, 16#1FFF:13, _/binary>>, Bin),
  gen_tcp:close(Sock).


capture_mpegts_url(URL) ->
  {ok, Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()},{hls,false},{hds,false}]),
  Stream ! flu_rtmp_tests:h264_aac_media_info(),
  Frames = flu_rtmp_tests:h264_aac_frames(),
  gen_server:call(Stream, hd(Frames)),
  {ok, M} = gen_server:call(Stream, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),    

  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET ",URL," HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),

  [Stream ! Frame || Frame <- tl(Frames)],
  Data = read_stream(Sock),
  gen_tcp:close(Sock),
  flussonic_sup:stop_stream(<<"testlivestream">>),
  ?assert(size(Data) > 0),
  ?assert(size(Data) > 10000),
  {ok, NetFrames} = mpegts_decoder:decode_file(Data),
  ?assertMatch(Len when Len > 10, length(NetFrames)),
  ok.



test_null_packets_when_frames_delay() ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel0/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),
  {ok, Stream} = flu_stream:find(<<"channel0">>),
  {ok, M} = gen_server:call(Stream, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),    

  [Stream ! Frame || Frame <- lists:sublist(flu_rtmp_tests:h264_aac_frames(),1,50)],
  _Data = read_stream1(Sock),

  {ok, Bin} = gen_tcp:recv(Sock, 188),
  ?assertMatch(<<16#47, _:3, 16#1FFF:13, _/binary>>, Bin),

  gen_tcp:close(Sock),
  ok.


test_change_media_info() ->
  {ok, Stream} = flu_stream:autostart(<<"channel1">>),
  AllFrames = flu_rtmp_tests:h264_aac_frames(),
  {Frames1, Frames2} = lists:split(length(AllFrames) div 2, AllFrames),
  NoAudio = [F || #video_frame{content = video} = F <- Frames1],
  MI1 = video_frame:define_media_info(undefined, NoAudio),
  Stream ! MI1,
  MI2 = video_frame:define_media_info(MI1, Frames1),

  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel1/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),

  {ok, M} = gen_server:call(Stream, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),    


  [Stream ! Frame || Frame <- NoAudio],
  Data1 = read_stream1(Sock),
  {ok, OutFrames1} = mpegts_decoder:decode_file(Data1),
  ?assertMatch(Len when Len > 10, length(OutFrames1)),
  ?assertMatch(0, length([F || #video_frame{content = audio} = F <- OutFrames1])),

  gen_server:call(Stream, {set, MI2}),
  [Stream ! Frame || Frame <- Frames2],

  Data2 = read_stream1(Sock),
  {ok, OutFrames2} = mpegts_decoder:decode_file(Data2),
  ?assertMatch(Len when Len > 10, length(OutFrames2)),
  ?assertMatch(Len when Len > 10, length([F || #video_frame{content = audio} = F <- OutFrames2])),

  ok.


test_unauthorized_access() ->
  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock1, ["GET /channel2/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock1, 0),
  ?assertEqual(403, Code),
  gen_tcp:close(Sock1),
  ok.


test_authorized_access_with_unique_user_id() ->

  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock1, ["GET /channel2/mpegts?token=123 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock1, 0),
  ?assertEqual(200, Code),
  read_headers(Sock1),


  {ok, Sock2} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock2, ["GET /channel2/mpegts?token=456 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock2, 0),
  read_headers(Sock2),
  ?assertEqual(200, Code),

  inet:setopts(Sock1, [{active,true}]),
  receive
    {tcp_closed, Sock1} -> ok
  after
    500 -> error(previous_socket_not_closed)
  end,
  gen_tcp:close(Sock2),
  ok.


% test_publish_mpegts() ->
%   {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
%   gen_tcp:send(Sock1, ["POST /channel1/mpegts HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n"]),
%   {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock1, 0, 200),
%   ?assertEqual(200, Code),
%   gen_tcp:close(Sock1),
%   ok.


read_headers(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, _, _, _}} -> read_headers(Sock);
    {ok, http_eoh} -> inet:setopts(Sock, [binary,{packet,raw}]), ok
  end.

read_stream(Sock) -> read_stream(Sock, []).
read_stream1(Sock) ->
  inet:setopts(Sock, [{active,true}]),
  Bin = read_stream1(Sock, []),
  inet:setopts(Sock, [{active,false}]),
  Bin.



read_stream(Sock, Acc) ->
  case gen_tcp:recv(Sock, 1024*188, 100) of
    {ok, Bin} -> read_stream(Sock, [Bin|Acc]);
    {error, timeout} -> iolist_to_binary(lists:reverse(Acc))
  end.


read_stream1(Sock, Acc) ->
  receive
    {tcp, Sock, Bin} -> read_stream1(Sock, [Bin|Acc])
  after
    400 -> iolist_to_binary(lists:reverse(Acc))
  end.
