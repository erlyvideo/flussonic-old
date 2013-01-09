-module(mpegts_handler_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").



mpegts_test_() ->
  {foreach,
  fun() ->
    Apps = [ranch, gen_tracker, cowboy, flussonic],
    [application:stop(App) || App <- Apps],
    [ok = application:start(App) || App <- Apps],
    Conf = [
      {rewrite, "testlivestream", "/dev/null"},
      {mpegts, "mpegts"}
    ],
    {ok, Config} = flu_config:parse_config(Conf, undefined),
    application:set_env(flussonic, config, Config),
    cowboy:start_http(fake_http, 3, 
      [{port,5555}],
      [{dispatch,[{'_',flu_config:parse_routes(Config)}]}]
    ),
    meck:new(flu_monotone, [{passthrough,true}]),
    meck:expect(flu_monotone, delay, fun(_,_) -> 0 end),
    ok
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    meck:unload(flu_monotone),
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(gen_tracker),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end,
  [
    {"test_mpegts", fun test_mpegts/0},
    {"test_mpegts2", fun test_mpegts2/0}
    ,{"test_not_started_mpegts", fun test_not_started_mpegts/0}
  ]
  }.

test_mpegts() ->
  capture_mpegts_url("/mpegts/testlivestream"),
  ok.

test_mpegts2() ->
  capture_mpegts_url("/testlivestream/mpegts"),
  ok.


test_not_started_mpegts() ->
  % {ok, _Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /livestream/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  ?assertEqual(404, Code),
  gen_tcp:close(Sock).



capture_mpegts_url(URL) ->
  {ok, Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  Stream ! flu_rtmp_tests:h264_aac_media_info(),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET ",URL," HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),
  [Stream ! Frame || Frame <- flu_rtmp_tests:h264_aac_frames()],
  Data = read_stream(Sock),
  gen_tcp:close(Sock),
  flussonic_sup:stop_stream(<<"testlivestream">>),
  ?assert(size(Data) > 0),
  ?assert(size(Data) > 10000),

  {ok, Frames} = mpegts_decoder:decode_file(Data),
  ?assertMatch(Len when Len > 10, length(Frames)),
  ok.




read_headers(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, _, _, _}} -> read_headers(Sock);
    {ok, http_eoh} -> inet:setopts(Sock, [binary,{packet,raw}]), ok
  end.

read_stream(Sock) -> read_stream(Sock, []).


read_stream(Sock, Acc) ->
  case gen_tcp:recv(Sock, 1024, 500) of
    {ok, Data} -> read_stream(Sock, [Data|Acc]);
    {error, _} -> iolist_to_binary(lists:reverse(Acc))
  end.

