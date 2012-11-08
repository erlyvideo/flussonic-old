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
      ok
  end,
  fun(_) ->
    application:stop(cowboy),
    application:stop(flussonic),
    application:stop(ranch),
    application:stop(gen_tracker)
  end,
  [
    fun test_mpegts/0,
    fun test_mpegts2/0
  ]
  }.

test_mpegts() ->
  {ok, Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  Stream ! flu_rtmp_tests:h264_aac_media_info(),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /mpegts/testlivestream HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),
  [Stream ! Frame || Frame <- flu_rtmp_tests:h264_aac_frames()],
  Data = read_stream(Sock),
  gen_tcp:close(Sock),
  flussonic_sup:stop_stream(<<"testlivestream">>),
  ?assert(size(Data) > 0),
  ?assert(size(Data) > 100),

  {ok, Frames} = mpegts_decoder:decode_file(Data),
  ?assert(length(Frames) > 10),
  ok.


test_mpegts2() ->
  {ok, Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  Stream ! flu_rtmp_tests:h264_aac_media_info(),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5555, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, "GET /testlivestream/mpegts HTTP/1.0\r\n\r\n"),
  {ok, {http_response, _, Code,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  ?assertEqual(200, Code),
  [Stream ! Frame || Frame <- flu_rtmp_tests:h264_aac_frames()],
  Data = read_stream(Sock),
  gen_tcp:close(Sock),
  flussonic_sup:stop_stream(<<"testlivestream">>),
  ?assert(size(Data) > 0),
  ?assert(size(Data) > 100),

  {ok, Frames} = mpegts_decoder:decode_file(Data),
  ?assert(length(Frames) > 10),
  ok.


read_headers(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, _, _, _}} -> read_headers(Sock);
    {ok, http_eoh} -> inet:setopts(Sock, [binary,{packet,raw}]), ok
  end.

read_stream(Sock) -> read_stream(Sock, []).


read_stream(Sock, Acc) ->
  case gen_tcp:recv(Sock, 188, 500) of
    {ok, Data} -> read_stream(Sock, [Data|Acc]);
    {error, _} -> iolist_to_binary(lists:reverse(Acc))
  end.

