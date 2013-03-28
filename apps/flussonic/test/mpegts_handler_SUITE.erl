-module(mpegts_handler_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("common_test/include/ct.hrl").



all() ->
  [{group, mpegts}].


groups() ->
  [
    {mpegts, [parallel], [
      read_404,
      null_packets_if_no_media_info,
      change_media_info,
      access_protection,
      publish_mpegts_wrong_url,
      authorized_access_with_unique_user_id,
      null_packets_when_frames_delay,
      publish_mpegts
    ]}
  ].

init_per_suite(Config) ->
  R = flu_test:setup([log], fun() ->
    flu_test:set_config([
      {rewrite, "channel0", "passive://ok"},
      {rewrite, "channel_no_media_info", "passive://ok"},
      {rewrite, "change_media_info", "passive://ok"},
      {rewrite, "protected_channel", "passive://ok", [{sessions, "http://127.0.0.1:5671/auth/user_unique"}]},
      {rewrite, "publish_channel", "passive://ok", [{password,"pass1"},{publish_enabled,true},{sessions, "http://127.0.0.1:5671/auth/user_unique"}]},
      {mpegts, "mpegts"},
      {live, "live"},
      {live, "liveauth", [{password, "user2:pass2"}]}
    ]),
    ok
  end),
  [{r,R}|Config].

end_per_suite(Config) ->
  {value,{r,R},Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1.



read_404(_Config) ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /unexistent_channel/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 404,_}} = gen_tcp:recv(Sock, 0),
  gen_tcp:close(Sock).



null_packets_if_no_media_info(_Config) ->
  % {ok, _Stream} = flu_stream:autostart(<<"testlivestream">>, [{source_timeout,10000},{source,self()}]),
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel_no_media_info/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock, 0),
  ok = read_headers(Sock),
  {ok, <<16#47, _:3, 16#1FFF:13, _/binary>>} = gen_tcp:recv(Sock, 188),
  gen_tcp:close(Sock).




change_media_info(_Config) ->
  {ok, Stream} = flu_stream:autostart(<<"change_media_info">>),
  MI = #media_info{streams = Streams} = flu_test:media_info(),
  MI_video = MI#media_info{streams = [S || #stream_info{content = video} = S <- Streams]},

  gen_server:call(Stream, {set, MI_video}),


  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /change_media_info/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),

  {ok, M} = gen_server:call(Stream, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),    


  [Stream ! Frame || #video_frame{content = video} = Frame <- flu_test:gop(1)],
  gen_server:call(Stream, {get, media_info}),
  gen_server:call(M, media_info),

  Data1 = read_stream_till_delay(Sock, 100),
  {ok, OutFrames1} = mpegts_decoder:decode_file(Data1),
  length(OutFrames1) > 10 orelse error({too_small_video_frames,length(OutFrames1)}),
  [] = [F || #video_frame{content = audio} = F <- OutFrames1],

  gen_server:call(Stream, {set, MI}),
  [Stream ! Frame || Frame <- flu_test:gop(2)],

  Data2 = read_stream_till_delay(Sock, 100),
  {ok, OutFrames2} = mpegts_decoder:decode_file(Data2),
  length(OutFrames2) > 10 orelse error({too_small_av_frames,length(OutFrames2)}),
  length([F || #video_frame{content = audio} = F <- OutFrames2]) > 10 orelse error(no_audio_after_change_mi),

  ok.




access_protection(_Config) ->
  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock1, ["GET /protected_channel/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 403,_}} = gen_tcp:recv(Sock1, 0),
  gen_tcp:close(Sock1),

  {ok, Sock2} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock2, ["GET /protected_channel/mpegts?token=143 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock2, 0),
  gen_tcp:close(Sock2),
  ok.






publish_mpegts_wrong_url(_Config) ->
  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock1, ["POST /channel0/mpegts?token=123 HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n"]),
  {ok, {http_response, _, 403,_}} = gen_tcp:recv(Sock1, 0, 200),
  gen_tcp:close(Sock1),
  ok.





authorized_access_with_unique_user_id(_Config) ->

  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock1, ["GET /protected_channel/mpegts?token=456 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock1, 0),
  read_headers(Sock1),


  {ok, Sock2} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock2, ["GET /protected_channel/mpegts?token=456 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock2, 0),
  read_headers(Sock2),

  inet:setopts(Sock1, [{active,true}]),
  receive
    {tcp_closed, Sock1} -> ok
  after
    500 -> error(previous_socket_not_closed)
  end,
  gen_tcp:close(Sock2),
  ok.




publish_mpegts(_Config) ->
  {ok, Sock1} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false},{send_timeout,1500},{nodelay,true}]),
  gen_tcp:send(Sock1, ["POST /publish_channel/mpegts?password=pass1 HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock1, 0, 200),
  read_headers(Sock1),

  Gop1 = flu_test:gop(1),
  publish_frames(Sock1, [flu_test:media_info()|Gop1]),

  {ok, Sock2} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false},{send_timeout,500}]),
  gen_tcp:send(Sock2, ["GET /publish_channel/mpegts?token=167 HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock2, 0, 200),
  read_headers(Sock2),

  Bin1 = read_stream_till_delay(Sock2, 300),

  ok = gen_tcp:send(Sock1, "0\r\n\r\n"),
  inet:setopts(Sock1,[{active,true}]),
  receive
    {tcp_closed, Sock1} -> ok;
    {tcp_error, Sock1} -> ok
  after
    200 -> ?debugFmt("queue: ~p", [process_info(self(),messages)]), error(not_closed_publish)
  end,
  ?debugFmt("publish is stopped and socked is closed",[]),

  flu_stream:restart(<<"publish_channel">>),


  Bin2 = read_stream_till_close(Sock2),
  {ok, Frames} = mpegts_decoder:decode_file(<<Bin1/binary, Bin2/binary>>),
  length(Frames) > 0.5*length(Gop1) orelse error({{too_few_frames,length(Frames),length(Gop1)}, error}),

  gen_tcp:close(Sock2),
  ok.


publish_frames(Sock, Frames) ->
  publish_frames(Sock, Frames, mpegts:init([{resync_on_keyframe,true}])).

publish_frames(Sock, [F|Frames], M) ->
  {M1, D} = mpegts:encode(M,F),
  case iolist_size(D) > 0 of
    true -> ok = gen_tcp:send(Sock, [integer_to_list(iolist_size(D),16), "\r\n",D,"\r\n"]);
    false -> ok 
  end,
  publish_frames(Sock, Frames, M1);

publish_frames(_Sock, [], M) ->
  M.








null_packets_when_frames_delay(_Config) ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5670, [binary,{packet,http},{active,false}]),
  gen_tcp:send(Sock, ["GET /channel0/mpegts HTTP/1.0\r\n\r\n"]),
  {ok, {http_response, _, 200,_}} = gen_tcp:recv(Sock, 0),
  read_headers(Sock),
  {ok, Stream} = flu_stream:find(<<"channel0">>),
  {ok, M} = gen_server:call(Stream, start_monotone),
  gen_server:call(M, {set_start_at,{0,0,0}}),    

  Stream ! flu_test:media_info(),
  [Stream ! Frame || Frame <- flu_test:gop(1)],
  _Data = read_stream_till_delay(Sock, 1000),

  {ok, <<16#47, _:3, 16#1FFF:13, _/binary>>} = gen_tcp:recv(Sock, 188),

  gen_tcp:close(Sock),
  ok.

















read_headers(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, _, _, _}} -> read_headers(Sock);
    {ok, http_eoh} -> inet:setopts(Sock, [binary,{packet,raw}]), ok
  end.


read_stream_till_delay(Sock, Delay) ->
  inet:setopts(Sock, [{active,true}]),
  Bin = read_stream_till_delay(Sock, Delay, []),
  inet:setopts(Sock, [{active,false}]),
  Bin.


read_stream_till_delay(Sock, Delay, Acc) ->
  receive
    {tcp, Sock, Bin} -> read_stream_till_delay(Sock, Delay, [Bin|Acc])
  after
    Delay -> iolist_to_binary(lists:reverse(Acc))
  end.



read_stream_till_close(Sock) ->
  read_stream_till_close(Sock, []).

read_stream_till_close(Sock, Acc) ->
  case gen_tcp:recv(Sock, 0, 1000) of
    {ok, Bin} -> read_stream_till_close(Sock, [Bin|Acc]);
    {error, _} -> iolist_to_binary(lists:reverse(Acc))
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
