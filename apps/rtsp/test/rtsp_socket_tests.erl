-module(rtsp_socket_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).


padding_test() ->
  {ok, P} = rtsp_socket:init([[{consumer,self()}]]),
  #media_info{streams = Streams} = sdp:decode(sdp_tests:hikvision_sdp()),
  {reply, _, P2} = lists:foldl(fun(#stream_info{track_id = N} = S,{reply,_,P_}) ->
    rtsp_socket:handle_call({add_channel, N - 1, S, tcp}, from, P_)
  end, {reply, ok, P}, Streams),

  {ok, RTP} = file:read_file("../test/padding.rtp"),
  {ok, _, []} = rtsp_socket:decode_rtp(RTP, element(15,P2)),
  ok.



accept_rtsp(Port) ->
  {ok, L} = gen_tcp:listen(Port, [{reuseaddr,true},binary,{active,false}]),
  {ok, S} = gen_tcp:accept(L),
  gen_tcp:close(L),
  {ok, S}.


read(S) ->
  read(S, <<>>).

read(S, Acc) ->
  {ok, C} = gen_tcp:recv(S, 1),
  Bin = <<Acc/binary,C/binary>>,
  case rtsp:read(Bin) of
    more -> read(S, Bin);
    {more,_} -> read(S, Bin);
    Else -> Else
  end.


prepare_interleaved_session() ->
  prepare_interleaved_session([]).


prepare_interleaved_session(Opts) ->
  {ok, R} = rtsp_reader:start_link("rtsp://localhost:8554/stream", [{consumer,self()}]),
  {ok, S} = accept_rtsp(8554),

  {ok, {rtsp, request, {<<"OPTIONS">>,<<"rtsp://localhost:8554/stream">>}, 
    [{<<"CSeq">>,<<"1">>}], undefined},<<>>} = read(S),
  GP = case proplists:get_value(get_parameter, Opts, true) of
    true -> ", GET_PARAMETER";
    _ -> ""
  end,
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nCSeq: 1\r\nPublic: SETUP, TEARDOWN, ANNOUNCE, RECORD, PLAY, OPTIONS, DESCRIBE",GP,"\r\n\r\n"]),

  {ok,{rtsp,request, {<<"DESCRIBE">>,<<"rtsp://localhost:8554/stream">>},
    [{<<"CSeq">>,<<"2">>},{<<"Accept">>,<<"application/sdp">>}],undefined},<<>>} = read(S),
  SDP = sdp_tests:grandstream_sdp(),
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nCSeq: 2\r\nContent-Length: ", integer_to_list(iolist_size(SDP)), "\r\n",
    "Content-Base: rtsp://localhost:8554/stream/\r\n",
    "Content-Type: application/sdp\r\n\r\n", SDP]),

  {ok,{rtsp,request,{<<"SETUP">>,<<"rtsp://localhost:8554/stream/trackID=0">>},
    [{<<"CSeq">>,<<"3">>}, {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1">>}],undefined},<<>>} = read(S),
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nTransport: RTP/AVP/TCP;unicast;interleaved=0-1\r\n\r\n"]),


  {ok,{rtsp,request, {<<"PLAY">>,<<"rtsp://localhost:8554/stream">>},
    [{<<"CSeq">>,<<"4">>},{<<"Range">>,<<"npt=0.000-">>}],undefined},<<>>} = read(S),
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nRTP-Info: url=rtsp://localhost:8554/stream/trackID=0;seq=0;rtptime=0\r\n\r\n"]),

  {ok, S, R}.









prepare_udp_session() ->
  {ok, R} = rtsp_reader:start_link("rtsp://localhost:8554/stream", [{consumer,self()},{rtp,udp}]),
  {ok, S} = accept_rtsp(8554),

  {ok, {rtsp, request, {<<"OPTIONS">>,<<"rtsp://localhost:8554/stream">>}, 
    [{<<"CSeq">>,<<"1">>}], undefined},<<>>} = read(S),
  gen_tcp:send(S, "RTSP/1.0 200 OK\r\nCSeq: 1\r\nPublic: SETUP, TEARDOWN, ANNOUNCE, RECORD, PLAY, OPTIONS, DESCRIBE, GET_PARAMETER\r\n\r\n"),

  {ok,{rtsp,request, {<<"DESCRIBE">>,<<"rtsp://localhost:8554/stream">>},
    [{<<"CSeq">>,<<"2">>},{<<"Accept">>,<<"application/sdp">>}],undefined},<<>>} = read(S),
  SDP = sdp_tests:grandstream_sdp(),
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nCSeq: 2\r\nContent-Length: ", integer_to_list(iolist_size(SDP)), "\r\n",
    "Content-Base: rtsp://localhost:8554/stream/\r\n",
    "Content-Type: application/sdp\r\n\r\n", SDP]),

  {ok,{rtsp,request,{<<"SETUP">>,<<"rtsp://localhost:8554/stream/trackID=0">>},
    [{<<"CSeq">>,<<"3">>}, {<<"Transport">>,<<"RTP/AVP;unicast;client_port=",Client/binary>>}],undefined},<<>>} = read(S),
  {match, [CliRTP]} = re:run(Client, "(\\d+)-", [{capture,all_but_first,list}]),
  {ok, RTP, RTCP} = rtsp_socket:bind_udp(),
  gen_udp:connect(RTP, "127.0.0.1", list_to_integer(CliRTP)),
  {ok, RTP_n} = inet:port(RTP), inet:setopts(RTP, [{active,false}]),
  {ok, RTCP_n} = inet:port(RTCP), inet:setopts(RTCP, [{active,false}]),

  Reply = io_lib:format("~B-~B", [RTP_n, RTCP_n]),

  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nTransport: RTP/AVP;unicast;client_port=",Client,";server_port=",Reply, "\r\n\r\n"]),

  {ok,{rtsp,request, {<<"PLAY">>,<<"rtsp://localhost:8554/stream">>},
    [{<<"CSeq">>,<<"4">>},{<<"Range">>,<<"npt=0.000-">>}],undefined},<<>>} = read(S),
  gen_tcp:send(S, ["RTSP/1.0 200 OK\r\nRTP-Info: url=rtsp://localhost:8554/stream/trackID=0;seq=0;rtptime=0\r\n\r\n"]),

  {ok, S, R, RTP, RTCP}.



rtsp_interleaved_axis_test() ->
  {ok, S, R} = prepare_interleaved_session(),

  R ! send_rr,
  R ! keepalive,
  {ok,{rtsp,request, {<<"GET_PARAMETER">>,<<"rtsp://localhost:8554/stream">>},
      [{<<"CSeq">>,<<"5">>}], undefined}, <<>>} = read(S),
  % First we check that no RTCP is available


  RTCP_SR = <<128,200,0,6,118,32,142,25,212,206,1,246,245,1,108,231,67,184,129,254,0,0,0,80,0,0,
  107,79,129,202,0,9,118,32,142,25,1,26,115,116,114,101,97,109,101,114,64,97,120,105,
  115,45,48,48,52,48,56,99,97,53,49,51,51,52,0,0,0,0>>,
  ok = gen_tcp:send(S, [<<$$, 1, (size(RTCP_SR)):16>>, RTCP_SR]),

  R ! keepalive,
  {ok,{rtsp,request, {<<"GET_PARAMETER">>,<<"rtsp://localhost:8554/stream">>},
      [{<<"CSeq">>,<<"6">>}], undefined}, <<>>} = read(S),
  % First we check that no RTCP is available


  R ! send_rr,
  {ok,{rtsp,rtp,1,undefined,<<129,201,0,7,118,32,142,25,118,32,142,25,0,0,0,0,0,0,0,0,0,0,0,0,_:32,_:32>>},<<>>} 
    = read(S),
  ok.




rtsp_interleaved_read_no_rr_test() ->
  {ok, S, R} = prepare_interleaved_session(),

  R ! send_rr,
  R ! keepalive,
  % We have not send any SR with SSRC so we shouldn't receive any RR, only GET_PARAMETER
  {ok,{rtsp,request, {<<"GET_PARAMETER">>,<<"rtsp://localhost:8554/stream">>},
      [{<<"CSeq">>,<<"5">>}], undefined}, <<>>} = read(S),
  gen_tcp:close(S),
  erlang:exit(R,normal),
  ok.



rtsp_interleaved_read_no_get_parameter_test() ->
  {ok, S, R} = prepare_interleaved_session([{get_parameter,false}]),

  SSRC = 143,
  RTP = <<2:2, 0:1, 0:1, 0:4, 1:1, 97:7, 0:16, 0:32, SSRC:32, 9,0>>,
  gen_tcp:send(S, [<<$$, 0, (size(RTP)): 16>>, RTP]),

  R ! keepalive,
  R ! send_rr,
  % We send response without GET_PARAMETER among methods, so flussonic must keepalive 
  % only with RTCP RR
  {ok,{rtsp,rtp,1,undefined,<<129,201,0,7,0,0,0,143,0,0,0,143,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>},<<>>} 
    = read(S),

  gen_tcp:close(S),
  erlang:exit(R,normal),
  ok.


rtsp_interleaved_read_test() ->
  {ok, S, R} = prepare_interleaved_session(),

  SSRC = 143,
  RTP = <<2:2, 0:1, 0:1, 0:4, 1:1, 97:7, 0:16, 0:32, SSRC:32, 9,0>>,
  gen_tcp:send(S, [<<$$, 0, (size(RTP)): 16>>, RTP]),
  R ! keepalive,
  R ! send_rr,

  {ok,{rtsp,request,{<<"GET_PARAMETER">>,<<"rtsp://localhost:8554/stream">>},
    [{<<"CSeq">>,<<"5">>}],undefined},
    <<>>} = read(S),
  % We have sent RTP packet with SSRC, so we receive some RR
  {ok,{rtsp,rtp,1,undefined,<<129,201,0,7,0,0,0,143,0,0,0,143,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>},<<>>} 
    = read(S),
  gen_tcp:close(S),
  erlang:exit(R,normal),
  ok.



rtsp_udp_test() ->
  {ok, S, R, RTP, RTCP} = prepare_udp_session(),
  SSRC = 143,
  Bin = <<2:2, 0:1, 0:1, 0:4, 1:1, 97:7, 0:16, 0:32, SSRC:32, 9,0>>,
  gen_udp:send(RTP, Bin),
  timer:sleep(50),
  R ! send_rr,
  % We have sent RTP packet with SSRC, so we receive some RR
  {ok, {_, _, <<129,201,0,7,SSRC:32,SSRC:32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}} = gen_udp:recv(RTCP,0),
  gen_tcp:close(S),
  erlang:exit(R,normal),
  ok.






rtsp_play_test_() ->
  {foreach, fun() ->
    application:start(crypto),
    application:start(ranch),
    rtsp:start_server(8854, fake_rtsp, fake_rtsp_callback)
  end, fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(ranch),
    application:stop(lager),
    error_logger:add_report_handler(error_logger_tty_h),
  ok
  end, 
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("play_", atom_to_list(F))]
  }.



play_udp() ->
  {ok, S} = gen_tcp:connect("localhost", 8854, [binary, {active,false}]),

  gen_tcp:send(S, "DESCRIBE rtsp://localhost:8854/stream1 RTSP/1.0\r\nCSeq: 1\r\nAccept: application/sdp\r\n\r\n"),
  {ok, {rtsp, response, {200, _}, _, _}, <<>>} = read(S),

  {ok, RTP, RTCP} = rtsp_socket:bind_udp(),
  {ok, RTP_n} = inet:port(RTP), inet:setopts(RTP, [{active,false}]),
  {ok, RTCP_n} = inet:port(RTCP), inet:setopts(RTCP, [{active,false}]),

  Ports = io_lib:format("~B-~B", [RTP_n, RTCP_n]),

  gen_tcp:send(S, 
    ["SETUP rtsp://localhost:8854/stream1/trackID=1 RTSP/1.0\r\nCSeq: 2\r\nTransport: RTP/AVP;unicast;client_port=",
    Ports,"\r\n\r\n"]),
  {ok, {rtsp, response, {200, _}, SetupHeaders, _}, <<>>} = read(S),
  {match, [_RTPPort, RTCPPort]} = re:run(rtsp:header(transport, SetupHeaders), "server_port=(\\d+)-(\\d+)", [{capture,all_but_first,list}]),
  gen_udp:connect(RTCP, "127.0.0.1", list_to_integer(RTCPPort)),


  Ports2 = io_lib:format("~B-~B", [RTP_n+2, RTCP_n+2]),
  gen_tcp:send(S, 
    ["SETUP rtsp://localhost:8854/stream1/trackID=2 RTSP/1.0\r\nCSeq: 3\r\nTransport: RTP/AVP;unicast;client_port=",
    Ports2,"\r\n\r\n"]),
  {ok, {rtsp, response, {200, _}, _, _}, <<>>} = read(S),



  Session = rtsp:header(session, SetupHeaders),
  gen_tcp:send(S, ["PLAY rtsp://localhost:8854/stream1/trackID=1 RTSP/1.0\r\nCSeq: 4\r\nSession: ", Session,"\r\n\r\n"]),
  {ok, {rtsp, response, {200,_}, _, _},<<>>} = read(S),

  {ok, {_,_, <<_RTPHeader:8, _Marker:1, _:7, Seq:16, _Timecode:32, VSSRC:32, _/binary>>}} = gen_udp:recv(RTP, 0),

  RR = <<2:2, 0:1, 1:5, 201, 7:16, VSSRC:32, VSSRC:32, 0:32, Seq:32, 0:32, 0:32, 0:32>>,
  gen_udp:send(RTCP, RR),


  gen_tcp:send(S, "GET_PARAMETER rtsp://localhost:8854/stream1 RTSP/1.0\r\nCSeq: 5\r\n\r\n"),
  {ok, {rtsp, response, {200, _}, _, _}, <<>>} = read(S),

  ok.















