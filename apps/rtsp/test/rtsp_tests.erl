-module(rtsp_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


rtsp_parse_test_() ->
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))].


pair() ->
  {ok, L} = gen_tcp:listen(7777, [binary,{reuseaddr, true}]),
  Self = self(),
  spawn_link(fun() ->
    {ok, Srv} = gen_tcp:accept(L),
    inet:setopts(Srv, [binary,{active,false},{nodelay, true}]),
    gen_tcp:controlling_process(Srv, Self),
    Self ! {server, Srv}
  end),
  spawn_link(fun() ->
    {ok, Cli} = gen_tcp:connect("127.0.0.1", 7777, [binary,{active,false},{reuseaddr, true}, {nodelay, true}]),
    gen_tcp:controlling_process(Cli, Self),
    Self ! {client, Cli}
  end),
  Client = receive {client, Cli} -> Cli after 100 -> error(timeout_client) end,
  Server = receive {server, Serv} -> Serv after 100 -> error(timeout_server) end,
  gen_tcp:close(L),
  {ok, Client, Server}.



test_single_request0() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, options_request()),
  ?assertEqual({ok, {rtsp, request, {<<"OPTIONS">>, <<"rtsp://axis-00408ca51334.local./axis-media/media.amp">>},
    [{<<"CSeq">>, <<"1">>},{<<"Authorization">>,<<"Basic cm9vdDp0b29y">>}], undefined}, <<>>}, rtsp:read(S2, <<>>)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_single_request1() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, options_request()),
  {ok, R1} = gen_tcp:recv(S2, 80),
  ?assertEqual({ok, {rtsp, request, {<<"OPTIONS">>, <<"rtsp://axis-00408ca51334.local./axis-media/media.amp">>},
    [{<<"CSeq">>, <<"1">>},{<<"Authorization">>,<<"Basic cm9vdDp0b29y">>}], undefined}, <<>>}, rtsp:read(S2, R1)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).

test_single_request2() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, options_request()),
  {ok, R1} = gen_tcp:recv(S2, 20),
  ?assertEqual({ok, {rtsp, request, {<<"OPTIONS">>, <<"rtsp://axis-00408ca51334.local./axis-media/media.amp">>},
    [{<<"CSeq">>, <<"1">>},{<<"Authorization">>,<<"Basic cm9vdDp0b29y">>}], undefined}, <<>>}, rtsp:read(S2, R1)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).



test_single_response0() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, response_no_body()),
  ?assertEqual({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"3">>},{<<"Session">>,<<"CD94B91F; timeout=60">>},
    {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>}
    ], undefined}, <<>>}, rtsp:read(S2, <<>>)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).

test_single_response1() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [response_no_body(),1,2,3,4]),
  {ok, R1} = gen_tcp:recv(S2, 80),
  ?assertEqual({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"3">>},{<<"Session">>,<<"CD94B91F; timeout=60">>},
    {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>}
    ], undefined}, <<>>}, rtsp:read(S2, R1)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_single_response2() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [response_no_body(), 1,2,3,4]),
  {ok, R1} = gen_tcp:recv(S2, iolist_size(response_no_body())+4),
  ?assertEqual({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"3">>},{<<"Session">>,<<"CD94B91F; timeout=60">>},
    {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>}
    ], undefined}, <<1,2,3,4>>}, rtsp:read(S2, R1)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_body_response1() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [describe_response(), 1,2,3,4]),
  % {ok, R1} = gen_tcp:recv(S2, iolist_size(response_no_body())+4),
  ?assertMatch({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"2">>},{<<"Content-Type">>,<<"application/sdp">>},
    {<<"Content-Base">>,<<"rtsp://axis-00408ca51334.local./axis-media/media.amp/">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>},{<<"Content-Length">>,<<"389">>}
    ], <<"v=0\r\no=- ", _/binary>> = Body}, <<>>} when size(Body) == 389, rtsp:read(S2, <<>>)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).



test_body_response2() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [describe_response(), 1,2,3,4]),
  {ok, R1} = gen_tcp:recv(S2, 210),
  ?assertMatch({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"2">>},{<<"Content-Type">>,<<"application/sdp">>},
    {<<"Content-Base">>,<<"rtsp://axis-00408ca51334.local./axis-media/media.amp/">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>},{<<"Content-Length">>,<<"389">>}
    ], <<"v=0\r\no=- ", _/binary>> = Body}, <<>>} when size(Body) == 389, rtsp:read(S2, R1)),
  gen_tcp:close(S1),
  gen_tcp:close(S2).



test_interleaved1() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [options_request(), rtp(), response_no_body()]),
  R1 = <<>>,
  {ok, {rtsp, request, {<<"OPTIONS">>, _}, _, _}, R2} = rtsp:read(S2, R1),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,5,6,7,8>>}, R3} = rtsp:read(S2, R2),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(S2, R3),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_interleaved2() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [options_request(), rtp(), response_no_body()]),
  {ok, R1} = gen_tcp:recv(S2, 1),
  {ok, {rtsp, request, {<<"OPTIONS">>, _}, _, _}, R2} = rtsp:read(S2, R1),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,5,6,7,8>>}, R3} = rtsp:read(S2, R2),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(S2, R3),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_interleaved3() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [options_request(), rtp(), response_no_body()]),
  {ok, R1} = gen_tcp:recv(S2, 3),
  {ok, {rtsp, request, {<<"OPTIONS">>, _}, _, _}, R2} = rtsp:read(S2, R1),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,5,6,7,8>>}, R3} = rtsp:read(S2, R2),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(S2, R3),
  gen_tcp:close(S1),
  gen_tcp:close(S2).



test_interleaved4() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [options_request(), rtp(), response_no_body()]),
  {ok, R1} = gen_tcp:recv(S2, 203),
  {ok, {rtsp, request, {<<"OPTIONS">>, _}, _, _}, R2} = rtsp:read(S2, R1),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,5,6,7,8>>}, R3} = rtsp:read(S2, R2),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(S2, R3),
  gen_tcp:close(S1),
  gen_tcp:close(S2).


test_interleaved5() ->
  {ok, S1, S2} = pair(),
  gen_tcp:send(S1, [rtp1(), response_no_body()]),
  {ok, R1} = gen_tcp:recv(S2, 11),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,"\r\n", 5,6,7,8>>}, R3} = rtsp:read(S2, R1),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(S2, R3),
  gen_tcp:close(S1),
  gen_tcp:close(S2).






options_request() ->
<<"OPTIONS rtsp://axis-00408ca51334.local./axis-media/media.amp RTSP/1.0\r
CSeq: 1\r
Authorization: Basic cm9vdDp0b29y\r
\r
">>.

response_no_body() ->
<<"RTSP/1.0 200 OK\r
Cseq: 3\r
Session: CD94B91F; timeout=60\r
Transport: RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"\r
Date: Sun, 20 Jan 2013 12:11:04 GMT\r
\r
">>.

rtp() -> <<$$, 1, 8:16, 1,2,3,4,5,6,7,8>>.


rtp1() -> <<$$, 1, 10:16, 1,2,3,4,"\r\n",5,6,7,8>>.

describe_response() -> 
<<"RTSP/1.0 200 OK\r
Cseq: 2\r
Content-Type: application/sdp\r
Content-Base: rtsp://axis-00408ca51334.local./axis-media/media.amp/\r
Date: Sun, 20 Jan 2013 12:11:04 GMT\r
Content-Length: 389\r
\r
v=0\r
o=- 1358683864292867 1358683864292867 IN IP4 axis-00408ca51334.local.\r
s=Media Presentation\r
e=NONE\r
c=IN IP4 0.0.0.0\r
b=AS:50000\r
t=0 0\r
a=control:*\r
a=range:npt=0.000000-\r
m=video 0 RTP/AVP 96\r
b=AS:50000\r
a=framerate:30.0\r
a=control:trackID=1\r
a=rtpmap:96 H264/90000\r
a=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==\r
">>.









