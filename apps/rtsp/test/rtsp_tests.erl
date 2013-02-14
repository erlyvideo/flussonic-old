-module(rtsp_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


rtsp_parse_test_() ->
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))].



test_single_request0() ->
  ?assertEqual({ok, {rtsp, request, {<<"OPTIONS">>, <<"rtsp://axis-00408ca51334.local./axis-media/media.amp">>},
    [{<<"CSeq">>, <<"1">>},{<<"Authorization">>,<<"Basic cm9vdDp0b29y">>}], undefined}, <<>>}, rtsp:read(options_request())),
  ok.

test_single_request1() ->
  ?assertEqual({ok, {rtsp, request, {<<"GET_PARAMETER">>, <<"rtsp://localhost:1554/vod/bunny.mp4/">>},
    [{<<"CSeq">>, <<"7">>},{<<"User-Agent">>,<<"LibVLC/2.0.1 (LIVE555 Streaming Media v2011.12.23)">>},
    {<<"Session">>,<<"1360704095752081">>}], undefined}, <<>>}, rtsp:read(describe_request())).



test_single_response0() ->
  ?assertEqual({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"3">>},{<<"Session">>,<<"CD94B91F; timeout=60">>},
    {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>}
    ], undefined}, <<>>}, rtsp:read(response_no_body())),
  ok.

test_single_response1() ->
  ?assertEqual({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"3">>},{<<"Session">>,<<"CD94B91F; timeout=60">>},
    {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=4ED43635;mode=\"PLAY\"">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>}
    ], undefined}, <<1,2,3,4>>}, rtsp:read(iolist_to_binary([response_no_body(),1,2,3,4]))),
  ok.



test_body_response1() ->
  ?assertMatch({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"2">>},{<<"Content-Type">>,<<"application/sdp">>},
    {<<"Content-Base">>,<<"rtsp://axis-00408ca51334.local./axis-media/media.amp/">>},
    {<<"Date">>,<<"Sun, 20 Jan 2013 12:11:04 GMT">>},{<<"Content-Length">>,<<"389">>}
    ], <<"v=0\r\no=- ", _/binary>> = Body}, <<1,2,3,4>>} when size(Body) == 389, rtsp:read(iolist_to_binary([describe_response(), 1,2,3,4]) )),
  ok.


%% Grandstream hacks
test_body_response2() ->
  ?assertMatch({ok, {rtsp, response, {200, <<"OK">>},
    [{<<"Cseq">>, <<"2">>},{<<"Content-length">>,<<"10">>}
    ], <<"0123456789">>}, <<>>}, rtsp:read(<<"RTSP/1.0 200 OK\r\nCseq: 2\r\nContent-length: 10\r\n\r\n0123456789">>) ),
  ok.


test_badly_parse() ->
  RTSP = <<"RTSP/1.0 501 Not Implemented\r\nCSeq: 6\r\nServer: GrandStream Rtsp Server V100R001\r\n"
  "Accept: OPTIONS, DESCRIBE, SETUP, PLAY, TEARDOWN, SET_PARAMETER\n\r\n$">>,
  ?assertEqual({ok, {rtsp, response, {501, <<"Not Implemented">>}, [
    {<<"CSeq">>,<<"6">>}, {<<"Server">>, <<"GrandStream Rtsp Server V100R001">>},
    {<<"Accept">>,<<"OPTIONS, DESCRIBE, SETUP, PLAY, TEARDOWN, SET_PARAMETER">>}
  ], undefined}, <<"$">>}, rtsp:read(RTSP)).


test_interleaved1() ->
  {ok, {rtsp, request, {<<"OPTIONS">>, _}, _, _}, R2} = rtsp:read(iolist_to_binary([options_request(), rtp(), response_no_body()])),
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,5,6,7,8>>}, R3} = rtsp:read(R2),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(R3),
  ok.



test_interleaved5() ->
  {ok, {rtsp, rtp, 1, _, <<1,2,3,4,"\r\n", 5,6,7,8>>}, R3} = rtsp:read(iolist_to_binary([rtp1(), response_no_body()]) ),
  {ok, {rtsp, response, {200, <<"OK">>}, _, _}, <<>>} = rtsp:read(R3),
  ok.




describe_request() ->
<<"GET_PARAMETER rtsp://localhost:1554/vod/bunny.mp4/ RTSP/1.0\r\nCSeq: 7\r\n"
"User-Agent: LibVLC/2.0.1 (LIVE555 Streaming Media v2011.12.23)\r\n"
"Session: 1360704095752081\r\n\r\n">>.


announce_request() ->
iolist_to_binary([<<"ANNOUNCE rtsp://localhost:1554/mystream.sdp RTSP/1.0\r\n"
"CSeq: 1\r\n"
"Content-Type: application/sdp\r\n"
"User-Agent: QuickTime/7.7.1 (qtver=7.7.1;cpu=IA32;os=Mac 10.7.5)\r\n"
"Content-Length: 596\r\n\r\n">>, sdp_tests:quicktime_broadcaster_sdp()]).


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









