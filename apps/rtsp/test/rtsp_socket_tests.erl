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
  {ok, _, []} = rtsp_socket:decode_rtp(RTP, element(14,P2)),
  ok.

