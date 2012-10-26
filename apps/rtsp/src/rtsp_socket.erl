-module(rtsp_socket).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").

-define(PT_H264, 96).
-define(PT_AAC, 97).
-define(PT_PCMA, 8).
-define(PT_PCMU, 0).
   


-record(rtsp, {
  socket,
  peer_addr,
  callback,
  timeout,
  args,
  media_info,
  session,

  v_s_rtp,
  v_s_rtcp,
  v_c_rtp,
  v_c_rtcp,
  v_seq = 0,
  v_scale,
  length_size,
  nals = [],

  a_s_rtp,
  a_s_rtcp,
  a_c_rtp,
  a_c_rtcp,
  a_seq = 0,
  a_scale
}).

init([Socket, Callback, Args]) ->
  inet:setopts(Socket, [{active,once},{packet,line}]),
  Timeout = 10000,
  {ok, {PeerAddr,_}} = inet:peername(Socket),
  {ok, #rtsp{socket = Socket, callback = Callback, args = Args, timeout = Timeout, peer_addr = PeerAddr}, Timeout}.


handle_call(Call, _From, #rtsp{timeout = Timeout} = RTSP) ->
  {reply, {error, {unknown_call, Call}}, RTSP, Timeout}.


handle_info({tcp, Socket, Line}, #rtsp{timeout = Timeout} = RTSP) ->
  case re:run(Line, "^(\\w+) ([^ ]+) RTSP/1.0", [{capture,all_but_first,binary}]) of
    {match, [Method, URL_]} ->
      {Headers, Dump} = rtsp_protocol:collect_headers(Socket),
      Body = rtsp_protocol:collect_body(Socket, Headers),
      inet:setopts(Socket, [{active,once},{packet,line}]),

      io:format(">>>>>> RTSP IN (~p:~p) >>>>>~n~s~s~n~s~n", [?MODULE, ?LINE, Line, Dump, Body]),

      Len = size(URL_)-1,
      URL = case URL_ of <<URL__:Len/binary, "/">> -> URL__; _ -> URL_ end,

      RTSP1 = handle_request(Method, URL, Headers, Body, RTSP),
      {noreply, RTSP1, Timeout};
    nomatch ->
      ?DBG("Unknown RTSP data: ~250p", [Line]),
      {stop, bad_rtsp, RTSP}
  end;

handle_info({tcp_closed, _Socket}, RTSP) ->
  {stop, normal, RTSP};

handle_info(timeout, RTSP) ->
  {stop, normal, RTSP};

handle_info({udp, Port, _Addr, _RPort, _Bin}, #rtsp{} = RTSP) ->
  List = lists:zip(tl(tuple_to_list(RTSP)), record_info(fields, rtsp)),
  _Name = proplists:get_value(Port, List),
  % ?D({udp_on_port,Name, Bin}),
  inet:setopts(Port, [{active,once}]),
  {noreply, RTSP};

handle_info(#video_frame{content = metadata}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{flavor = config}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{content = video, dts = DTS, body = Data, track_id = TrackID, flavor = Flavor}, 
  #rtsp{v_s_rtp = SRTP, v_c_rtp = CRTP, peer_addr = Addr, v_seq = Seq, v_scale = Scale, 
  length_size = LengthSize, nals = ConfigNals} = RTSP) ->
  LengthSize = 4,
  FUA_NALS = lists:flatmap(fun(NAL) -> h264:fua_split(NAL, 1387) end, split_h264_frame(Data, LengthSize)),

  Nals = case Flavor of
    keyframe -> ConfigNals ++ FUA_NALS;
    frame -> FUA_NALS
  end,

  Packets = pack_h264(Nals, round(DTS*Scale), 1, TrackID, Seq),
  [  gen_udp:send(SRTP, Addr, CRTP, RTP) || RTP <- Packets],
  {noreply, RTSP#rtsp{v_seq = (Seq + length(Packets)) rem 65536}};




handle_info(#video_frame{content = audio, codec = aac, dts = DTS, body = Body1, track_id = TrackID}, 
  #rtsp{a_s_rtp = SRTP, a_c_rtp = CRTP, peer_addr = Addr, a_seq = Seq, a_scale = Scale} = RTSP) ->
  
  Bodies = [Body1|receive_aac(3)],
  AUHeader = [<<(size(Body)):13, 0:3>> || Body <- Bodies],
  AULength = iolist_size(AUHeader)*8,
  Marker = 1,
  RTP = iolist_to_binary([<<2:2, 0:1, 0:1, 0:4, Marker:1, ?PT_AAC:7, Seq:16, (round(DTS*Scale)):32, TrackID:32, 
  AULength:16>>, AUHeader, Bodies]),
  % ?D({pack_aac, round(DTS), round(DTS*Scale), length(Bodies)}),
  gen_udp:send(SRTP, Addr, CRTP, RTP),
  {noreply, RTSP#rtsp{a_seq = (Seq +1) div 65536}};


handle_info(Info, #rtsp{} = RTSP) ->
  {stop, {unknown_message, Info}, RTSP}.


receive_aac(0) -> [];
receive_aac(Count) ->
  receive
    #video_frame{codec = aac, body = Body} -> [Body|receive_aac(Count-1)]
  after
    4000 -> error(timeout)
  end.

pack_h264([NAL|Nals], Timecode, Marker, TrackID, Seq) ->
  [<<2:2, 0:1, 0:1, 0:4, Marker:1, ?PT_H264:7, Seq:16, Timecode:32, TrackID:32, NAL/binary>>|
  pack_h264(Nals, Timecode, 0, TrackID, Seq+1)];

pack_h264([], _, _, _, _) -> [].


terminate(_,_) ->
  ?D({terminate,rtsp}),
  ok.



handle_request(Method, _URL, Headers, _Body, RTSP) when Method == <<"OPTIONS">> orelse Method == <<"GET_PARAMETER">>->
  reply("200 OK", [
    {'Server', <<"Erlyvideo">>}, {'Cseq', seq(Headers)}, 
    {<<"Supported">>, <<"play.basic, con.persistent">>},
    {'Public', "SETUP, TEARDOWN, PLAY, PAUSE, OPTIONS, ANNOUNCE, DESCRIBE, RECORD, GET_PARAMETER"}], RTSP);

handle_request(<<"DESCRIBE">>, URL, Headers, Body, #rtsp{callback = Callback} = RTSP) ->
  case Callback:describe(URL, Headers, Body) of
    {error, authentication} ->
      reply("401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}], RTSP);
    {ok, #media_info{streams = Streams} = MediaInfo1} ->
      MediaInfo = MediaInfo1#media_info{streams = 
        [S || #stream_info{content = C} = S <- Streams, C == video orelse C == audio]
      },
      SDP = sdp:encode(MediaInfo),


      {A1, A2, A3} = now(),
      Session = integer_to_list((A1*1000000+A2)*1000000+A3),

      #stream_info{timescale = AScale} = lists:keyfind(audio, #stream_info.content, Streams),
      #stream_info{timescale = VScale, params = VideoParams} = lists:keyfind(video, #stream_info.content, Streams),
      #video_params{length_size = LengthSize, nals = Nals} = VideoParams,
      reply("200 OK", [{'Cseq', seq(Headers)}, {'Server', <<"Erlyvideo">>},
        {'Date', httpd_util:rfc1123_date()}, {'Expires', httpd_util:rfc1123_date()},
        {'Content-Base', io_lib:format("~s/", [URL])}], SDP, 
        RTSP#rtsp{media_info = MediaInfo, v_scale = VScale, a_scale = AScale, session = Session,
          length_size = LengthSize, nals = Nals})
  end;

handle_request(<<"SETUP">>, URL, Headers, _Body, #rtsp{media_info = #media_info{streams = Streams}} = RTSP) ->
  {match, [Track_]} = re:run(URL, "/trackID=(\\d+)", [{capture,all_but_first,list}]),
  TrackId = list_to_integer(Track_),
  Transport = proplists:get_value(<<"Transport">>, Headers),
  case binary:split(Transport, <<";">>, [global]) of
    [<<"RTP/AVP",_/binary>>, <<"unicast">>, <<"client_port=", Ports/binary>>] ->
      {match, [ClientRTP_, ClientRTCP_]} = re:run(Ports, "(\\d+)-(\\d+)", [{capture,all_but_first,list}]),
      ClientRTP = list_to_integer(ClientRTP_),
      ClientRTCP = list_to_integer(ClientRTCP_),
      {ok, RTP, RTCP} = bind_udp(),
      {ok, ServerRTP} = inet:port(RTP),
      {ok, ServerRTCP} = inet:port(RTCP),
      RTSP1 = case lists:keyfind(TrackId, #stream_info.track_id, Streams) of
        #stream_info{content = video} ->
          RTSP#rtsp{v_s_rtp = RTP, v_s_rtcp = RTCP, v_c_rtp = ClientRTP, v_c_rtcp = ClientRTCP};
        #stream_info{content = audio} ->
          RTSP#rtsp{a_s_rtp = RTP, a_s_rtcp = RTCP, a_c_rtp = ClientRTP, a_c_rtcp = ClientRTCP}
      end,
      Reply = iolist_to_binary(io_lib:format("~B-~B", [ServerRTP, ServerRTCP])),
      reply("200 OK", [{'Cseq', seq(Headers)}, {'Server', <<"Erlyvideo">>},
        {<<"Transport">>,<<Transport/binary, ";server_port=", Reply/binary>>}], RTSP1);
    _ ->
      throw({stop, {unknown_transport, Transport}, RTSP})
  end;


handle_request(<<"PLAY">>, URL, Headers, Body, #rtsp{callback = Callback, media_info = #media_info{streams = Streams}} = RTSP) ->
  case Callback:play(URL, Headers, Body) of
    {ok, Stream} ->
      erlang:monitor(process, Stream),
      FirstDTS = receive
        #video_frame{flavor = config, content = video, dts = DTS} -> DTS
      after
        5000 -> throw({stop, {no_video_config,URL}, RTSP})
      end,
      RtpInfo = string:join(lists:map(fun(#stream_info{track_id = Id, timescale = Scale}) ->
        io_lib:format("url=~s/trackID=~B;seq=0;rtptime=~B", [URL, Id, round(FirstDTS*Scale)])
      end, Streams),","),
      reply("200 OK", [{'Cseq', seq(Headers)}, {'Server', <<"Erlyvideo">>},
        {"RTP-Info", RtpInfo}
      ], RTSP);
    Reply ->
      throw({stop, {play_disabled,Reply}, RTSP})
  end;

handle_request(<<"TEARDOWN">>, _URL, Headers, _Body, #rtsp{} = RTSP) ->
  RTSP1 = reply("200 OK", [{'Cseq', seq(Headers)}], RTSP),
  throw({stop, normal, RTSP1});

handle_request(Method, URL, _Headers, _Body, RTSP) ->
  ?DBG("~s ~s~n", [Method, URL]),
  RTSP.

bind_udp() ->
  StartPort = (crypto:rand_uniform(10000,20000) div 2)*2,
  bind_udp(StartPort).

bind_udp(Port) when Port > 60000 ->
  {error, no_ports};

bind_udp(Port) ->
  case gen_udp:open(Port, [binary,{active,once}]) of
    {ok, RTP} ->
      case gen_udp:open(Port + 1, [binary,{active,once}]) of
        {ok, RTCP} -> {ok, RTP, RTCP};
        {error, _} ->
          gen_udp:close(RTP),
          bind_udp(Port + 2)
      end;
    {error, _} ->
      bind_udp(Port + 2)
  end.










% compose_rtp(#rtp_channel{sequence = Sequence} = RTP, [Part|Parts], Acc, Timecode) ->
%   Pack = make_rtp_pack(RTP, case length(Parts) of 0 -> 1; _ -> 0 end, Part, Timecode),
%   compose_rtp(RTP#rtp_channel{sequence = inc_seq(Sequence)}, Parts, [Pack|Acc], Timecode).



split_h264_frame(Frame, LengthSize) ->
  split_h264_frame(Frame, LengthSize, []).

split_h264_frame(<<>>, _LengthSize, Acc) ->
  lists:reverse(Acc);
split_h264_frame(<<Size:16, NAL:Size/binary, Rest/binary>>, 2, Acc) ->
  split_h264_frame(Rest, 2, [NAL|Acc]);
split_h264_frame(<<Size:32, NAL:Size/binary, Rest/binary>>, 4, Acc) ->
  split_h264_frame(Rest, 4, [NAL|Acc]).









seq(Headers) ->
  proplists:get_value(<<"Cseq">>, Headers, 1).

to_s(undefined) -> undefined;
to_s(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
to_s(List) when is_list(List) -> List;
to_s(Int) when is_integer(Int) -> integer_to_list(Int);
to_s(Bin) when is_binary(Bin) -> Bin.


reply(Code, Headers, #rtsp{} = RTSP) ->
  reply(Code, Headers, undefined, #rtsp{} = RTSP).

reply(Code, Headers, Body, #rtsp{socket = Socket, session = Session} = RTSP) ->
  Headers1 = case Session of
    undefined -> Headers;
    _ -> [{'Session', Session}|Headers]
  end,
  RTSP1 = RTSP,
  Headers2 = case Body of
    undefined -> Headers1;
    _ -> [{'Content-Length', iolist_size(Body)}, {'Content-Type', <<"application/sdp">>}|Headers1]
  end,
  Reply = iolist_to_binary(["RTSP/1.0 ", Code, <<"\r\n">>, [[to_s(K),": ",to_s(V),"\r\n"] || {K,V} <- Headers2], <<"\r\n">>,
  case Body of
    undefined -> <<>>;
    _ -> Body
  end]),
  io:format(">>>>>> RTSP OUT (~p:~p) >>>>>~n~s~n", [?MODULE, ?LINE, Reply]),
  gen_tcp:send(Socket, Reply),
  RTSP1.
