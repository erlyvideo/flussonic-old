-module(rtsp_socket).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([bind_udp/0]).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("sdp.hrl").

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
  media,

  flow_type = stream :: stream | file,

  paused = false,

  ticker,
  url,

  first_dts,

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


% handle_info({tcp, Socket, <<$$, _>> = Bin}, #rtsp{timeout = Timeout} = RTSP) ->
  


handle_info({tcp, Socket, Bin}, #rtsp{timeout = Timeout} = RTSP) ->
  inet:setopts(Socket, [{active,false},{packet,raw}]),
  Line = skip_rtcp(Bin, Socket),
  case re:run(Line, "^(\\w+) ([^ ]+) (RTSP|HTTP)/1.0", [{capture,all_but_first,binary}]) of
    {match, [Method, URL_, Proto]} ->
      {Headers, _Dump} = rtsp_protocol:collect_headers(Socket),
      Body = rtsp_protocol:collect_body(Socket, Headers),
      inet:setopts(Socket, [{active,once},{packet,line}]),

      % io:format(">>>>>> RTSP IN (~p:~p) >>>>>~n~s~s~n~s~n", [?MODULE, ?LINE, Line, Dump, case Body of undefined -> ""; _ -> Body end]),

      Len = size(URL_)-1,
      URL = case URL_ of <<URL__:Len/binary, "/">> -> URL__; _ -> URL_ end,

      RTSP1 = case Proto of
        <<"RTSP">> -> handle_request(Method, URL, Headers, Body, RTSP);
        <<"HTTP">> -> handle_http(Method, URL, Headers, Body, RTSP)
      end,
      {noreply, RTSP1, Timeout};
    nomatch ->
      inet:setopts(Socket, [{active,once},{packet,line}]),
      {noreply, RTSP, Timeout}
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

handle_info(#video_frame{}, #rtsp{paused = true} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{dts = DTS} = Frame, #rtsp{first_dts = undefined} = RTSP) ->
  handle_info(Frame, RTSP#rtsp{first_dts = DTS});

handle_info(#video_frame{content = metadata}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{flavor = config}, #rtsp{} = RTSP) ->
  {noreply, RTSP};


% interleaved TCP
handle_info(#video_frame{content = video, dts = DTS, pts = PTS} = Frame,
  #rtsp{v_s_rtp = SRTP, v_seq = Seq, first_dts = FDTS} = RTSP) when is_integer(SRTP) ->
  Packets = rtp_packets(Frame#video_frame{dts = DTS - FDTS, pts = PTS - FDTS}, RTSP),
  [tcp_send(RTSP, [$$, SRTP, <<(size(RTP)):16>>, RTP]) || RTP <- Packets],
  {noreply, RTSP#rtsp{v_seq = (Seq + length(Packets)) rem 65536}};

handle_info(#video_frame{content = audio, dts = DTS, pts = PTS} = Frame, 
  #rtsp{a_s_rtp = SRTP, a_seq = Seq, first_dts = FDTS} = RTSP) when is_integer(SRTP) ->
  [RTP] = rtp_packets(Frame#video_frame{dts = DTS - FDTS, pts = PTS - FDTS}, RTSP),
  tcp_send(RTSP, [$$, SRTP, <<(size(RTP)):16>>, RTP]),
  {noreply, RTSP#rtsp{a_seq = (Seq +1) rem 65536}};


% RTP UDP

handle_info(#video_frame{content = video, dts = DTS, pts = PTS} = Frame,
  #rtsp{v_s_rtp = SRTP, v_c_rtp = CRTP, peer_addr = Addr, v_seq = Seq, first_dts = FDTS} = RTSP) when is_port(SRTP) ->
  Packets = rtp_packets(Frame#video_frame{dts = DTS - FDTS, pts = PTS - FDTS}, RTSP),
  [gen_udp:send(SRTP, Addr, CRTP, RTP) || RTP <- Packets],
  {noreply, RTSP#rtsp{v_seq = (Seq + length(Packets)) rem 65536}};

handle_info(#video_frame{content = audio, dts = DTS, pts = PTS} = Frame, 
  #rtsp{a_s_rtp = SRTP, a_c_rtp = CRTP, peer_addr = Addr, a_seq = Seq, first_dts = FDTS} = RTSP) when is_port(SRTP) ->
  [RTP] = rtp_packets(Frame#video_frame{dts = DTS - FDTS, pts = PTS - FDTS}, RTSP),
  gen_udp:send(SRTP, Addr, CRTP, RTP),
  {noreply, RTSP#rtsp{a_seq = (Seq +1) rem 65536}};


handle_info(Info, #rtsp{} = RTSP) ->
  {stop, {unknown_message, Info}, RTSP}.


tcp_send(#rtsp{socket = Socket} = RTSP, IOList) ->
  case gen_tcp:send(Socket, IOList) of
    ok -> ok;
    {error, closed} -> throw({stop, normal, RTSP});
    {error, timeout} -> throw({stop, normal, RTSP});
    {error, Error} -> throw({stop, {tcp,Error}, RTSP})
  end.


skip_rtcp(<<$$, _, Length:16, _Bin:Length/binary, Rest/binary>>, Socket) ->
  skip_rtcp(Rest, Socket);

skip_rtcp(<<$$, _, Length:16, Bin/binary>>, Socket) ->
  {ok, _Rest} = gen_tcp:recv(Socket, Length - size(Bin), 3000),
  <<>>;

skip_rtcp(<<$$, _/binary>> = Bin, Socket) ->
  {ok, Rest} = gen_tcp:recv(Socket, 3, 3000),
  skip_rtcp(<<Bin/binary, Rest/binary>>, Socket);

skip_rtcp(<<>>, _Socket) -> ok;
skip_rtcp(Else, _Socket) -> Else.



rtp_packets(#video_frame{codec = h264, dts = DTS, body = Data, track_id = TrackID}, 
  #rtsp{v_seq = Seq, v_scale = Scale, length_size = LengthSize}) ->
  FUA_NALS = lists:flatmap(fun(NAL) -> h264:fua_split(NAL, 1387) end, split_h264_frame(Data, LengthSize)),

  Nals = FUA_NALS,

  Packets = pack_h264(Nals, round(DTS*Scale), TrackID, Seq),
  Packets;


rtp_packets(#video_frame{codec = aac, dts = DTS, body = Body1, track_id = TrackID}, 
  #rtsp{a_seq = Seq, a_scale = Scale}) ->
  
  Bodies = [Body1|receive_aac(3)],
  AUHeader = [<<(size(Body)):13, 0:3>> || Body <- Bodies],
  AULength = iolist_size(AUHeader)*8,
  Marker = 1,
  RTP = iolist_to_binary([<<2:2, 0:1, 0:1, 0:4, Marker:1, ?PT_AAC:7, Seq:16, (round(DTS*Scale)):32, TrackID:32, 
  AULength:16>>, AUHeader, Bodies]),
  [RTP].
  

receive_aac(0) -> [];
receive_aac(Count) ->
  receive
    #video_frame{codec = aac, body = Body} -> [Body|receive_aac(Count-1)]
  after
    4000 -> error(timeout)
  end.

pack_h264([NAL|Nals], Timecode, TrackID, Seq) ->
  Marker = case Nals of
    [] -> 1;
    _ -> 0
  end,
  [<<2:2, 0:1, 0:1, 0:4, Marker:1, ?PT_H264:7, Seq:16, Timecode:32, TrackID:32, NAL/binary>>|
  pack_h264(Nals, Timecode, TrackID, (Seq+1) rem 65536)];

pack_h264([], _, _, _) -> [].


terminate(_,_) ->
  % ?D({terminate,rtsp}),
  ok.



handle_request(Method, _URL, Headers, _Body, RTSP) when Method == <<"OPTIONS">> orelse Method == <<"GET_PARAMETER">>->
  reply(200, [
    {'Cseq', seq(Headers)}, 
    {<<"Supported">>, <<"play.basic, con.persistent">>},
    {'Date', httpd_util:rfc1123_date()},
    {'Public', "SETUP, TEARDOWN, ANNOUNCE, RECORD, PLAY, OPTIONS, DESCRIBE, GET_PARAMETER"}], RTSP);

handle_request(<<"DESCRIBE">>, URL1, Headers, Body, #rtsp{callback = Callback} = RTSP) ->
  {URL, Auth} = extract_url(URL1),
  % ?D({describe,URL}),
  case Callback:describe(URL, Auth ++ Headers, Body) of
    {error, authentication} ->
      reply(401, [{"WWW-Authenticate", "Basic realm=\"Flussonic Streaming Server\""}], RTSP);
    {error, no_media_info} ->
      reply(404, [], RTSP);
    {ok, #media_info{streams = Streams} = MediaInfo1} ->
      MediaInfo = MediaInfo1#media_info{streams = 
        [S#stream_info{options = []} 
        || #stream_info{content = C} = S <- Streams, C == video orelse C == audio],
        options = []
      },
      SDP = sdp:encode(MediaInfo),


      {A1, A2, A3} = now(),
      Session = integer_to_list((A1*1000000+A2)*1000000+A3),

      AScale = case lists:keyfind(audio, #stream_info.content, Streams) of
        #stream_info{timescale = AScale_} -> AScale_;
        false -> undefined
      end,
      #stream_info{timescale = VScale, params = VideoParams} = lists:keyfind(video, #stream_info.content, Streams),
      #video_params{length_size = LengthSize, nals = Nals} = VideoParams,
      reply(200, [{'Cseq', seq(Headers)},
        {'Date', httpd_util:rfc1123_date()}, {'Expires', httpd_util:rfc1123_date()},
        {'Content-Base', io_lib:format("~s/", [URL])}], SDP, 
        RTSP#rtsp{media_info = MediaInfo, v_scale = VScale, a_scale = AScale, session = Session,
          length_size = LengthSize, nals = Nals, url = URL1})
  end;

handle_request(<<"SETUP">>, URL, Headers, _Body, #rtsp{media_info = #media_info{streams = Streams}} = RTSP) ->
  {match, [Track_]} = re:run(URL, "/track[iI][dD]=(\\d+)", [{capture,all_but_first,list}]),
  TrackId = list_to_integer(Track_),
  Transport = proplists:get_value(<<"Transport">>, Headers),
  case binary:split(Transport, <<";">>, [global]) of
    % [<<"RTP/AVP",_/binary>>, <<"unicast">>, <<"client_port=", _Ports/binary>>|_] ->
    %   reply(400, [{'Cseq', seq(Headers)}], RTSP);
    [<<"RTP/AVP",_/binary>>, <<"unicast">>, <<"client_port=", Ports/binary>>|TransportArgs] ->
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
      OutputMode = case TransportArgs of
        [<<"mode=record">>|_] -> <<";mode=receive">>;
        _ -> <<>>
      end, 
      Reply = iolist_to_binary(io_lib:format("~B-~B", [ServerRTP, ServerRTCP])),
      reply(200, [{'Cseq', seq(Headers)},
        {<<"Transport">>,<<"RTP/AVP;unicast;client_port=",Ports/binary,";server_port=", 
        Reply/binary, OutputMode/binary>>}], RTSP1);
    [<<"RTP/AVP/TCP">>, <<"unicast">>, <<"interleaved=", Interleave/binary>>] ->
      {match, [RTP_, RTCP_]} = re:run(Interleave, "(\\d+)-(\\d+)", [{capture,all_but_first,list}]),
      RTP = list_to_integer(RTP_),
      RTCP = list_to_integer(RTCP_),
      RTSP1 = case lists:keyfind(TrackId, #stream_info.track_id, Streams) of
        #stream_info{content = video} ->
          RTSP#rtsp{v_s_rtp = RTP, v_s_rtcp = RTCP, v_c_rtp = RTP, v_c_rtcp = RTCP};
        #stream_info{content = audio} ->
          RTSP#rtsp{a_s_rtp = RTP, a_s_rtcp = RTCP, a_c_rtp = RTP, a_c_rtcp = RTCP}
      end,
      reply(200, [{'Cseq', seq(Headers)},
        {<<"Transport">>,<<"RTP/AVP/TCP;unicast;interleaved=",Interleave/binary>>}], RTSP1);
    [<<"RTP/AVP/TCP">>, <<"unicast">>, <<"mode=record">>, <<"interleaved=", _Interleave/binary>>] ->
      reply(461, [{'Cseq', seq(Headers)}], RTSP);
      % {match, [RTP_, RTCP_]} = re:run(Interleave, "(\\d+)-(\\d+)", [{capture,all_but_first,list}]),
      % RTP = list_to_integer(RTP_),
      % RTCP = list_to_integer(RTCP_),
      % RTSP1 = case lists:keyfind(TrackId, #stream_info.track_id, Streams) of
      %   #stream_info{content = video} ->
      %     RTSP#rtsp{v_s_rtp = RTP, v_s_rtcp = RTCP, v_c_rtp = RTP, v_c_rtcp = RTCP};
      %   #stream_info{content = audio} ->
      %     RTSP#rtsp{a_s_rtp = RTP, a_s_rtcp = RTCP, a_c_rtp = RTP, a_c_rtcp = RTCP}
      % end,
      % reply(200, [{'Cseq', seq(Headers)},
      %   {'Transport', Transport}], RTSP1);
    _ ->
      reply(461, [{'Cseq', seq(Headers)}], RTSP)
  end;

handle_request(<<"PLAY">>, _URL, Headers, _Body, #rtsp{media = Media, paused = true, flow_type = stream} = RTSP) when is_pid(Media) ->
  reply(200, [{'Cseq', seq(Headers)}], RTSP#rtsp{paused = false});

handle_request(<<"PLAY">>, _URL1, Headers, Body, #rtsp{url = URL1, callback = Callback, peer_addr = Addr, 
  media_info = #media_info{streams = Streams}} = RTSP) ->
  {URL, Auth} = extract_url(URL1),
  case Callback:play(URL, [{ip,inet_parse:ntoa(Addr)}]++ Auth ++ Headers, Body) of
    {ok, {Type, Stream}} ->
      % ?D({got, Type, Stream}),
      erlang:monitor(process, Stream),
      % It is ok here to fetch first config frame, because we know its contents
      % from media_info and already sent to client
      % But we need its time to understand what is the first DTS of stream
      RtpInfo = string:join(lists:map(fun(#stream_info{track_id = Id}) ->
        io_lib:format("url=~s/trackID=~B;seq=0;rtptime=0", [URL, Id])
      end, Streams),","),
      reply(200, [{'Cseq', seq(Headers)}, {"RTP-Info", RtpInfo},{"Range", "npt=0-"},{"Date",httpd_util:rfc1123_date()}], 
        RTSP#rtsp{media = Stream, flow_type = Type});
    {fail, Code, Message} ->
      reply(Code, [{'Cseq', seq(Headers)}], Message, RTSP);
    Reply ->
      throw({stop, {play_disabled,Reply}, RTSP})
  end;

handle_request(<<"TEARDOWN">>, _URL, Headers, _Body, #rtsp{} = RTSP) ->
  RTSP1 = reply(200, [{'Cseq', seq(Headers)}], RTSP),
  throw({stop, normal, RTSP1});


handle_request(<<"PAUSE">>, _URL, Headers, _Body, #rtsp{flow_type = stream} = RTSP) ->
  reply(200, [{'Cseq', seq(Headers)}], RTSP#rtsp{paused = true});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request(<<"ANNOUNCE">>, URL, Headers, Body, #rtsp{callback = Callback} = RTSP) ->
  ContentType = proplists:get_value('Content-Type', Headers),
  ContentType == <<"application/sdp">> orelse erlang:error({unknown_content_type,ContentType}),
  MediaInfo = sdp:decode(Body),
  % ?DBG("MI: ~240p",[MediaInfo]),
  try Callback:announce(URL, Headers, MediaInfo) of
    {ok, Media} ->
      reply(200, [{'Cseq',seq(Headers)}], RTSP#rtsp{media = Media, media_info = MediaInfo})
  catch
    throw:authorization ->
      reply(401, [{'Cseq', seq(Headers)}], RTSP)
  end;


handle_request(<<"RECORD">>, _URL, Headers, _Body, #rtsp{} = RTSP) ->
  reply(200, [{'Cseq',seq(Headers)}], RTSP);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request(_Method, _URL, Headers, _Body, RTSP) ->
  reply("405 Method not allowed", [{'Cseq', seq(Headers)}], RTSP).




handle_http(<<"GET">>, _URL, _Headers, _Body, RTSP) ->
  reply({http, 500}, [
    {'Content-Type', "application/x-rtsp-tunnelled"},
    {'Connection', "close"}
  ], <<"Not supported">>, RTSP).




extract_url(URL1) ->
  {Host, PathQuery} = http_uri2:extract_path_with_query(URL1),
  {Path, Query} = http_uri2:parse_path_query(PathQuery),
  Name = case Path of
    "/live" -> "/"++proplists:get_value("id", Query);
    _ -> Path
  end,
  {"rtsp://"++Host++Name, Query}.









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
  Headers3 = [{'Server', <<"Flussonic (http://erlyvideo.org/) ", (list_to_binary(flu:version()))/binary>>}|Headers2],

  CodeMsg = case Code of
    {http, C} when is_number(C) -> ["HTTP/1.0 ", integer_to_list(C), " ", message(C)];
    {http, C} when is_list(C) -> ["HTTP/1.0 ", C];
    C when is_number(C) -> ["RTSP/1.0 ", integer_to_list(C), " ", message(C)];
    C when is_list(C) -> ["RTSP/1.0 ", C]
  end,

  Reply = iolist_to_binary([CodeMsg, <<"\r\n">>, [[to_s(K),": ",to_s(V),"\r\n"] || {K,V} <- Headers3], <<"\r\n">>,
  case Body of
    undefined -> <<>>;
    _ -> Body
  end]),
  % io:format(">>>>>> RTSP OUT (~p:~p) >>>>>~n~s~n", [?MODULE, ?LINE, Reply]),
  gen_tcp:send(Socket, Reply),
  RTSP1.

message(200) -> "OK";
message(400) -> "Bad Request";
message(401) -> "Unauthorized";
message(404) -> "Not found";
message(406) -> "Not Acceptable";
message(461) -> "Unsupported transport";
message(_) -> "Response".


