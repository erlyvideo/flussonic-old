-module(rtsp_socket).



-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([stop/1]).

%% Output API (server mode)
-export([start_link/4]).
-export([run/4]).
-export([bind_udp/0]).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("sdp.hrl").

-define(PT_H264, 96).
-define(PT_AAC, 97).
-define(PT_PCMA, 8).
-define(PT_PCMU, 0).
   


%% Input API (client mode)
-export([start_link/1]).
-export([call/3]).
-export([add_channel/4, sync/3]).

-export([to_hex/1, digest_auth/5]).


-export([decode_rtp/2]).

-define(RTCP_SR, 200).
-define(RTCP_RR, 201).
-define(RTCP_SD, 202).
-define(YEARS_70, 2208988800).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.
-define(YEARS_100, 3155673600).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.


-define(KEEPALIVE, 3000).

start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

stop(Proto) ->
  gen_server:call(Proto, stop).

call(Proto, Request, RequestHeaders) ->
  Ref = erlang:make_ref(),
  Proto ! {request, Ref, Request, RequestHeaders},
  receive
    {response, Ref, Code, Headers, Body} ->
      {ok, Code, Headers, Body};
    {'DOWN', _, _, Proto, Reason} ->
      throw({rtsp, exit, Reason})
  after
    10000 ->
      erlang:exit(Proto, kill),
      throw({rtsp, timeout, Request})
  end.

add_channel(Proto, Channel, StreamInfo, Transport) ->
  gen_server:call(Proto, {add_channel, Channel, StreamInfo, Transport}).

sync(Proto, Channel, Sync) ->
  Seq = proplists:get_value(seq, Sync),
  Time = proplists:get_value(rtptime, Sync),
  gen_server:call(Proto, {sync, Channel, Seq, Time}).




-record(rtsp, {
  socket,
  seq = 1,
  url,
  buffer = <<>>,
  session,


  % Here goes reading part
  auth_type = undefined,
  auth_info = "",
  dump = true,

  consumer,
  last_request,
  get_parameter,
  auth,

  % Never ever reorder these fields, because chan1 offset is basic for tricky calculations
  chan1, 
  chan2,
  rtp_chan1,
  rtp_chan2,
  rtcp_chan1,
  rtcp_chan2,


  % Output part

  peer_addr,
  callback,
  timeout,
  args,
  media_info,
  media,


  flow_type = stream :: stream | file,

  paused = false,

  ticker,

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
  a_scale,

  options = []
}).




-record(rtp, {
  channel,
  rtp_port,
  rtcp_port,
  ssrc,
  seq,
  ntp,
  last_sr_at,
  timecode,
  wall_clock,
  decoder,
  codec,
  timescale
}).




start_link(ListenerPid, Socket, _Transport, [Callback, Args]) ->
  {ok, RTSP} = proc_lib:start_link(rtsp_socket, run, [ListenerPid, Socket, Callback, Args]),
  {ok, RTSP}.


run(ListenerPid, Socket, Callback, Args) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),
  {ok, State, Timeout} = rtsp_socket:init([Socket, Callback, Args]),
  gen_server:enter_loop(rtsp_socket, [], State, Timeout).



init([Socket, Callback, Args]) ->
  inet:setopts(Socket, [{active,once},{packet,raw}]),
  Timeout = 10000,
  {ok, {PeerAddr,_}} = inet:peername(Socket),
  {ok, #rtsp{socket = Socket, callback = Callback, args = Args, timeout = Timeout, peer_addr = PeerAddr}, Timeout};


init([Options]) ->
  {consumer, Consumer} = lists:keyfind(consumer, 1, Options),
  erlang:monitor(process, Consumer),
  self() ! connect,
  URL = proplists:get_value(url, Options),
  Dump = case proplists:get_value(log_error, Options) of
    false -> false;
    _ -> proplists:get_value(dump_rtsp, Options, true)
  end,
  GetParameter = proplists:get_value(get_parameter, Options, true),
  {ok, #rtsp{consumer = Consumer, url = URL, dump = Dump, get_parameter = GetParameter, options = Options}}.





handle_call({sync, Channel, Seq, Timecode}, _From, #rtsp{} = RTSP) ->
  case element(#rtsp.chan1 + Channel, RTSP) of
    Chan = #rtp{decoder = Decoder} ->
      Decoder1 = rtp_decoder:sync(Decoder, [{seq,Seq},{rtptime,Timecode}]),
      Chan1 = Chan#rtp{decoder = Decoder1, seq = Seq, timecode = Timecode, wall_clock = 0},
      {reply, ok, setelement(#rtsp.chan1 + Channel, RTSP, Chan1)};
    undefined ->
      {reply, {error, no_channel}, RTSP}
  end;

handle_call(stop, _From, #rtsp{} = RTSP) ->
  {stop, normal, ok, RTSP};

handle_call({add_channel, Channel, #stream_info{codec = Codec, timescale = Scale} = StreamInfo, Transport}, _From, #rtsp{} = RTSP) ->
  Decoder = rtp_decoder:init(StreamInfo),
  Chan = #rtp{channel = Channel, decoder = Decoder, codec = Codec, timescale = Scale},
  case Transport of
    tcp -> 
      {reply, ok, setelement(#rtsp.chan1 + Channel, RTSP, Chan)};
    udp ->
      {ok, RTP, RTCP} = rtsp_socket:bind_udp(),
      {ok, RTPport} = inet:port(RTP),
      {ok, RTCPport} = inet:port(RTCP),
      Chan1 = Chan#rtp{rtp_port = RTPport, rtcp_port = RTCPport},
      RTSP1 = setelement(#rtsp.chan1 + Channel, RTSP, Chan1),
      RTSP2 = setelement(#rtsp.rtp_chan1 + Channel, RTSP1, RTP),
      RTSP3 = setelement(#rtsp.rtcp_chan1 + Channel, RTSP2, RTCP),
      {reply, {ok, {RTPport, RTCPport}}, RTSP3}
  end;



handle_call(#video_frame{} = Frame, From, #rtsp{} = RTSP) ->
  gen_server:reply(From, ok),
  {noreply, RTSP1} = handle_info(Frame, RTSP),
  {noreply, RTSP1};

handle_call(Call, _From, #rtsp{timeout = Timeout} = RTSP) ->
  {reply, {error, {unknown_call, Call}}, RTSP, Timeout}.


% handle_info({tcp, Socket, <<$$, _>> = Bin}, #rtsp{timeout = Timeout} = RTSP) ->
  


handle_info(timeout, RTSP) ->
  {stop, normal, RTSP};

handle_info(#video_frame{}, #rtsp{paused = true} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{dts = DTS} = Frame, #rtsp{first_dts = undefined} = RTSP) ->
  handle_info(Frame, RTSP#rtsp{first_dts = DTS});

handle_info(#video_frame{content = metadata}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info(#video_frame{flavor = config}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info(#media_info{}, #rtsp{} = RTSP) ->
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




handle_info(connect, #rtsp{socket = undefined, url = URL, get_parameter = GetParameter, options = Options} = RTSP) when URL =/= undefined ->
  {_, AuthInfo, Host, Port, _, _} = http_uri2:parse(URL),
  put(host,{Host,Port}),
  % For RTSP capturing we need to overwrite host and port from url but leave proper url
  % because we connect to local proxy
  {ConnectHost, ConnectPort} = proplists:get_value(hostport, Options, {Host, Port}),
  io:format("Connect to ~p:~p~n", [ConnectHost, ConnectPort]),
  {ok, Socket} = case gen_tcp:connect(ConnectHost, ConnectPort, [binary, {active,false}, {send_timeout, 10000}, {recbuf, 1024*1024}]) of
  % {ok, Socket} = case gen_tcp:connect("localhost", 3300, [binary, {active,false}, {send_timeout, 10000}]) of
    {ok, Sock} -> {ok, Sock};
    {error, Error} ->
      lager:error("Failed to connect to \"~s\": ~p", [URL, Error]),
      throw({stop, normal, RTSP})
  end,

  {Auth, AuthType} = case AuthInfo of
    [] -> {fun empty_auth/2, undefined};
    _ -> {fun(_Req, _Url) -> "Authorization: Basic "++binary_to_list(base64:encode(AuthInfo))++"\r\n" end, basic}
  end,
  % Auth = fun empty_auth/2, 
  % AuthType = undefined,
  CleanURL = re:replace(URL, "^rtsp://(.+@)?(.*)$", "rtsp://\\2", [{return, list}]),
  erlang:send_after(?KEEPALIVE, self(), send_rr),
  case GetParameter of
    true -> erlang:send_after(?KEEPALIVE, self(), keepalive);
    false -> ok
  end,
  {noreply, RTSP#rtsp{url = CleanURL, socket = Socket, auth_info = AuthInfo, auth = Auth, auth_type = AuthType}};

handle_info({request, Ref, Request, RequestHeaders}, #rtsp{socket = Socket} = RTSP) ->
  RTSP1 = send(RTSP, Request, RequestHeaders),
  inet:setopts(Socket, [{active,once},{packet,raw}]),
  {noreply, RTSP1#rtsp{last_request = {Ref, Request, RequestHeaders}}};

handle_info(keepalive, #rtsp{socket = Socket, dump = Dump} = RTSP) ->
  RTSP2 = send(RTSP#rtsp{dump = false}, 'GET_PARAMETER', []),
  inet:setopts(Socket, [{active,once},{packet,raw}]),
  erlang:send_after(?KEEPALIVE*3, self(), keepalive),
  {noreply, RTSP2#rtsp{dump = Dump}};


handle_info(send_rr, #rtsp{chan1 = undefined, chan2 = undefined} = RTSP) ->
  erlang:send_after(2000, self(), send_rr),
  {noreply, RTSP};

handle_info(send_rr, #rtsp{} = RTSP) ->
  self() ! {send_rr, 1},
  erlang:send_after(1000, self(), {send_rr, 2}),
  erlang:send_after(?KEEPALIVE*5, self(), send_rr),
  {noreply, RTSP};


handle_info({send_rr, N}, #rtsp{chan1 = Chan1, chan2 = Chan2, socket = Socket} = RTSP) ->
  Chan = case N of
    1 -> Chan1;
    2 -> Chan2
  end,
  case Chan of
    #rtp{ssrc = SSRC, channel = Channel, seq = Seq, ntp = LSR, last_sr_at = LastSRAt} when SSRC + Seq + LSR > 0 ->
      Delay = (timer:now_diff(os:timestamp(), LastSRAt) * 65536) div 1000000,
    % <<1:2, 0:1, Count:5, ?RTCP_RR, Length:16, StreamId:32, FractionLost, LostPackets:24, MaxSeq:32, Jitter:32, LSR:32, DLSR:32>>.
      RR = <<2:2, 0:1, 1:5, ?RTCP_RR, 7:16, SSRC:32, SSRC:32, 0:32, Seq:32, 0:32, (LSR bsr 16):32, Delay:32>>,
      % ?D({rr,Channel*2+1, RR}),
      gen_tcp:send(Socket, [<<$$, (Channel*2 + 1), (size(RR)): 16>>, RR]);
    _ ->
      ok    
  end,
  {noreply, RTSP};

handle_info({udp, S, _, _, RTP}, #rtsp{rtp_chan1 = S, chan1 = Chan, consumer = Consumer} = RTSP) ->
  {ok, Chan_, Frames} = decode_rtp(RTP, Chan),
  [Consumer ! Frame || Frame <- Frames],
  inet:setopts(S, [{active,once}]),
  {noreply, RTSP#rtsp{chan1 = Chan_}};

handle_info({udp, S, _, _, RTP}, #rtsp{rtp_chan2 = S, chan2 = Chan, consumer = Consumer} = RTSP) ->
  {ok, Chan_, Frames} = decode_rtp(RTP, Chan),
  [Consumer ! Frame || Frame <- Frames],
  inet:setopts(S, [{active,once}]),
  {noreply, RTSP#rtsp{chan2 = Chan_}};

handle_info({udp, S, _, _, RTP}, #rtsp{rtcp_chan1 = S1, rtcp_chan2 = S2} = RTSP) when S == S1 orelse S == S2 ->
  RTSP1 = decode_rtcp(RTP, RTSP),
  inet:setopts(S, [{active,once}]),
  {noreply, RTSP1};

handle_info({tcp, Socket, Bin}, #rtsp{buffer = Buffer} = RTSP) ->
  {ok, RTSP1, Rest} = handle_input_tcp(RTSP, <<Buffer/binary, Bin/binary>>),
  inet:setopts(Socket, [{active,once}]),
  {noreply, RTSP1#rtsp{buffer = Rest}};

handle_info({tcp_closed, _Socket}, #rtsp{url = URL} = RTSP) ->
  lager:info("RTSP server ~s just closed socket", [URL]),
  {stop, normal, RTSP};

handle_info({tcp_error, _Socket, Error}, #rtsp{url = URL} = RTSP) ->
  lager:warning("RTSP socket ~s closed with error ~p", [URL, Error]),
  {stop, normal, RTSP};

handle_info({'DOWN', _, _, _Consumer, _}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP}.





handle_input_tcp(#rtsp{socket = _S, dump = NeedToDump} = RTSP, Bin) ->
  case rtsp:read(Bin) of
    {ok, {rtsp, rtp, ChannelId, undefined, RTP}, Rest} ->
      handle_input_tcp(handle_input_rtp(ChannelId, RTP, RTSP), Rest);
    {ok, {rtsp, response, {Code, _Status}, Headers, Body} = Response, Rest} ->
      Dump = NeedToDump andalso RTSP#rtsp.last_request =/= undefined,
      if Dump -> lager:info("<<<<<< RTSP IN <<<<<~n~s~n", [rtsp:dump(Response)]);
      true -> ok end,    
      handle_input_tcp(handle_response(Code, Headers, Body, RTSP), Rest);
    {ok, {rtsp, request, {Method, URL_}, Headers, Body} = Request, Rest} ->
      if NeedToDump -> lager:info("<<<<<< RTSP IN <<<<<~n~s~n", [rtsp:dump(Request)]);
      true -> ok end,    
      Len = size(URL_)-1,
      URL = case URL_ of <<URL__:Len/binary, "/">> -> URL__; _ -> URL_ end,
      RTSP1 = handle_request(Method, URL, Headers, Body, RTSP),
      handle_input_tcp(RTSP1, Rest);
    more ->
      {ok, RTSP, Bin};
    {more, _Bytes} ->
      % {ok, Bin2} = gen_tcp:recv(S, Bytes, ?TIMEOUT),
      {ok, RTSP, Bin};
    {error, desync} ->
      lager:error("RTSP desync on ~s", [RTSP#rtsp.url]),
      throw({stop, normal, RTSP})
  end.




decode_rtcp(RTCP, #rtsp{} = RTSP) ->
  try decode_rtcp0(RTCP, RTSP)
  catch
    throw:#rtsp{} = RTSP1 -> RTSP1
  end.

decode_rtcp0(<<2:2, _:6, ?RTCP_SR, _Length:16, SSRC:32, NTP:64, Timecode:32, _PktCount:32, _OctCount:32, _/binary>>, #rtsp{} = RTSP) ->
  Id = case element(#rtsp.chan1, RTSP) of
    #rtp{ssrc = SSRC} -> 0;
    _ ->
      case element(#rtsp.chan2, RTSP) of
        #rtp{ssrc = SSRC} -> 1;
        _ -> throw(RTSP)
      end
  end,
  Chan = element(#rtsp.chan1 + Id, RTSP),

  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  Chan1 = Chan#rtp{
    ntp = NTP, timecode = Timecode, wall_clock = WallClock, last_sr_at = os:timestamp()
  },

  RTSP1 = setelement(#rtsp.chan1 + Id, RTSP, Chan1),
  RTSP1;

decode_rtcp0(_RTCP, #rtsp{} = RTSP) ->
  RTSP.




handle_input_rtp(ChannelId, RTP, RTSP) when ChannelId rem 2 == 1 ->
  RTSP1 = decode_rtcp(RTP, RTSP),
  RTSP1;

handle_input_rtp(ChannelId, RTP, #rtsp{consumer = Consumer} = RTSP) ->
  % <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>
  ChannelId_ = ChannelId div 2,
  ChannelId_ == 0 orelse ChannelId_ == 1 orelse throw({stop, {unknown_rtp_channel, ChannelId}, RTSP}),
  Chan1 = element(#rtsp.chan1 + ChannelId_, RTSP),
  RTSP1 = case Chan1 of
    undefined ->
      % ?D({unknown_rtp,ChannelId}),
      RTSP;
    _ ->
      {ok, Chan2, Frames} = decode_rtp(RTP, Chan1),
      % if length(Frames) > 0 andalso ChannelId =/= 0 -> ?D({ChannelId, size(RTP), length(Frames), 
      %   [{C,round(T)} || #video_frame{codec = C, dts = T} <- Frames]}); true -> ok end,

      [Consumer ! Frame || Frame <- Frames],
      % <<_:32, TC:32, _/binary>> = RTP,
      % [lager:info("~4s ~8s ~B ~B", [Codec, Flavor, round(DTS), TC]) || #video_frame{codec = Codec, flavor = Flavor, dts = DTS, body = Body} <- Frames],
      setelement(#rtsp.chan1 + ChannelId_, RTSP, Chan2)
  end,
  RTSP1.











tcp_send(#rtsp{socket = Socket} = RTSP, IOList) ->
  case gen_tcp:send(Socket, IOList) of
    ok -> ok;
    {error, closed} -> throw({stop, normal, RTSP});
    {error, timeout} -> throw({stop, normal, RTSP});
    {error, Error} -> throw({stop, {tcp,Error}, RTSP})
  end.



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
    #video_frame{codec = aac, body = Body} -> [Body|receive_aac(Count-1)];
    {'$gen_call', From, #video_frame{codec = aac, body = Body}} -> gen_server:reply(From, ok), [Body|receive_aac(Count-1)]
  after
    150 -> []
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
      reply(401, [{"Www-Authenticate", "Basic realm=\"Flussonic Streaming Server\""}], RTSP);
    {error, no_media_info} ->
      reply(404, [], RTSP);
    {error, enoent} ->
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
  ContentType = rtsp:header('content-type', Headers),
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
  rtsp:header(cseq, Headers).

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






%%
%%
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%
%%


handle_response(_Code, _Headers, _Body, #rtsp{last_request = undefined} = RTSP) ->
  RTSP;

handle_response(401 = Code, Headers, Body, #rtsp{auth_type = Auth, auth_info = AuthInfo, consumer = Pid} = RTSP) when Auth =/= digest ->
  {Ref, Request, RequestHeaders} = RTSP#rtsp.last_request,
  case parse_auth_headers(Headers) of
    undefined ->
      Pid ! {response, Ref, Code, Headers, Body},
      RTSP#rtsp{last_request = undefined};
    [digest|Digest] -> 
      [Username, Password] = string:tokens(AuthInfo, ":"),
      DigestAuth = fun(ReqName, URL) ->
        digest_auth(Digest, Username, Password, URL, ReqName)
      end,
      RTSP1 = send(RTSP#rtsp{auth_type = digest, auth = DigestAuth}, Request, RequestHeaders),
      RTSP1
  end;

handle_response(Code, Headers, Body, #rtsp{consumer = Pid, last_request = {Ref,_,_}} = RTSP) ->
  RTSP2 = case proplists:get_value(<<"Session">>, Headers) of
    undefined -> RTSP;
    SessToken -> RTSP#rtsp{session = hd(binary:split(SessToken, <<";">>))}
  end,
  Pid ! {response, Ref, Code, Headers, Body},
  RTSP2#rtsp{last_request = undefined}.





decode_rtp(<<_RTPHeader:8, _Marker:1, _:7, Seq:16, _Timecode:32, _SSRC:32, _/binary>> = RTP, #rtp{decoder = Decoder1} = Chan1) ->
  <<Version:2, _Padding:1, Extension:1, CC:4>> = <<_RTPHeader>>,
  Version == 2 orelse error({invalid_rtp_version, Version}),
  Extension == 0 orelse error(extension_not_supported),
  CC == 0 orelse error(cc_not_supported),
  % ?D({rtp, Version, Padding, Extension, CC}),

  {ok, Decoder2, Frames} = try rtp_decoder:decode(RTP, Decoder1)
  catch
    Class:Error ->
      lager:error("Error decoding RTP: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
      erlang:raise(Class, Error, erlang:get_stacktrace()),
      {ok, Decoder1, []}
  end,
  % lager:info("rtp channel:~B, marker:~B, seq:~B, tc:~B, ssrc:~B, dts:~w",
  %   [Chan1#rtp.channel, _Marker,Seq,_Timecode, _SSRC, [round(D) || #video_frame{dts = D} <- Frames]]),
  {ok, Chan1#rtp{decoder = Decoder2, seq = Seq}, Frames}.






parse_auth_headers(Headers) ->
  case rtsp:header(<<"Www-Authenticate">>, Headers) of
    undefined -> undefined;
    WwwAuthenticate -> parse_auth_header(WwwAuthenticate)
  end.

parse_auth_header(<<"Digest ", Header/binary>>) ->
  Parts = [begin
    {match, [K,V]} = re:run(Part, "^\\s?(\\w+)=\\\"?([^\\\"]*)\\\"?$", [{capture,all_but_first,binary}]),
    {well_known_auth_key(K), V}
  end || Part <- binary:split(Header, <<",">>, [global])],
  [digest | Parts].
well_known_auth_key(<<"realm">>) -> realm;
well_known_auth_key(<<"nonce">>) -> nonce;
well_known_auth_key(<<"qop">>) -> qop;
well_known_auth_key(K) -> K.



send(#rtsp{socket = Socket, seq = Seq, url = URL, session = Session, auth = Auth} = RTSP, Command, Headers) ->
  SessionHeader = case Session of
    undefined -> "";
    _ -> case proplists:get_value(session,Headers) of
      false -> "";
      _ -> [<<"Session: ">>, Session, <<"\r\n">>]
    end
  end,
  RealURL = proplists:get_value(url, Headers, URL),
  Call =  io_lib:format("~s ~s RTSP/1.0\r\nCSeq: ~B\r\n~s~s~s\r\n", [Command, RealURL, Seq, Auth(Command, RealURL), SessionHeader,
    [io_lib:format("~s: ~s\r\n", [K,V]) || {K,V} <- Headers, K =/= url, K =/= session]]),
  dump_out(RTSP, Call),
  case gen_tcp:send(Socket, Call) of
    ok -> RTSP#rtsp{seq = Seq + 1};
    {error, Error} -> throw({stop, {error, {Command, URL, Error}}, RTSP})
  end.



dump_out(#rtsp{dump = true}, Call) -> io:format(">>>>>> RTSP OUT (~p:~p) >>>>>~n~s~n", [?MODULE, ?LINE, Call]);
dump_out(_, _) -> ok.




empty_auth(_Method, _URL) ->
  "".

hex(C) when 0 =< C andalso C =< 9 -> C + $0;
hex(C) when 10 =< C andalso C =< 15 -> C - 10 + $a.
to_hex(Bin) -> << <<(hex(C div 16)), (hex(C rem 16))>> || <<C>> <= Bin >>.


digest_auth(Digest, Username, Password, URL, Request) ->
  Realm = proplists:get_value(realm, Digest),
  Nonce = proplists:get_value(nonce, Digest),
  % CNonce = to_hex(crypto:md5(iolist_to_binary([Username, ":erlyvideo:", Password]))),
  % CNonce = <<>>,
  % NonceCount = <<"00000000">>,
  _Qop = proplists:get_value(qop, Digest),

  % <<"auth">> == Qop orelse erlang:throw({unsupported_digest_auth, Qop}),
  HA1 = to_hex(crypto:md5(iolist_to_binary([Username, ":", Realm, ":", Password]))),
  HA2 = to_hex(crypto:md5(io_lib:format("~s:~s", [Request, URL]))),
  Response = to_hex(crypto:md5(iolist_to_binary([HA1, ":", Nonce, ":", HA2]))),


  DigestAuth = io_lib:format("Authorization: Digest username=\"~s\", realm=\"~s\", nonce=\"~s\", uri=\"~s\", response=\"~s\"\r\n",
  [Username, Realm, Nonce, URL, Response]),
  iolist_to_binary(DigestAuth).


parse_auth_header_test() ->
  ?assertEqual([digest, {realm, <<"AXIS_00408CA51334">>},{nonce,<<"001f1b29Y164788a2dfdbe4340f02b48a0f23fdd381085">>}, {<<"stale">>, <<"FALSE">>}],
    parse_auth_header(<<"Digest realm=\"AXIS_00408CA51334\", nonce=\"001f1b29Y164788a2dfdbe4340f02b48a0f23fdd381085\", stale=FALSE">>)).

digest_auth1_test() ->
  ?assertEqual(<<"Authorization: Digest username=\"admin\", realm=\"Avigilon-12045784\", "
    "nonce=\"dh9U5wffmjzbGZguCeXukieLz277ckKgelszUk86230000\", "
    "uri=\"rtsp://admin:admin@94.80.16.122:554/defaultPrimary0?streamType=u\", response=\"99a9e6b080a96e25547b9425ff5d68bf\"\r\n">>,
  digest_auth([{realm, <<"Avigilon-12045784">>}, {nonce, <<"dh9U5wffmjzbGZguCeXukieLz277ckKgelszUk86230000">>}, {qop, <<"auth">>}], 
    "admin", "admin", "rtsp://admin:admin@94.80.16.122:554/defaultPrimary0?streamType=u", "OPTIONS")).

digest_auth2_test() ->
  ?assertEqual(<<"Authorization: Digest username=\"root\", realm=\"AXIS_00408CA51334\", "
    "nonce=\"001f187aY315978eceda072f7ffdde87041d6cc0fd9d11\", "
    "uri=\"rtsp://axis-00408ca51334.local.:554/axis-media/media.amp\", response=\"64847b496c6778f3743f0a883e22e305\"\r\n">>,
  digest_auth([{realm, <<"AXIS_00408CA51334">>}, {nonce, <<"001f187aY315978eceda072f7ffdde87041d6cc0fd9d11">>}, {qop, <<"auth">>}],
    "root", "toor", "rtsp://axis-00408ca51334.local.:554/axis-media/media.amp", "DESCRIBE")).


parse_auth_headers_test() ->
  ?assertEqual([digest, {realm, <<"AXIS_00408CDD2171">>}, {nonce, <<"0001d0fdY3300574b60dc5f86d983d0ca0c97050fd3392">>}, {<<"stale">>,<<"FALSE">>}],
    parse_auth_headers([{'Www-Authenticate', <<"Digest realm=\"AXIS_00408CDD2171\", nonce=\"0001d0fdY3300574b60dc5f86d983d0ca0c97050fd3392\", stale=FALSE">>},
      {'Www-Authenticate', <<"Basic realm=\"AXIS_00408CDD2171\"">>}])).





























