-module(rtsp_protocol).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").

-export([start_link/1]).

-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([call/3, stop/1]).
-export([add_channel/4, sync/3]).

-export([to_hex/1, digest_auth/5]).


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
  rtcp_chan2
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
  {ok, #rtsp{consumer = Consumer, url = URL, dump = Dump, get_parameter = GetParameter}}.



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
  end.



handle_info(connect, #rtsp{socket = undefined, url = URL, get_parameter = GetParameter} = RTSP) when URL =/= undefined ->
  {_, AuthInfo, Host, Port, _, _} = http_uri2:parse(URL),
  put(host,{Host,Port}),
  {ok, Socket} = case gen_tcp:connect(Host, Port, [binary, {active,false}, {send_timeout, 10000}]) of
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

handle_info({udp, S, _, _, RTP}, #rtsp{rtcp_chan1 = S, chan1 = Chan} = RTSP) ->
  Chan_ = decode_rtcp(RTP, Chan),
  inet:setopts(S, [{active,once}]),
  {noreply, RTSP#rtsp{chan1 = Chan_}};

handle_info({udp, S, _, _, RTP}, #rtsp{rtcp_chan2 = S, chan1 = Chan} = RTSP) ->
  Chan_ = decode_rtcp(RTP, Chan),
  inet:setopts(S, [{active,once}]),
  {noreply, RTSP#rtsp{chan2 = Chan_}};

handle_info({tcp, Socket, Bin}, #rtsp{buffer = Buffer} = RTSP) ->
  {ok, RTSP1, Rest} = handle_input_tcp(RTSP, <<Buffer/binary, Bin/binary>>),
  inet:setopts(Socket, [{active,once}]),
  {noreply, RTSP1#rtsp{buffer = Rest}};

handle_info({tcp_closed, _Socket}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP};

handle_info({tcp_error, _Socket, Error}, #rtsp{url = URL} = RTSP) ->
  lager:warning("RTSP socket ~s closed with error ~p", [URL, Error]),
  {stop, normal, RTSP};

handle_info({'DOWN', _, _, _Consumer, _}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP}.




decode_rtcp(<<2:2, _:6, ?RTCP_SR, _Length:16, SSRC:32, NTP:64, Timecode:32, _PktCount:32, _OctCount:32, _/binary>>, #rtp{} = Chan) ->
  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  Chan#rtp{
    ntp = NTP, timecode = Timecode, wall_clock = WallClock, ssrc = SSRC, last_sr_at = os:timestamp()
  };

decode_rtcp(_, #rtp{} = Chan) ->
  Chan.

handle_input_rtp(ChannelId, RTCP, RTSP) when ChannelId rem 2 == 1 ->
  ChannelId_ = ChannelId div 2,
  Chan = (element(#rtsp.chan1 + ChannelId_, RTSP)),
  Chan1 = decode_rtcp(RTCP, Chan),
  RTSP1 = setelement(#rtsp.chan1 + ChannelId_, RTSP, Chan1),
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


handle_input_tcp(#rtsp{socket = _S, dump = NeedToDump} = RTSP, Bin) ->
  case rtsp:read(Bin) of
    {ok, {rtsp, rtp, ChannelId, undefined, RTP}, Rest} ->
      handle_input_tcp(handle_input_rtp(ChannelId, RTP, RTSP), Rest);
    {ok, {rtsp, response, {Code, _Status}, Headers, Body} = Response, Rest} ->
      Dump = NeedToDump andalso RTSP#rtsp.last_request =/= undefined,
      if Dump -> io:format("<<<<<< RTSP IN (~p:~p) <<<<<~n~s~n", [?MODULE, ?LINE, rtsp:dump(Response)]);
      true -> ok end,    
      handle_input_tcp(handle_response(Code, Headers, Body, RTSP), Rest);
    {ok, {rtsp, request, {Method, URL}, _Headers, _Body}, Rest} ->
      lager:error("Input socket doesn't know how to handle request ~s ~s", [Method, URL]),
      handle_input_tcp(RTSP, Rest);
    more ->
      {ok, RTSP, Bin};
    {more, _Bytes} ->
      % {ok, Bin2} = gen_tcp:recv(S, Bytes, ?TIMEOUT),
      {ok, RTSP, Bin};
    {error, desync} ->
      ?D({rtsp_desync,Bin}),
      throw({stop, normal, RTSP})
  end.


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





decode_rtp(<<_:8, _Marker:1, _:7, Seq:16, _Timecode:32, _/binary>> = RTP, #rtp{decoder = Decoder1} = Chan1) ->
  % ?D({rtp,_Marker,Seq,_Timecode}),

  {ok, Decoder2, Frames} = rtp_decoder:decode(RTP, Decoder1),
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


terminate(_,_) ->
  ok.



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


