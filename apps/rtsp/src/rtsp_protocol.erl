-module(rtsp_protocol).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").

-export([start_link/1]).

-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([call/3, stop/1]).
-export([add_channel/3, sync/3]).

-export([to_hex/1, digest_auth/5]).

-export([collect_headers/1, collect_body/2, collect_headers_and_body/1]).

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



add_channel(Proto, Channel, StreamInfo) ->
  gen_server:call(Proto, {add_channel, Channel, StreamInfo}).

sync(Proto, Channel, Sync) ->
  Seq = proplists:get_value(seq, Sync),
  Time = proplists:get_value(rtptime, Sync),
  gen_server:call(Proto, {sync, Channel, Seq, Time}).

-record(rtsp, {
  consumer,
  socket,
  seq = 1,
  url,
  session,
  auth_type = undefined,
  auth_info = "",
  auth,
  chan1,
  chan2,
  chan3,
  chan4,
  chan5,
  chan6,
  dump = true
}).

-record(rtp, {
  channel,
  stream_id,
  seq,
  ntp,
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
  Dump = proplists:get_value(dump_rtsp, Options, true),
  {ok, #rtsp{consumer = Consumer, url = URL, dump = Dump}}.



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

handle_call({add_channel, Channel, #stream_info{codec = Codec, timescale = Scale} = StreamInfo}, _From, #rtsp{} = RTSP) ->
  Decoder = rtp_decoder:init(StreamInfo),
  Chan = #rtp{channel = Channel, decoder = Decoder, codec = Codec, timescale = Scale},
  {reply, ok, setelement(#rtsp.chan1 + Channel, RTSP, Chan)}.



handle_info(connect, #rtsp{socket = undefined, url = URL} = RTSP) when URL =/= undefined ->
  {_, AuthInfo, Host, Port, _, _} = http_uri2:parse(URL),
  {ok, Socket} = case gen_tcp:connect(Host, Port, [binary, {active,false}, {send_timeout, 10000}]) of
    {ok, Sock} -> {ok, Sock};
    {error, Error} ->
      ?ERR("Failed to connect to \"~s\": ~p", [URL, Error]),
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
  erlang:send_after(?KEEPALIVE, self(), keepalive),
  {noreply, RTSP#rtsp{url = CleanURL, socket = Socket, auth_info = AuthInfo, auth = Auth, auth_type = AuthType}};

handle_info({request, Ref, Request, RequestHeaders}, #rtsp{socket = Socket, consumer = Consumer} = RTSP) ->
  {RTSP2, Code, Headers, Body} = call_with_authenticate(RTSP, Request, RequestHeaders),
  Consumer ! {response, Ref, Code, Headers, Body},
  inet:setopts(Socket, [{active,once}]),
  {noreply, RTSP2};

handle_info(keepalive, #rtsp{socket = Socket, dump = Dump} = RTSP) ->
  {RTSP2, _, _, _} = call_with_authenticate(RTSP#rtsp{dump = false}, 'GET_PARAMETER', []),
  inet:setopts(Socket, [{active,once},{packet,raw}]),
  erlang:send_after(?KEEPALIVE, self(), keepalive),
  {noreply, RTSP2#rtsp{dump = Dump}};


handle_info(send_rr, #rtsp{chan1 = undefined, chan2 = undefined} = RTSP) ->
  erlang:send_after(2000, self(), send_rr),
  {noreply, RTSP};

handle_info(send_rr, #rtsp{chan1 = Chan1, chan2 = Chan2, socket = Socket} = RTSP) ->
  % <<1:2, 0:1, Count:5, ?RTCP_RR, Length:16, StreamId:32, FractionLost, LostPackets:24, MaxSeq:32, Jitter:32, LSR:32, DLSR:32>>.
  [begin
    RR = <<1:2, 0:6, ?RTCP_RR, 16:16, StreamId:32, 0:32, Seq:32, 0:32, LSR:32, 0:32>>,
    gen_tcp:send(Socket, [<<$$, (Channel*2 + 1), (size(RR)): 16>>, RR])
  end || #rtp{stream_id = StreamId, channel = Channel, seq = Seq, ntp = LSR} <- [Chan1, Chan2], StreamId + Seq + LSR > 0],
  erlang:send_after(5000, self(), send_rr),
  {noreply, RTSP};

handle_info({tcp, Socket, <<$$, _/binary>> = Bin}, #rtsp{} = RTSP) ->
  {ok, RTSP1, <<>>} = read_rtp_packets(Bin, RTSP),
  inet:setopts(Socket, [{active,once}]),
  {noreply, RTSP1};

handle_info({tcp, Socket, Bin}, #rtsp{} = RTSP) ->
  ?DBG("read strange RTSP/RTP data: ~240p", [Bin]),
  RTSP1 = case binary:split(Bin, <<$$>>) of
    [Bin] -> RTSP;
    [_Junk, Rest] ->
      {ok, RTSP_, <<>>} = read_rtp_packets(<<$$, Rest/binary>>, RTSP),
      RTSP_
  end,
  inet:setopts(Socket, [{active,once}]),
  {noreply, RTSP1};


handle_info({tcp_closed, _Socket}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP};

handle_info({'DOWN', _, _, _Consumer, _}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP}.



call_with_authenticate(#rtsp{} = RTSP, Request, RequestHeaders) ->
  RTSP0 = flush_rtp_packets(RTSP),
  RTSP1 = send(RTSP0, Request, RequestHeaders),

  case recv(RTSP1) of
    {#rtsp{auth_type = OldType, auth_info = AuthInfo} = RTSP2, 401, Headers, _} = Response when OldType =/= digest ->
      case parse_auth_headers(Headers) of
        undefined ->
          Response;
        [digest|Digest] -> 
          [Username, Password] = string:tokens(AuthInfo, ":"),
          DigestAuth = fun(ReqName, URL) ->
            digest_auth(Digest, Username, Password, URL, ReqName)
          end,
          call_with_authenticate(RTSP2#rtsp{auth_type = digest, auth = DigestAuth}, Request, RequestHeaders)
      end;
    Response ->
      Response
  end.



read_rtp_packets(<<$$, ChannelId, Length:16, RTCP:Length/binary, Rest/binary>>, #rtsp{} = RTSP) when ChannelId rem 2 == 1 ->
  RTSP1 = case RTCP of
    <<2:2, _:6, ?RTCP_SR, _Length:16, StreamId:32, NTP:64, Timecode:32, _PacketCount:32, _OctetCount:32, _/binary>> ->
      ChannelId_ = ChannelId div 2,
      WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
      Chan = (element(#rtsp.chan1 + ChannelId_, RTSP))#rtp{ntp = NTP, timecode = Timecode, wall_clock = WallClock, stream_id = StreamId},
      setelement(#rtsp.chan1 + ChannelId_, RTSP, Chan);
    _ ->
      RTSP
  end,
  read_rtp_packets(Rest, RTSP1);

read_rtp_packets(<<$$, ChannelId, Length:16, RTP:Length/binary, Rest/binary>>, #rtsp{consumer = Consumer} = RTSP) ->
  % <<_:16, Sequence:16, _/binary>> = RTP,
  ChannelId_ = ChannelId div 2,
  Chan1 = element(#rtsp.chan1 + ChannelId_, RTSP),
  {ok, Chan2, Frames} = decode_rtp(RTP, Chan1),
  [Consumer ! Frame || Frame <- Frames],
  RTSP1 = setelement(#rtsp.chan1 + ChannelId_, RTSP, Chan2),
  read_rtp_packets(Rest, RTSP1);

read_rtp_packets(<<$$, _ChannelId, Length:16, RTP/binary>> = Bin, #rtsp{socket = Socket} = RTSP) ->
  {ok, Rest} = gen_tcp:recv(Socket, Length - size(RTP), 10000),
  read_rtp_packets(<<Bin/binary, Rest/binary>>, RTSP);

read_rtp_packets(<<$$, _/binary>> = Bin, #rtsp{socket = Socket} = RTSP) when size(Bin) < 4 ->
  {ok, Rest} = gen_tcp:recv(Socket, 4 - size(Bin), 10000),
  read_rtp_packets(<<Bin/binary, Rest/binary>>, RTSP);

read_rtp_packets(<<"RTSP", _/binary>> = Bin, RTSP) ->
  {ok, RTSP, Bin};

read_rtp_packets(<<>>, RTSP) ->
  {ok, RTSP, <<>>};

read_rtp_packets(Bin, RTSP) ->
  case binary:split(Bin, <<$$>>) of
    [Bin] -> ?D({bad_data, Bin}), {ok, RTSP, <<>>};
    [_Skip, Rest] -> ?D(resync), read_rtp_packets(<<$$, Rest/binary>>, RTSP)
  end.


decode_rtp(RTP, #rtp{decoder = Decoder1} = Chan1) ->
  {ok, Decoder2, Frames} = rtp_decoder:decode(RTP, Decoder1),
  {ok, Chan1#rtp{decoder = Decoder2}, Frames}.




flush_rtp_packets(#rtsp{socket = Socket} = RTSP) ->
  inet:setopts(Socket, [{active,false},{packet,raw}]),
  receive
    {tcp, Socket, Bin} -> {ok, RTSP1, <<>>} = read_rtp_packets(Bin, RTSP), RTSP1
  after
    0 -> RTSP
  end.




read_response_code(#rtsp{socket = Socket, url = URL} = RTSP) ->
  inet:setopts(Socket, [{packet, line},{active,false}]),
  case gen_tcp:recv(Socket, 0, 10000) of
    {error, Error} ->
      
      throw({stop, {error, {socket_recv, Error, URL}}, RTSP});
    {ok, Bin} ->
      inet:setopts(Socket, [{packet,raw}]),
      case read_rtp_packets(Bin, RTSP) of
        {ok, RTSP1, <<"RTSP", _/binary>> = Line} ->
          case re:run(Line, "RTSP/1.0 (\\d+) .*", [{capture,all_but_first,list}]) of
            {match, [Code_]} -> {list_to_integer(Code_), Line, RTSP1};
            nomatch -> throw({stop, {error, {response_line, Line, URL}}, RTSP1})
          end;
        {ok, RTSP1, <<>>} ->
          read_response_code(RTSP1)
      end
  end.

recv(#rtsp{socket = Socket, dump = NeedToDump, url = URL} = RTSP) ->
  {Code, Dump1, RTSP1} = read_response_code(RTSP),
  {Headers, Dump2} = collect_headers(Socket),
  if NeedToDump ->
  io:format(">>>>>> RTSP IN (~p:~p) >>>>>~n~s~s~n", [?MODULE, ?LINE, Dump1, Dump2]);
  true -> ok end,
  is_list(Headers) orelse throw({stop, {error, {headers, Headers, URL}}, RTSP1}),
  Body = try collect_body(Socket, Headers)
  catch
    throw:{error, Error} -> throw({stop, {error, Error, URL}, RTSP1})
  end,
  if NeedToDump andalso is_binary(Body) -> io:format("~s~n", [Body]); true -> ok end,

  RTSP2 = case proplists:get_value(<<"Session">>, Headers) of
    undefined -> RTSP1;
    SessToken -> RTSP1#rtsp{session = hd(binary:split(SessToken, <<";">>))}
  end,
  {RTSP2, Code, Headers, Body}.


collect_headers_and_body(Socket) ->
  {Headers, Dump} = collect_headers(Socket),
  Body = collect_body(Socket, Headers),
  {Headers, Body, Dump}.


collect_body(Socket, Headers) ->
  inet:setopts(Socket, [{packet, raw}]),
  Body = case proplists:get_value('Content-Length', Headers) of
    undefined -> undefined;
    ContentLength_ ->
      ContentLength = list_to_integer(binary_to_list(ContentLength_)),
      case gen_tcp:recv(Socket, ContentLength, 10000) of
        {ok, Bin} -> Bin;
        {error, Err} -> throw({error, {read_body, ContentLength, Err}})
      end
  end,
  Body.

collect_headers(Socket) ->
  inet:setopts(Socket, [{packet, httph_bin}]),
  collect_headers(Socket, [], []).

collect_headers(Socket, Acc, Dump) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, {http_header, _, Key, _, Value}} ->
      D = io_lib:format("~s: ~s~n", [Key, Value]),
      collect_headers(Socket, [{Key,Value}|Acc], [D|Dump]);
    {ok, http_eoh} -> {lists:reverse(Acc), lists:reverse(Dump)};
    {error, Error} -> {{error, read_headers, Error}, lists:reverse(Dump)}
  end.


parse_auth_headers(Headers) ->
  case proplists:get_value('Www-Authenticate', Headers) of
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



dump_out(#rtsp{dump = false}, _) -> ok;
dump_out(_, Call) -> io:format(">>>>>> RTSP OUT (~p:~p) >>>>>~n~s~n", [?MODULE, ?LINE, Call]).


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
