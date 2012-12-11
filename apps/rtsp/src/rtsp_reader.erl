-module(rtsp_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("sdp.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([start_link/2, media_info/1]).

-export([init/1, handle_info/2, handle_call/3, terminate/2]).


start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).


media_info(RTSP) ->
  gen_server:call(RTSP, media_info, 15000).

-record(rtsp, {
  url,
  rtp_mode,
  content_base,
  consumer,
  media_info,
  proto,
  options
}).

init([URL, Options]) ->
  % {_HostPort, Path} = http_uri2:extract_path_with_query(URL),
  {consumer, Consumer} = lists:keyfind(consumer, 1, Options),
  erlang:monitor(process, Consumer),
  % erlang:send_after(5000, self(), teardown),
  RTPMode = proplists:get_value(rtp, Options, tcp),
  self() ! work,
  {ok, #rtsp{url = URL, content_base = URL, options = Options, consumer = Consumer, rtp_mode = RTPMode}}.


handle_info(work, #rtsp{} = RTSP) ->
  RTSP1 = try_read(RTSP),
  {noreply, RTSP1};

handle_info(#video_frame{codec = Codec} = Frame, #rtsp{consumer = Consumer} = RTSP) when 
  Codec == h264 orelse Codec == aac orelse Codec == mp3 ->
  % #video_frame{content = Content, codec = Codec, flavor = Flavor, dts = DTS} = Frame,
  % io:format("~6s ~4s ~10s ~B~n", [Content, Codec, Flavor, round(DTS)]),
  Consumer ! Frame,
  {noreply, RTSP};

handle_info(#video_frame{}, #rtsp{} = RTSP) ->
  {noreply, RTSP};

handle_info({'DOWN', _, _, _,_}, #rtsp{} = RTSP) ->
  {stop, normal, RTSP};

handle_info(teardown, #rtsp{proto = Proto} = RTSP) ->
  rtsp_protocol:call(Proto, 'TEARDOWN', []),
  {stop, RTSP, normal}.



handle_call(media_info, _From, #rtsp{media_info = MediaInfo} = RTSP) ->
  {reply, {ok, MediaInfo}, RTSP}.


terminate(_,#rtsp{}) ->
  ok.


try_read(#rtsp{url = URL} = RTSP) ->
  try try_read0(RTSP)
  catch
    throw:{rtsp, exit, normal} ->
      throw({stop, normal, RTSP});
    throw:{rtsp, restart, RTSP1} ->
      try_read(RTSP1);
    throw:{rtsp, Error, Reason} ->
      ?ERR("Failed to read from \"~s\": ~p:~240p", [URL, Error, Reason]),
      throw({stop, normal, RTSP})
  end.

try_read0(#rtsp{url = URL, options = Options, rtp_mode = RTPMode} = RTSP) ->
  {ok, Proto} = rtsp_protocol:start_link([{consumer, self()}, {url, URL}|Options]),
  unlink(Proto),
  Ref = erlang:monitor(process, Proto),

  {ok, 200, _, _} = rtsp_protocol:call(Proto, 'OPTIONS', []),
  {ok, DescribeCode, DescribeHeaders, SDP} = rtsp_protocol:call(Proto, 'DESCRIBE', [{'Accept', <<"application/sdp">>}]),
  DescribeCode == 401 andalso throw({rtsp, denied, 401}),
  DescribeCode == 404 andalso throw({rtsp, not_found, 404}),
  DescribeCode == 200 orelse throw({rtsp, invalid_describe, DescribeCode}),
  ContentBase = parse_content_base(DescribeHeaders, URL, RTSP#rtsp.content_base),

  MI1 = #media_info{streams = Streams1} = sdp:decode(SDP),
  MI2 = MI1#media_info{streams = [S || #stream_info{content = Content, codec = Codec} = S <- Streams1,
    (Content == audio orelse Content == video) andalso Codec =/= undefined]},
  MediaInfo = MI2,
  lists:foldl(fun(#stream_info{options = Opt, track_id = TrackId} = StreamInfo, N) ->
    Control = proplists:get_value(control, Opt),
    Track = control_url(ContentBase, Control),
    Transport = case RTPMode of
      tcp ->
        rtsp_protocol:add_channel(Proto, TrackId-1, StreamInfo, tcp),
        io_lib:format("RTP/AVP/TCP;unicast;interleaved=~B-~B", [N, N+1]);
      udp ->
        {ok, {RTPport, RTCPport}} = rtsp_protocol:add_channel(Proto, TrackId-1, StreamInfo, udp),
        io_lib:format("RTP/AVP;unicast;client_port=~B-~B", [RTPport, RTCPport])
    end,
    {ok, SetupCode, _, _} = rtsp_protocol:call(Proto, 'SETUP', [{'Transport', Transport},{url, Track}]),
    case SetupCode of
      406 when RTPMode == tcp ->
        erlang:demonitor(Ref, [flush]),
        (catch rtsp_protocol:stop(Proto)),
        throw({rtsp, restart, RTSP#rtsp{rtp_mode = udp}});
      200 -> 
        ok;
      _ ->
        throw({rtsp, failed_setup, {SetupCode, Track, RTPMode}})
    end,
    N + 2
  end, 0, MediaInfo#media_info.streams),

  {ok, PlayCode, PlayHeaders, _} = rtsp_protocol:call(Proto, 'PLAY', []),
  PlayCode == 200 orelse throw({rtsp, rejected_play, PlayCode}),
  RtpInfo = parse_rtp_info(PlayHeaders),

  [rtsp_protocol:sync(Proto, N, Sync) || {N, Sync} <- lists:zip(lists:seq(0,length(RtpInfo)-1),RtpInfo)],

  RTSP#rtsp{content_base = ContentBase, media_info = MediaInfo, proto = Proto}.


% Axis cameras have "rtsp://192.168.0.1:554/axis-media/media.amp/trackID=1" in SDP
control_url(_ContentBase, "rtsp://" ++ _ = ControlUrl) -> ControlUrl;
control_url(ContentBase, ControlUrl) -> ContentBase ++ ControlUrl.

parse_content_base(Headers, URL, OldContentBase) ->
  case proplists:get_value('Content-Base', Headers) of
    undefined -> OldContentBase;
    NewContentBase -> % Here we must handle important case when Content-Base is given with local network
      URL1 = case re:run(NewContentBase, "rtsp://([^/]+)(/.*)$", [{capture,all_but_first,list}]) of
        {match, [_Host, BasePath]} ->
          {match, [Host, _Path]} = re:run(URL, "rtsp://([^/]+)(/.*)$", [{capture,all_but_first,list}]),
          "rtsp://" ++ Host ++ BasePath;
        nomatch ->
          case lists:last(URL) of
            $/ -> URL ++ NewContentBase;
            _ -> URL ++ "/" ++ NewContentBase
          end
      end,
      URL2 = re:replace(URL1, "^rtsp://(.+@)?(.*)$", "rtsp://\\2", [{return, list}]),
      case lists:last(URL2) of
        $/ -> URL2;
        _ -> URL2 ++ "/"
      end
  end.


parse_rtp_info(Headers) ->
  case proplists:get_value(<<"Rtp-Info">>, Headers) of
    undefined -> case proplists:get_value(<<"RTP-Info">>, Headers) of
      undefined -> [];
      S -> parse_rtp_info_header(S)
    end;
    S ->parse_rtp_info_header(S)
  end. 


parse_rtp_info_header(String) when is_binary(String) ->
  parse_rtp_info_header(binary_to_list(String));

parse_rtp_info_header(String) when is_list(String) ->
  {ok, Re} = re:compile(" *([^=]+)=([^\\r ]*)"),
  F = fun(S) ->
    {match, [_, K, V]} = re:run(S, Re, [{capture, all, list}]),
    Key = list_to_existing_atom(K),
    Value = case Key of
      seq -> list_to_integer(V);
      rtptime -> list_to_integer(V);
      _ -> V
    end,
    {Key, Value}
  end,
  [[F(S1) || S1 <- string:tokens(S, ";")] || S <- string:tokens(String, ",")].


parse_content_base_test_() ->
 [
  ?_assertEqual("rtsp://75.130.113.168:1025/11/", 
    parse_content_base([{'Content-Base', "rtsp://75.130.113.168:1025/11/"}],
      "rtsp://admin:admin@75.130.113.168:1025/11/", "rtsp://admin:admin@75.130.113.168:1025/11/"))
  ,?_assertEqual("rtsp://75.130.113.168:1025/ipcamera/",
    parse_content_base([{'Content-Base', "ipcamera"}],
      "rtsp://admin:admin@75.130.113.168:1025", "rtsp://admin:admin@75.130.113.168:1025"))
 ].

parse_rtp_info_test_() ->
  [
    ?_assertEqual([[{url,"rtsp://75.130.113.168:1025/11/trackID=0"},{seq,0},{rtptime,3051549469}]], 
      parse_rtp_info_header("url=rtsp://75.130.113.168:1025/11/trackID=0;seq=0;rtptime=3051549469 "))
  ].

