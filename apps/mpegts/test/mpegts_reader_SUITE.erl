-module(mpegts_reader_SUITE).
-compile(export_all).

-include_lib("erlmedia/include/aac.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl").




all() ->
  [{group, reader}].

groups() ->
  [{reader, [parallel], [
    read_protected_basic,
    have_media_info,
    udptest_multicast,
    udptest_unicast,
    dont_send_empty_mi,
    refresh_media_info
  ]}].


init_per_suite(Config) ->
  inets:start(),
  DocRoot = code:lib_dir(mpegts,test),
  inets:start(httpd,[
    {server_root,DocRoot},
    {port,9090},
    {server_name,"test_server"},
    {document_root,DocRoot ++ "/fixtures"},
    {modules,[mod_alias,mod_range, mod_auth, ?MODULE, mod_actions, mod_dir, mod_get, mod_head]}
  ]),
  Config.

end_per_suite(Config) ->
  application:stop(inets),
  Config.




do(Mod) ->
  case handle_test_req(Mod) of
    false ->
      {proceed, Mod#mod.data};
    Else ->
      Else
  end.



handle_test_req(#mod{absolute_uri="127.0.0.1:9090/protected", parsed_header = Headers}) ->
  case proplists:get_value("authorization", Headers) of
    "Basic YWRtaW46MTIz" ->
      {proceed, [{response,{response,[{code,200}],"ok\n"}}]};
    _ ->
      {proceed, [{response,{response,[{code,401},{"WWW-Authenticate", "Basic realm=\"mpegts\""}],"forbidden\n"}}]}
  end;

handle_test_req(_Mod) ->
  false.





read_protected_basic(_Config) ->
  Options = [{consumer,self()},{url,"tshttp://admin:123@127.0.0.1:9090/protected"}],
  {ok, State1} = mpegts_reader:init([Options]),
  {reply, ok, _State2} = mpegts_reader:handle_call(connect, from, State1),
  receive {tcp, _, _} -> ok after 5 -> ok end,
  ok.




have_media_info(_Config) ->
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()},{url, "http://127.0.0.1:9090/fileSequence0.ts"}]),
  erlang:monitor(process, Reader),
  ?assertEqual(ok, gen_server:call(Reader, connect)),
  receive
    {'DOWN', _, _, Reader, _} -> error(failed_reader);
    #media_info{} -> ok;
    #video_frame{} -> error(video_frame_before_media_info)
  after
    500 -> error(timeout_media_info)
  end,
  monotone_read_frames(),
  ok.


monotone_read_frames() -> monotone_read_frames(0, 0).

monotone_read_frames(PrevDTS, Count) ->
  receive
    #video_frame{dts = DTS} -> 
      ?assertMatch(Delta when Delta >= 0, round(DTS - PrevDTS)),
      monotone_read_frames(DTS, Count + 1);
    {'$gen_call', From, #video_frame{dts = DTS}} ->
      ?assertMatch(Delta when Delta >= 0, round(DTS - PrevDTS)),
      gen_server:reply(From, ok),
      monotone_read_frames(DTS, Count + 1);
    #media_info{} ->
      monotone_read_frames(PrevDTS, Count);
    Else ->
      error({invalid_message, Else})
  after
    100 -> Count > 40 orelse error(not_enough_monotone_frames)
  end.



send_udp(<<Chunk:1316/binary, Rest/binary>>, UDP) ->
  gen_udp:send(UDP, Chunk),
  timer:sleep(1),
  send_udp(Rest, UDP);

send_udp(Chunk, UDP) ->
  gen_udp:send(UDP, Chunk),
  gen_udp:close(UDP),
  ok.


udptest_unicast(_Config) ->
  check_udp("127.0.0.1:9090").

udptest_multicast(_Config) ->
  check_udp("239.1.2.3:5060").


check_udp(URL) ->
  [Host, Port] = string:tokens(URL, ":"),
  {ok, Bin} = file:read_file(filename:join(code:lib_dir(mpegts,test),"fixtures/fileSequence0.ts")),
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()},{url, "udp://" ++ URL}]),
  ?assertEqual(ok, gen_server:call(Reader, connect)),

  proc_lib:spawn_link(fun() ->
    {ok, UDP} = gen_udp:open(0),
    {ok, Addr} = inet_parse:address(Host),
    gen_udp:connect(UDP, Addr, list_to_integer(Port)),
    send_udp(Bin, UDP)
  end),
  receive
    #media_info{} -> ok;
    #video_frame{} -> error(video_frame_before_media_info)
  after
    500 -> error(timeout)
  end,
  monotone_read_frames(),
  ok.




dont_send_empty_mi(_Config) ->
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()}]),
  <<Header:376/binary, _Skip:376/binary, VideoTS:23312/binary, _/binary>> = mpegts_decoder_SUITE:only_video_mpegts(),
  NoConfigTS = <<Header/binary, VideoTS/binary>>,

  Reader ! {input_data, socket, NoConfigTS},
  ?assertEqual(undefined, fetch_frames()),
  close_reader(Reader),
  ok.



refresh_media_info(_Config) ->
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()}]),

  VideoTS = mpegts_decoder_SUITE:only_video_mpegts(),
  Reader ! {input_data, socket, VideoTS},
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264, track_id = 1, config = C}]} when is_binary(C), fetch_frames()),

  AllTS = mpegts_decoder_SUITE:all_mpegts(),
  Reader ! {input_data, socket, AllTS},
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264, track_id = 1, config = V}, 
    #stream_info{codec = aac, track_id = 2, config = A}]} when is_binary(V) andalso is_binary(A), fetch_frames()),


  close_reader(Reader),
  ok.

fetch_frames() -> fetch_frames(undefined).

fetch_frames(MI1) ->
  receive
    #media_info{} = MI2 -> MI2;
    {'$gen_call', From, #video_frame{}} -> gen_server:reply(From, ok), fetch_frames(MI1);
    Else -> Else
  after
    100 -> MI1
  end.


close_reader(Reader) ->
  receive
    {'$gen_call', From, #video_frame{}} -> gen_server:reply(From, ok), close_reader(Reader);
    _Else -> close_reader(Reader)
  after
    10 -> erlang:exit(Reader, normal)
  end.


















