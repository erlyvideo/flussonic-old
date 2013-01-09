-module(mpegts_reader_tests).
-compile(export_all).

-include_lib("erlmedia/include/aac.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("eunit/include/eunit.hrl").





read_http_test_() ->
  TestSpec = {setup, 
    fun() ->
      inets:start(),
      inets:start(httpd,[
        {server_root,"../test"},
        {port,9090},
        {server_name,"test_server"},
        {document_root,"../test/fixtures"},
        {modules,[mod_alias,mod_range, mod_auth, mod_actions, mod_dir, mod_get, mod_head]}
      ])
  end, 
  fun({ok,Pid}) ->
    error_logger:delete_report_handler(error_logger_tty_h),    
    inets:stop(httpd,Pid),
    application:stop(inets),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end, [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
      lists:prefix("httptest_", atom_to_list(F))]},

  case file:read_file_info("../test/fixtures") of
    {ok, _} -> TestSpec;
    {error, _} -> []
  end.


httptest_have_media_info() ->
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
      ?assertEqual(a, Else)
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


udp_test_() ->
  case file:read_file_info("../test/fixtures") of
    {ok, _} ->
      [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
      lists:prefix("udptest_", atom_to_list(F))];
    {error, _} -> []
  end.

udptest_unicast() ->
  check_udp("127.0.0.1:9090").

udptest_multicast() ->
  check_udp("239.1.2.3:5060").


check_udp(URL) ->
  [Host, Port] = string:tokens(URL, ":"),
  {ok, Bin} = file:read_file("../test/fixtures/fileSequence0.ts"),
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




dont_send_empty_mi_test() ->
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()}]),
  <<Header:376/binary, _Skip:376/binary, VideoTS:23312/binary, _/binary>> = mpegts_decoder_tests:only_video_mpegts(),
  NoConfigTS = <<Header/binary, VideoTS/binary>>,

  Reader ! {input_data, socket, NoConfigTS},
  ?assertEqual(undefined, fetch_frames()),
  close_reader(Reader),
  ok.



refresh_media_info_test() ->
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()}]),

  VideoTS = mpegts_decoder_tests:only_video_mpegts(),
  Reader ! {input_data, socket, VideoTS},
  ?assertMatch(#media_info{streams = [#stream_info{codec = h264, track_id = 1, config = C}]} when is_binary(C), fetch_frames()),

  AllTS = mpegts_decoder_tests:all_mpegts(),
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
    10 -> MI1
  end.


close_reader(Reader) ->
  receive
    {'$gen_call', From, #video_frame{}} -> gen_server:reply(From, ok), close_reader(Reader);
    _Else -> close_reader(Reader)
  after
    10 -> erlang:exit(Reader, normal)
  end.


















