-module(mpegts_reader_tests).
-compile(export_all).

-include_lib("erlmedia/include/aac.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/mpegts_psi.hrl").
-include_lib("eunit/include/eunit.hrl").





read_http_test_() ->
  {setup, 
    fun() ->
      error_logger:delete_report_handler(error_logger_tty_h),
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
    inets:stop(httpd,Pid)
  end,[
  {"test_have_media_info", fun test_have_media_info/0}
  ]}.


test_have_media_info() ->
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
  monotone_read_frames(0),
  ok.


monotone_read_frames(PrevDTS) ->
  receive
    #video_frame{dts = DTS} -> 
      ?assertMatch(Delta when Delta >= 0, round(DTS - PrevDTS)),
      monotone_read_frames(DTS)
  after
    100 -> ok
  end.



send_udp(<<Chunk:188/binary, Rest/binary>>, UDP) ->
  gen_udp:send(UDP, {127,0,0,1}, 9090, Chunk),
  timer:sleep(1),
  send_udp(Rest, UDP);

send_udp(_, UDP) ->
  gen_udp:close(UDP),
  ok.


read_udp_test1() ->
  {ok, Bin} = file:read_file("../test/fixtures/fileSequence0.ts"),
  {ok, Reader} = mpegts_reader:start_link([{consumer,self()},{url, "udp://127.0.0.1:9090"}]),


  proc_lib:spawn_link(fun() ->
    {ok, UDP} = gen_udp:open(0),
    send_udp(Bin, UDP)
  end),
  ?assertEqual(ok, gen_server:call(Reader, connect)),
  receive
    #media_info{} -> ok;
    #video_frame{} -> error(video_frame_before_media_info)
  after
    500 -> error(timeout)
  end,
  monotone_read_frames(0),
  ok.


