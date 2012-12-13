-module(flu_file_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("inets/include/httpd.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/hds.hrl").
-compile(export_all).





flu_file_test_() ->
  CommonTests =   [
    {with, [fun test_hds_manifest/1]}
    ,{with, [fun test_hds_lang_segment/1]}
    ,{with, [fun test_hds_segment/1]}
  ],
  Tests = case file:read_file_info("../../hls") of
    {ok, _} -> [{with, [fun test_hls_playlist/1]},{with, [fun test_hls_segment/1]}] ++ CommonTests;
    {error, _} -> CommonTests
  end,
  {foreach,
  fun() -> setup_flu_file("bunny.mp4") end,
  fun teardown_flu_file/1,
  Tests}.

setup_flu_file(Path) ->
  error_logger:delete_report_handler(error_logger_tty_h),
  application:start(gen_tracker),
  gen_tracker_sup:start_tracker(flu_files),
  {ok, File} = flu_file:start_link(Path, [{root, "../../../priv"}]),
  unlink(File),
  % lager:set_loglevel(lager_console_backend, notice),
  {none,File}.


teardown_flu_file({none, File}) ->
  erlang:exit(File, shutdown),
  application:stop(gen_tracker),
  % lager:set_loglevel(lager_console_backend, info),
  ok.


test_hds_segment({_,File}) ->
  ?assertMatch({ok, <<_Size:32, "mdat", _FLV/binary>>}, flu_file:hds_segment(File, 4)),
  {ok, <<_Size:32, "mdat", FLV/binary>>} = flu_file:hds_segment(File, 4),
  Frames = flv:read_all_frames(FLV),
  ?assertMatch(_ when length(Frames) > 10, Frames),

  ?assertMatch([#video_frame{content = metadata}, #video_frame{content = video},
    #video_frame{content = audio}|_], Frames),

  Audio = [Frame || #video_frame{content = audio, flavor = frame} = Frame <- Frames],
  Ats = [DTS || #video_frame{dts = DTS} <- Audio],
  Video = [Frame || #video_frame{content = video} = Frame <- Frames],
  Vts = [DTS || #video_frame{dts = DTS} <- Video],
  ?assertMatch(Audio when length(Audio) > 10, Audio),
  ?assertMatch(Video when length(Video) > 10, Video),

  ?assertMatch([#video_frame{flavor = config}, #video_frame{flavor = keyframe}|_], Video),

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),
  ok.


test_hds_lang_segment({_,File}) ->
  ?assertMatch({ok, <<_Size:32, "mdat", _FLV/binary>>}, flu_file:hds_lang_segment(File, <<"2">>, 4)),
  {ok, <<_Size:32, "mdat", FLV/binary>>} = flu_file:hds_lang_segment(File, <<"2">>, 4),
  Frames = flv:read_all_frames(FLV),
  ?assertMatch(_ when length(Frames) > 10, Frames),

  Audio = [Frame || #video_frame{content = audio, flavor = frame} = Frame <- Frames],
  Ats = [DTS || #video_frame{dts = DTS} <- Audio],
  Video = [Frame || #video_frame{content = video} = Frame <- Frames],
  Meta = [Frame || #video_frame{content = metadata} = Frame <- Frames],
  
  ?assertEqual([], Video),
  ?assertEqual([], Meta),
  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ok.

test_hls_playlist({_,File}) ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hls_playlist(File)).

test_hls_segment({_,File}) ->
  ?assertMatch({ok, IOlist} when is_list(IOlist), flu_file:hls_segment(File, 2)),
  {ok, IOlist} = flu_file:hls_segment(File, 2),
  Bin = iolist_to_binary(IOlist),
  Pids = lists:usort([Pid || <<16#47, _:3, Pid:13, _:185/binary>> <= Bin]),
  ?assertEqual([0, 201, 202, 4095], Pids),
  {ok, Frames} = mpegts_decoder:decode_file(Bin),
  ?assert(length(Frames) > 10),
  ok.

test_hds_manifest({_,File}) ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hds_manifest(File)).




flu_video_only_file_test_() ->
  CommonTests =   [
    {with, [fun test_hds_video_manifest/1]}
    ,{with, [fun test_hds_video_segment/1]}
  ],
  Tests = case file:read_file_info("../../hls") of
    {ok, _} -> [{with, [fun test_hls_playlist/1]},{with, [fun test_hls_video_segment/1]}] ++ CommonTests;
    {error, _} -> CommonTests
  end,
  {foreach,
  fun() -> setup_flu_file("video_only.mp4") end,
  fun teardown_flu_file/1,
  Tests}.


test_hds_video_manifest({_,File}) ->
  Result = flu_file:hds_manifest(File),
  ?assertMatch({ok, Bin} when is_binary(Bin), Result),
  {ok, Bin} = Result,
  ?assertMatch({_,_}, binary:match(Bin, <<"media streamId=\"stream1\" url=\"hds/tracks-1/\" ">>)).



test_hls_video_segment({_,File}) ->
  ?assertMatch({ok, IOlist} when is_list(IOlist), flu_file:hls_segment(File, 2)),
  ok.

test_hds_video_segment({_,File}) ->
  ?assertMatch({ok, <<_Size:32, "mdat", _FLV/binary>>}, flu_file:hds_segment(File, 4)),
  {ok, <<_Size:32, "mdat", FLV/binary>>} = flu_file:hds_segment(File, 4),
  Frames = flv:read_all_frames(FLV),
  ?assertMatch(_ when length(Frames) > 10, Frames),
  ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%          MBR HDS FILE TESTS %%%%%%%%%%%%%%%%%




mbr_hds_file_test_() ->
  MbrTests = [
    {with, [fun test_mbr_hds_manifest/1]}
    ,{with, [fun test_mbr_first_track_segment/1]}
    ,{with, [fun test_mbr_lang_segment/1]}
  ],
  Tests = case file:read_file_info("../../../priv/mbr.mp4") of
    {ok, _} -> MbrTests;
    {error, _} -> []
  end,
  {foreach,
  fun() -> setup_flu_file("mbr.mp4") end,
  fun teardown_flu_file/1,
  Tests}.


test_mbr_hds_manifest({_,File}) ->
  {ok, Bin} = flu_file:hds_manifest(File),
  Result = parsexml:parse(Bin),
  ?assertMatch({<<"manifest">>, _, _Content}, Result),
  {<<"manifest">>, _, Content} = Result,
  Medias = [{<<"media">>, Attr,C} || {<<"media">>, Attr,C} <- Content],
  {<<"media">>, MediaAttrs, _Media} = hd(Medias),
  ?assertEqual(<<"hds/tracks-1,4/">>, proplists:get_value(<<"url">>, MediaAttrs)),
  % ?debugFmt("media: ~p / ~p", [MediaAttr, Media]),
  % 5 medias: 3 with video and 2 with alternate audio
  ?assertMatch(Len when Len == 5, length(Medias)).

test_mbr_first_track_segment({_,File}) ->
  {ok, HDS} = flu_file:hds_segment(File, 2, [1,4]),
  Frames = flv:read_all_frames(HDS),
  ?assertMatch(Len when Len > 400 andalso Len < 500, length(Frames)),
  Video = [F || #video_frame{content = video} = F <- Frames],
  Audio = [F || #video_frame{content = audio} = F <- Frames],
  ?assertMatch(VLen when VLen > 20, length(Video)),
  ?assertMatch(ALen when ALen > 20, length(Audio)),
  % ?debugFmt("total:~p,video:~p,audio:~p",[length(Frames),length(Video),length(Audio)]),
  ok.

test_mbr_lang_segment({_,File}) ->
  {ok, HDS} = flu_file:hds_segment(File, 2, [4]),
  Frames = flv:read_all_frames(HDS),
  % ?assertMatch(Len when Len > 300 andalso Len < 400, length(Frames)),
  Video = [F || #video_frame{content = video} = F <- Frames],
  Audio = [F || #video_frame{content = audio} = F <- Frames],
  ?assertMatch(VLen when VLen == 0, length(Video)),
  ?assertMatch(ALen when ALen > 300 andalso ALen < 350, length(Audio)),
  % ?debugFmt("total:~p,video:~p,audio:~p",[length(Frames),length(Video),length(Audio)]),
  ok.


mbr_hds_file1_test_() ->
  MbrTests = [
    {with, [fun test_mbr_hds_manifest1/1]}
  ],
  Tests = case file:read_file_info("../../../priv/mbr1.mp4") of
    {ok, _} -> MbrTests;
    {error, _} -> []
  end,
  {foreach,
  fun() -> setup_flu_file("mbr1.mp4") end,
  fun teardown_flu_file/1,
  Tests}.


test_mbr_hds_manifest1({_,File}) ->
  {ok, Bin} = flu_file:hds_manifest(File),
  Result = parsexml:parse(Bin),
  ?assertMatch({<<"manifest">>, _, _Content}, Result),
  {<<"manifest">>, _, Content} = Result,
  Medias = [{<<"media">>, Attr,C} || {<<"media">>, Attr,C} <- Content],
  {<<"media">>, MediaAttrs, _Media} = hd(Medias),
  ?assertEqual(<<"hds/tracks-1,2/">>, proplists:get_value(<<"url">>, MediaAttrs)),
  % ?debugFmt("media: ~p / ~p", [MediaAttr, Media]),
  % 5 medias: 3 with video and 2 with alternate audio
  ?assertMatch(Len when Len == 4, length(Medias)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%          MBR HLS FILE TESTS %%%%%%%%%%%%%%%%%


mbr_hls_file_test_() ->
  MbrTests =   [
    {with, [fun test_mbr_hls_root_playlist/1]}
    ,{with, [fun test_mbr_hls_playlist/1]}
    ,{with, [fun test_mbr_hls_segment/1]}
    % ,{with, [fun test_mbr_lang_segment/1]}
  ],
  Tests = case file:read_file_info("../../hls") of
    {ok, _} -> MbrTests;
    {error, _} -> []
  end,

  {foreach,
  fun() -> setup_flu_file("mbr.mp4") end,
  fun teardown_flu_file/1,
  Tests}.


test_mbr_hls_root_playlist({_,File}) ->
  {ok, Bin} = flu_file:hls_mbr_playlist(File),
  % ?debugFmt("bin: ~p",[Bin]),
  Rows = binary:split(Bin, <<"\n">>, [global]),
  URLs = [Row || <<S,_/binary>> = Row <- Rows, S =/= $#],
  Medias = [Row || <<"#EXT-X-MEDIA",_/binary>> = Row <- Rows],
  ?assertMatch(_ when length(URLs) == 3, URLs),
  ?assertMatch(_ when length(Medias) == 2, Medias),
  ok.

test_mbr_hls_playlist({_,File}) ->
  {ok, Bin} = flu_file:hls_playlist(File, [2,4]),
  Rows = binary:split(Bin, <<"\n">>, [global]),
  URLs = [Row || <<S,_/binary>> = Row <- Rows, S =/= $#],
  ?assertMatch(UrlCount when UrlCount == 10, length(URLs)).


test_mbr_hls_segment({_,File}) ->
  {ok, _Bin} = flu_file:hls_segment(File, root, 3, [2,4]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%          HTTP FILE TESTS %%%%%%%%%%%%%%%%%



http_file_test_() ->
  Spec = {foreach,
    fun setup/0,
    fun teardown/1,
    % [{with, [fun ?MODULE:F/1]} || F <- TestFunctions]
    [
      {"test_local_http_file_playlist", fun test_local_http_file_playlist/0},
      {"test_local_http_file_segment", fun test_local_http_file_segment/0},
      {"test_access_http_file", fun test_access_http_file/0}
    ]
  },

  case file:read_file_info("../../http_file") of
    {ok, _} -> Spec;
    {error, _} -> []
  end.



setup() ->
  inets:start(),
  {ok, Httpd} = inets:start(httpd,[
    {server_root,"../test"},
    {port,9090},
    {server_name,"test_server"},
    {document_root,"../../../priv"},
    {modules,[mod_alias,mod_range, ?MODULE, mod_auth, mod_actions, mod_dir, mod_get, mod_head]}
  ]),
  error_logger:delete_report_handler(error_logger_tty_h),

  http_file:start(),

  start_flu(),
  set_config([{file, "http_vod", "http://localhost:9090/"}]),
  {ok, Httpd}.

start_flu() ->
  application:start(gen_tracker),
  gen_tracker_sup:start_tracker(flu_files),
  application:start(flussonic),
  application:start(ranch),
  application:start(cowboy),
  ok.

set_config(Env) ->
  {ok, Conf} = flu_config:parse_config(Env,undefined),
  application:set_env(flussonic, config, Conf),
  (catch cowboy:stop_listener(fake_http)),
  cowboy:start_http(fake_http, 3, 
    [{port,5555}],
    [{dispatch,[{'_',flu_config:parse_routes(Conf)}]}]
  ), 
  ok.


teardown({ok, Httpd}) ->
  application:stop(http_file),
  inets:stop(httpd, Httpd),
  cowboy:stop_listener(fake_http),
  application:stop(ranch),
  ok.


test_local_http_file_playlist() ->
  {ok, File} = flussonic_sup:start_flu_file(<<"bunny.mp4">>, [{root, <<"http://localhost:9090/">>}]),
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hls_playlist(File)),
  ok.

test_local_http_file_segment() ->
  {ok, File} = flussonic_sup:start_flu_file(<<"bunny.mp4">>, [{root, <<"http://localhost:9090/">>}]),
  ?assertMatch({ok, Bin} when is_list(Bin), flu_file:hls_segment(File, 2)),
  ok.



test_access_http_file() ->
  Result = http_stream:request_body("http://localhost:5555/http_vod/bunny.mp4/index.m3u8",[{keepalive,false}]),
  ?assertMatch({ok,{_,200,_, _}}, Result),
  {ok,{_,200,_,Body}} = Result,
  Segments = [Row || <<"hls/segment", _/binary>> = Row <- binary:split(Body, <<"\n">>,[global])],
  ?assertMatch(_ when length(Segments) > 10, Segments),
  ok.


do(#mod{request_uri = URI} = Mod) ->
  try do0(Mod)
  catch
    Class:Error -> 
      ?debugFmt("test http(~s) ~p:~p~n~p~n",[URI, Class, Error, erlang:get_stacktrace()]),
      erlang:raise(Class, Error, erlang:get_stacktrace())
  end.


do0(Mod) ->
  case handle_test_req(Mod) of
    false ->
      {proceed, Mod#mod.data};
    Else ->
      Else
  end.


handle_test_req(#mod{absolute_uri = _URI} = _Mod) ->
  % ?debugFmt("Unknown uri ~p",[URI]),
  false.






flu_file_http_test_() ->
  {foreach,
  fun() ->
    start_flu(),
    set_config([{file, "vod", "../../../priv"}]),
    ok
  end,
  fun(_) ->
    application:stop(ranch),
    application:stop(flussonic),
    ok
  end, [
    {"test_flu_hds_no_segment", fun test_flu_hds_no_segment/0}
    ,{"test_answer_404_on_no_file", fun test_answer_404_on_no_file/0}
    ,{"test_answer_404_on_no_file_with_auth", fun test_answer_404_on_no_file_with_auth/0}
  ]}.


test_flu_hds_no_segment() ->
  Result = http_stream:request_body("http://127.0.0.1:5555/vod/bunny.mp4/hds/0/Seg0-Frag100",[{keepalive,false}]),
  ?assertMatch({error, {http_code,404}}, Result).

test_answer_404_on_no_file() ->
  Result = http_stream:request_body("http://127.0.0.1:5555/vod/bunny10.mp4/manifest.f4m",[{keepalive,false},{no_fail,true}]),
  ?assertMatch({ok, {_, 404, _, _}}, Result).

test_answer_404_on_no_file_with_auth() ->
  set_config([{file, "vod", "../../../priv", [{sessions, "http://127.0.0.1:5555/index.html"}]},{root, "../../../wwwroot"}]),

  Result1 = http_stream:request_body("http://127.0.0.1:5555/vod/bunny10.mp4/manifest.f4m",[{keepalive,false}]),
  ?assertMatch({error, {http_code, 403}}, Result1),

  Result2 = http_stream:request_body("http://127.0.0.1:5555/vod/bunny10.mp4/manifest.f4m?token=a",[{keepalive,false},{no_fail,true}]),
  ?assertMatch({ok, {_, 404, _, _}}, Result2),
  ok.

