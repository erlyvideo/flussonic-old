-module(flu_file_SUITE).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("inets/include/httpd.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/hds.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).





% flu_file_test_() ->
%   CommonTests =   [
%     {with, [fun test_hds_manifest/1]}
%     ,{with, [fun test_hds_fragment/1]}
%     ,{with, [fun test_hds_lang_segment/1]}
%     ,{with, [fun test_hds_missing_segment/1]}
%     ,{with, [fun test_read_gop/1]}
%   ],
%   Tests = case file:read_file_info("../../hls") of
%     {ok, _} -> 
%       [{with, [fun test_hls_playlist/1]},
%       {with, [fun test_hls_segment/1]},
%       {with, [fun test_hls_segment_with_video_track/1]}] ++ CommonTests;
%     {error, _} -> CommonTests
%   end,
%   {foreach,
%   flu_test:setup_(),
%   flu_test:teardown_(),
%   Tests}.





% test_read_gop({_,File}) ->
%   {ok, List} = flu_file:read_gop(<<"none">>, File, 2),
%   ?assertMatch([#video_frame{}|_], List),
%   ok.



all() ->
  [
    {group, mbr},
    {group, mp4},
    {group, flv},
    {group, video_only}
  ].


groups() ->
  [
  {mbr, [parallel], [
    mbr_hds_manifest,
    mbr_first_track_fragment,
    mbr_lang_fragment
    % mbr_hls_lang_segment,
    % mbr_hls
  ]},
  {mp4, [parallel], [
    flu_hls_good_segment,
    flu_hls_no_segment,
    hls_segment_with_video_track,
    answer_404_on_no_file,
    answer_404_on_no_file_with_auth,
    flu_hds_good_manifest,
    flu_hds_good_segment,
    hds_lang_segment,
    hls_segment_with_video_track
    % file_starts_with_client_count
  ]},
  {flv, [parallel], [
    flu_hls_good_segment,
    flu_hds_good_manifest,
    flu_hds_good_segment
  ]},
  {video_only, [parallel], [
    flu_hds_good_manifest,
    hds_video_only_fragment
  ]}
  ].



init_per_suite(Config) ->
  R = flu_test:setup([{log,info}], fun() ->
    flu_test:set_config([
      {file,"vod", "../../priv"},
      {file,"securevod", "../../priv", [{sessions, "http://127.0.0.1:5671/auth/token_a"}]}
    ])
  end),
  [{r,R}|Config].

end_per_suite(Config) ->
  {value,{r,R},Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1.


init_per_group(mp4, Config) ->
  [{path,"bunny.mp4"}|Config];

init_per_group(video_only, Config) ->
  [{path,"video_only.mp4"}|Config];

init_per_group(flv, Config) ->
  [{path, "bunny.flv"}|Config];

init_per_group(_, Config) ->
  Config.


end_per_group(_, Config) ->
  Config.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%          MBR HDS FILE TESTS %%%%%%%%%%%%%%%%%




mbr_hds_manifest(_Config) ->
  {ok, {{200,_}, _, Bin}} = flu_test:request("http://127.0.0.1:5670/vod/mbr.mp4/manifest.f4m",get, [], 1000),

  {<<"manifest">>, _, Content} = parsexml:parse(Bin),
  Medias = [{<<"media">>, Attr,C} || {<<"media">>, Attr,C} <- Content],
  {<<"media">>, MediaAttrs, _Media} = hd(Medias),
  <<"hds/tracks-1,4/">> = proplists:get_value(<<"url">>, MediaAttrs),
  <<"1166">> = proplists:get_value(<<"bitrate">>, MediaAttrs),
  % ?debugFmt("media: ~p / ~p", [MediaAttrs, _Media]),
  % 5 medias: 3 with video and 2 with alternate audio
  length(Medias) == 5 orelse error({invalid_media_count,length(Medias)}).


mbr_first_track_fragment(_Config) ->
  {ok, {{200,_}, _, HDS}} = flu_test:request("http://127.0.0.1:5670/vod/mbr.mp4/hds/tracks-1,4/Seg1-Frag2",get, [], 1000),
  Frames = flv:read_all_frames(iolist_to_binary(HDS)),
  case length(Frames) of
    Len when Len > 400 andalso Len < 500 -> ok;
    Len -> error({bad_number_of_frames,Len})
  end,
  Video = [F || #video_frame{content = video} = F <- Frames],
  Audio = [F || #video_frame{content = audio} = F <- Frames],
  length(Video) > 20 orelse error({too_few_video,length(Video)}),
  length(Audio) > 20 orelse error({too_few_video,length(Audio)}),
  ok.

mbr_lang_fragment(_Config) ->
  {ok, {{200,_}, _, HDS}} = lhttpc:request("http://127.0.0.1:5670/vod/mbr.mp4/hds/tracks-4/Seg1-Frag2",get, [], 1000),
  Frames = flv:read_all_frames(iolist_to_binary(HDS)),
  % ?assertMatch(Len when Len > 300 andalso Len < 400, length(Frames)),
  [] = [F || #video_frame{content = video} = F <- Frames],
  Audio = [F || #video_frame{content = audio} = F <- Frames],
  case length(Audio) of
    Len when Len > 300 andalso Len < 350 -> ok;
    Len -> error({bad_number_of_frames,Len})
  end,
  ok.



mbr_hls(_Config) ->
  ct:fail(not_implemented_yet),
  MbrURL = "http://127.0.0.1:5670/vod/mbr.mp4/mbr.m3u8",
  {ok, {{200,_}, _, MBR}} = flu_test:request(MbrURL,get, [], 1000),
  MbrRows = binary:split(MBR, <<"\n">>, [global]),
  MbrURLs = [Row || <<S,_/binary>> = Row <- MbrRows, S =/= $#],
  MbrMedias = [Row || <<"#EXT-X-MEDIA",_/binary>> = Row <- MbrRows],
  ?assertMatch(_ when length(MbrURLs) == 3, MbrURLs),
  ?assertMatch(_ when length(MbrMedias) == 2, MbrMedias),

  SbrURL = filename:dirname(MbrURL) ++ "/" ++ binary_to_list(hd(MbrURLs)),
  "http://127.0.0.1:5670/vod/mbr.mp4/tracks-1,4/index.m3u8" = SbrURL,
  {ok, {{200,_}, _, SBR}} = flu_test:request(SbrURL,get, [], 1000),
  SbrRows = binary:split(SBR, <<"\n">>, [global]),
  SbrURLs = [Row || <<S,_/binary>> = Row <- SbrRows, S =/= $#],
  ?assertMatch(UrlCount when UrlCount == 10, length(SbrURLs)),

  SegUrl = filename:dirname(SbrURL) ++ "/" ++ binary_to_list(hd(SbrURLs)),
  ?assertEqual("http://127.0.0.1:5670/vod/mbr.mp4/tracks-1,4/hls/segment1.ts", SegUrl),
  {ok, {{200,_}, _, _Segment}} = flu_test:request(SegUrl,get, [], 1000),


  {match, [LangRelURL]} = re:run(hd(MbrMedias), "URI=\\\"([^\"]*)\\\"", [{capture,all_but_first,list}]),
  LangURL = filename:dirname(MbrURL) ++ "/" ++ LangRelURL,
  ?assertEqual("http://127.0.0.1:5670/vod/mbr.mp4/tracks-5/index.m3u8", LangURL),
  {ok, {{200,_}, _, Lang}} = flu_test:request(LangURL,get, [], 1000),
  LangRows = binary:split(Lang, <<"\n">>, [global]),
  LangURLs = [Row || <<S,_/binary>> = Row <- LangRows, S =/= $#],
  ?assertMatch(UrlCount when UrlCount == 10, length(LangURLs)),

  LangSegUrl = filename:dirname(LangURL) ++ "/" ++ binary_to_list(hd(LangURLs)),
  ?assertEqual("http://127.0.0.1:5670/vod/mbr.mp4/tracks-5/hls/segment1.ts", LangSegUrl),
  {ok, {{200,_}, _, _LangSegment}} = flu_test:request(LangSegUrl,get, [], 1000),
  ok.


mbr_hls_lang_segment(_Config) ->
  ct:fail("not implemented yet"),
  {ok, {{200,_}, _, _Bin}} = flu_test:request("http://127.0.0.1:5670/vod/mbr.mp4/hls/tracks-4/segment3.ts",get, [], 1000),
  ok.












flu_hds_good_manifest(Config) ->
  Path = ?config(path,Config),
  {ok, {{200,_},_,Manifest}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/manifest.f4m",get,[],1000),

  XML = parsexml:parse(Manifest),
  {<<"manifest">>, _, Elements} = XML,
  {_, _, [Bootstrap64]} = lists:keyfind(<<"bootstrapInfo">>, 1, Elements),
  Bootstrap = base64:decode(Bootstrap64),
  Atom = mp4:parse_atom(Bootstrap, state),
  {'Bootstrap', Options, _} = Atom,
  ?assertEqual(0, proplists:get_value(live,Options)),

  % Here we check a bug when not duration, but last keyframe time
  % was sent as a file duration in HDS manifest.
  % It is very important that duration of file is bigger than 60 seconds
  case re:run(Path, "\.flv") of
    nomatch -> ?assertMatch(D when D > 60000, proplists:get_value(current_time,Options));
    {match, _} -> ok % FLV is bad at everything, so skip this test
  end,
  ok.




flu_hds_good_segment(Config) ->
  Path = ?config(path,Config),
  {ok, {{200,_},_,Frag}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/hds/0/Seg0-Frag4",get,[],1000),

  <<_Size:32, "mdat", FLV/binary>> = iolist_to_binary(Frag),
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

  case re:run(Path, ".flv") of
    nomatch ->

  ?assertEqual(11883, hd(Ats)),
  ?assertEqual(15744, lists:last(Ats)),
  ?assertEqual(11875, hd(Vts)),
  ?assertEqual(15708, lists:last(Vts)),
  ok;
  {match, _} -> ok end, % skip this test for flv
  ok.





hds_video_only_fragment(_Config) ->
  {ok, {{200,_},_,Seg}} = flu_test:request("http://127.0.0.1:5670/vod/video_only.mp4/hds/0/Seg0-Frag4",get,[],1000),

  <<_Size:32, "mdat", FLV/binary>> = iolist_to_binary(Seg),
  Frames = flv:read_all_frames(FLV),
  ?assertMatch(_ when length(Frames) > 10, Frames),
  [] = [Frame || #video_frame{content = audio} = Frame <- Frames],
  ok.




hds_lang_segment(Config) ->
  Path = ?config(path,Config),
  {ok, {{200,_},_,HDS}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/hds/tracks-2/Seg0-Frag4",get,[],1000),

  <<_Size:32, "mdat", _FLV/binary>> = iolist_to_binary(HDS),
  Frames = flv:read_all_frames(iolist_to_binary(HDS)),
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




flu_hls_good_segment(Config) ->
  Path = ?config(path,Config),
  {ok,{{200,_},_,Bin_}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/hls/segment4.ts",get,[],1000),

  Bin = iolist_to_binary(Bin_),
  Pids = lists:usort([Pid || <<16#47, _:3, Pid:13, _:185/binary>> <= Bin]),
  ?assertEqual([0, 201, 202, 4095], Pids),
  {ok, Frames} = mpegts_decoder:decode_file(Bin),
  ?assert(length(Frames) > 10),
  ok.

hls_segment_with_video_track(Config) ->
  Path = ?config(path,Config),
  {ok, {{200,_},_,Bin_}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/tracks-1/hls/segment2.ts",get,[],1000),

  Bin = iolist_to_binary(Bin_),
  Pids = lists:usort([Pid || <<16#47, _:3, Pid:13, _:185/binary>> <= Bin]),
  [0, 201, 4095] = Pids,
  {ok, Frames} = mpegts_decoder:decode_file(Bin),
  ?assert(length(Frames) > 10),
  AudioFrames = [F || #video_frame{content = audio} = F <- Frames],
  ?assertEqual(0, length(AudioFrames)),
  ok.




flu_hds_no_segment(Config) ->
  Path = ?config(path,Config),
  {ok,{{404,_},_,_}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/hds/0/Seg0-Frag100",get,[],1000).

file_starts_with_client_count(_Config) ->
  [erlang:exit(proplists:get_value(pid,Info),shutdown) || {_,Info} <- flu_file:list()],
  flu_session:delete_all_sessions(),
  Result = http_stream:request_body("http://127.0.0.1:5670/vod/bunny.mp4/hds/0/Seg0-Frag4",[{keepalive,false},{no_fail,true}]),
  ?assertMatch({ok, {_, 200, _, _}}, Result),
  Info = proplists:get_value(<<"vod/bunny.mp4">>, flu_file:list()),
  ?assertEqual(1, proplists:get_value(client_count,Info)),
  ok.




flu_hls_no_segment(Config) ->
  Path = ?config(path,Config),
  {ok, {{404, _}, _, _}} = flu_test:request("http://127.0.0.1:5670/vod/"++Path++"/hls/segment100.ts",get,[],1000).

answer_404_on_no_file(_Config) ->
  {ok, {{404,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/vod/bunny10.mp4/manifest.f4m",get,[],1000).


answer_404_on_no_file_with_auth(_Config) ->
  {ok, {{403,_},_,_}} = flu_test:request("http://127.0.0.1:5670/securevod/bunny10.mp4/manifest.f4m",get,[],1000),

  {ok, {{404,_},_,_}} = flu_test:request("http://127.0.0.1:5670/securevod/bunny10.mp4/manifest.f4m?token=a",get,[],1000),
  ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%          HTTP FILE TESTS %%%%%%%%%%%%%%%%%



http_file_test_() ->
  Spec = {foreach,
    flu_test:setup_(fun() ->
      flu_test:set_config([{file, "http_vod", "http://localhost:5672/"}]),
      inets:start(),
      inets:start(httpd,[
        {server_root,"../test"},
        {port,5672},
        {server_name,"test_server"},
        {document_root,"../../../priv"},
        {modules,[mod_alias,mod_range, mod_auth, mod_actions, mod_dir, mod_get, mod_head]}
      ])
    end),
    flu_test:teardown_(fun() ->
      application:stop(inets)
    end),
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




test_local_http_file_playlist() ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hls_playlist(<<"http://localhost:5672/bunny.mp4">>, <<"bunny.mp4">>)),
  ok.

test_local_http_file_segment() ->
  ?assertMatch({ok, _,  Bin} when is_binary(Bin) orelse is_list(Bin), 
    flu_file:hls_segment(<<"http://localhost:5672/bunny.mp4">>, <<"bunny.mp4">>, 2)),
  {ok, _, Bin_} = flu_file:hls_segment(<<"http://localhost:5672/bunny.mp4">>, <<"bunny.mp4">>, 2),
  Bin = iolist_to_binary(Bin_),
  {ok, Frames} = mpegts_decoder:decode_file(Bin),
  ?assert(length(Frames) > 10),
  ok.



test_access_http_file() ->
  Result = http_stream:request_body("http://localhost:5670/http_vod/bunny.mp4/index.m3u8",[{keepalive,false}]),
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


