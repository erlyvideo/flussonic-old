-module(media_handler_tests).

-include_lib("eunit/include/eunit.hrl").



media_handler_test_() ->
  {foreach, fun() ->
    meck:new([flu_config,media_handler], [{passthrough,true}]),
    [flu_config,media_handler]
  end, 
  fun meck:unload/1,
  [
    {"test_offline_stream_playlist", fun test_offline_stream_playlist/0}
    ,{"test_offline_rewrite_playlist", fun test_offline_rewrite_playlist/0}
    ,{"test_live_dvr_playlist", fun test_live_dvr_playlist/0}
    ,{"test_offline_live_dvr_ts", fun test_offline_live_dvr_ts/0}

    ,{"test_live_hds_manifest", fun test_live_hds_manifest/0}
    ,{"test_live_hds_manifest_with_auth", fun test_live_hds_manifest_with_auth/0}

    ,{"test_hls_file_segment", fun test_hls_file_segment/0}
    ,{"test_hls_file_mbr_segment", fun test_hls_file_mbr_segment/0}
    ,{"test_hls_file_mbr_root_playlist", fun test_hls_file_mbr_root_playlist/0}
    ,{"test_hls_file_playlist", fun test_hls_file_playlist/0}
    ,{"test_hls_file_mbr_playlist", fun test_hls_file_mbr_playlist/0}

    ,{"test_hds_file_manifest", fun test_hds_file_manifest/0}
    ,{"test_hds_file_segment", fun test_hds_file_segment/0}
    ,{"test_hds_file_mbr_segment", fun test_hds_file_mbr_segment/0}

    ,{"test_archive_dvr_manifest", fun test_archive_dvr_manifest/0}
    ,{"test_archive_dvr_event_manifest", fun test_archive_dvr_event_manifest/0}
    ,{"test_archive_dvr_event_manifest", fun test_archive_dvr_event_manifest_with_auth/0}
    ,{"test_archive_dvr_bootstrap", fun test_archive_dvr_bootstrap/0} 
    ,{"test_archive_fragment", fun test_archive_fragment/0}
    ,{"test_archive_event_fragment", fun test_archive_event_fragment/0}


    ,{"test_archive_mpeg_stream", fun test_archive_mpeg_stream/0}
    ,{"test_archive_mpeg_file", fun test_archive_mpeg_file/0}
    ,{"test_archive_timeshift_abs", fun test_archive_timeshift_abs/0}
    ,{"test_archive_timeshift_rel", fun test_archive_timeshift_rel/0}


    ,{"test_wrong_path1", fun test_wrong_path1/0}
  ]}.


test_lookup_by_path(Path) -> catch test_lookup_by_path0(Path).

test_lookup_by_path0(Path) when is_list(Path) -> test_lookup_by_path0(list_to_binary(Path));
test_lookup_by_path0(<<"/",_/binary>> = Path1) ->
  Routes = flu_config:parse_routes(flu_config:get_config()),
  Dispatch = cowboy_router:compile([{'_', Routes}]),
  {Path,Query} = case binary:split(Path1, <<"?">>) of
    [P,Q] -> {P,Q};
    [P] -> {P,<<"">>}
  end,
  % {Options, PathInfo} = try select_route(Routes, binary:split(Path, <<"/">>, [global]))
  % catch throw:{no_route_found,PI} -> throw({no_route_found,flu_config:get_config(),Routes,PI})
  % end,
  Req1 = cowboy_req:new(socket, fake_inet, <<"GET">>, Path, Query, <<>>, {1,1},
    [{<<"x-real-ip">>,<<"127.0.0.1">>}], <<"erlyvideo">>, 9090, <<>>, false, undefined, undefined),
  {ok, Req2, Env} = cowboy_router:execute(Req1, [{dispatch,Dispatch}]),

  {handler,media_handler} = lists:keyfind(handler,1,Env),
  {_,Options} = lists:keyfind(handler_opts,1,Env),
  PathInfo = cowboy_req:get(path_info,Req2),

  media_handler:lookup_name(PathInfo, Options, Req2, []);

test_lookup_by_path0(Path) ->
  test_lookup_by_path0(<<"/", Path/binary>>).


set_config(Config) ->
  {ok, Config2} = flu_config:parse_config(Config, undefined),
  meck:expect(flu_config, get_config, fun() -> Config2 end).


test_offline_stream_playlist() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/index-1234567-3600.m3u8")).


test_offline_rewrite_playlist() ->
  set_config([{rewrite, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>}, 
    test_lookup_by_path("/livestream/index-1234567-3600.m3u8")).


test_live_dvr_playlist() ->
  set_config([{live, "live", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"live/livestream">>}, 
    test_lookup_by_path("/live/livestream/index-1234567-3600.m3u8")).


test_offline_live_dvr_ts() ->
  set_config([{live, "live", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, mpeg_file, [<<"test/files">>,1348748644,3600, _]}, [], <<"live/livestream">>}, 
    test_lookup_by_path("/live/livestream/archive-1348748644-3600.ts")).




test_live_hds_manifest() ->
  set_config([{stream, "chan0", "udp://1"}]),
  ?assertMatch({{flu_stream, hds_manifest, []}, _, <<"chan0">>},
    test_lookup_by_path("/chan0/manifest.f4m")).


test_live_hds_manifest_with_auth() ->
  meck:expect(media_handler, check_sessions, fun(_,_,_) -> {ok, <<"a">>} end),
  set_config([{stream, "chan0", "udp://1", [{sessions, "http://127.0.0.1:8080/"}]}]),
  ?assertMatch({{flu_stream, hds_manifest, [<<"a">>]}, _, <<"chan0">>},
    test_lookup_by_path("/chan0/manifest.f4m")).




test_hls_file_playlist() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hls_playlist, []}, _, <<"movie.mp4">>},
    test_lookup_by_path("/vod/movie.mp4/index.m3u8")).

test_hls_file_mbr_root_playlist() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hls_mbr_playlist, []}, _, <<"movie.mp4">>},
    test_lookup_by_path("/vod/movie.mp4/mbr.m3u8")).

test_hls_file_mbr_playlist() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hls_playlist, [[1,2]]}, _, <<"movie.mp4">>},
    test_lookup_by_path("/vod/movie.mp4/tracks-1,2/index.m3u8")).

test_hls_file_segment() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hls_segment, [<<"test/files">>,5]}, _, <<"movie.mp4">>}, 
    test_lookup_by_path("/vod/movie.mp4/hls/segment5.ts")).

test_hls_file_mbr_segment() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hls_segment, [<<"test/files">>,5, [1,2]]}, _, <<"movie.mp4">>}, 
    test_lookup_by_path("/vod/movie.mp4/tracks-1,2/hls/segment5.ts")).



test_hds_file_manifest() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hds_manifest, []}, _, <<"movie.mp4">>},
    test_lookup_by_path("/vod/movie.mp4/manifest.f4m")).

test_hds_file_segment() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hds_segment, [5]}, _, <<"movie.mp4">>}, 
    test_lookup_by_path("/vod/movie.mp4/hds/0/Seg0-Frag5")).

test_hds_file_mbr_segment() ->
  set_config([{file, "vod", "test/files"}]),
  ?assertMatch({{flu_file, hds_segment, [5, [1,2]]}, _, <<"movie.mp4">>}, 
    test_lookup_by_path("/vod/movie.mp4/hds/tracks-1,2/Seg0-Frag5")).





test_archive_dvr_manifest() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_manifest, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/3600/manifest.f4m")).  

test_archive_dvr_event_manifest() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_manifest, [<<"test/files">>,1234567,now]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/now/manifest.f4m")).  

test_archive_dvr_event_manifest_with_auth() ->
  meck:expect(media_handler, check_sessions, fun(_,_,_) -> {ok, <<"a">>} end),
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>},{sessions,"http://127.0.0.1:8080"}]}]),
  ?assertMatch({{dvr_session, hds_manifest, [<<"test/files">>,1234567,now, <<"a">>]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/now/manifest.f4m")).  


test_archive_dvr_bootstrap() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_bootstrap, [<<"test/files">>,1234567,now]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/now/bootstrap")).  

test_archive_fragment() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_fragment, [<<"test/files">>,1234567,3600, 5]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/3600/0/Seg2-Frag5")).

test_archive_event_fragment() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_fragment, [<<"test/files">>,1234567,now, 5]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/now/0/Seg2-Frag5")).



test_archive_mpeg_stream() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, mpeg_stream, [<<"test/files">>,1234567,3600, _]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/3600/mpegts")).  

test_archive_mpeg_file() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, mpeg_file, [<<"test/files">>,1234567,3600, _]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive-1234567-3600.ts")).  

test_archive_timeshift_abs() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, timeshift_abs, [<<"test/files">>,1234567, _]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/timeshift_abs/1234567")).  

test_archive_timeshift_rel() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, timeshift_rel, [<<"test/files">>,1234567, _]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/timeshift_rel/1234567")).  



test_wrong_path1() ->
  set_config([{stream, "chan0", "fake://url", []}]),
  ?assertMatch({return, 415, _}, test_lookup_by_path("/chan0/archive/1359421400/manifest.f4m")).



