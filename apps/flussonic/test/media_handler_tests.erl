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

    ,{"test_hls_file_segment", fun test_hls_file_segment/0}
    ,{"test_hls_file_mbr_segment", fun test_hls_file_mbr_segment/0}
    ,{"test_hls_file_mbr_root_playlist", fun test_hls_file_mbr_root_playlist/0}
    ,{"test_hls_file_playlist", fun test_hls_file_playlist/0}
    ,{"test_hls_file_mbr_playlist", fun test_hls_file_mbr_playlist/0}

    ,{"test_hds_file_manifest", fun test_hds_file_manifest/0}
    ,{"test_hds_file_segment", fun test_hds_file_segment/0}
    ,{"test_hds_file_mbr_segment", fun test_hds_file_mbr_segment/0}

    ,{"test_archive_manifest", fun test_archive_manifest/0}
    ,{"test_archive_dvr_manifest", fun test_archive_dvr_manifest/0}
    ,{"test_archive_dvr_bootstrap", fun test_archive_dvr_bootstrap/0} 
    ,{"test_archive_fragment", fun test_archive_fragment/0}
    ,{"test_archive_event_fragment", fun test_archive_event_fragment/0}


    ,{"test_archive_mpeg_stream", fun test_archive_mpeg_stream/0}
    ,{"test_archive_mpeg_file", fun test_archive_mpeg_file/0}
    ,{"test_archive_timeshift_abs", fun test_archive_timeshift_abs/0}
    ,{"test_archive_timeshift_rel", fun test_archive_timeshift_rel/0}
  ]}.


test_lookup_by_path(Path) -> catch test_lookup_by_path0(Path).

test_lookup_by_path0(Path) when is_list(Path) -> test_lookup_by_path0(list_to_binary(Path));
test_lookup_by_path0(<<"/", Path/binary>>) -> test_lookup_by_path0(Path);
test_lookup_by_path0(Path) when is_binary(Path) ->
  Routes = flu_config:parse_routes(flu_config:get_config()),
  {Options, PathInfo} = try select_route(Routes, binary:split(Path, <<"/">>, [global]))
  catch throw:{no_route_found,PI} -> throw({no_route_found,flu_config:get_config(),Routes,PI})
  end,
  media_handler:lookup_name(PathInfo, Options, req, []).

select_route([{Prefix, _, Options}|Routes], PathInfo) ->
  case try_prefix(Prefix, PathInfo) of
    false ->
      select_route(Routes, PathInfo);
    PI1 ->
      {Options, PI1}
  end;

select_route([], PathInfo) -> throw({no_route_found, PathInfo}).


try_prefix([P|P1], [P|P2]) -> try_prefix(P1, P2);
try_prefix(['...'], P2) -> P2;
try_prefix([], []) -> [];
try_prefix(_, _) -> false.

set_config(Config) ->
  {ok, Config2} = flu_config:parse_config(Config, undefined),
  meck:expect(flu_config, get_config, fun() -> Config2 end).

try_prefix_test_() ->
  [?_assertEqual(false, try_prefix([<<"l1">>], [<<"l2">>])),
  ?_assertEqual(false, try_prefix([<<"l1">>], [<<"l1">>,<<"l2">>])),
  ?_assertEqual([<<"l2">>], try_prefix([<<"l1">>, '...'], [<<"l1">>,<<"l2">>])),
  ?_assertEqual([], try_prefix([<<"l1">>, <<"l2">>], [<<"l1">>,<<"l2">>]))
  ].

select_route_test_() ->
  [?_assertEqual({options1, []}, select_route([{[<<"l1">>],h, options1}], [<<"l1">>])),
  ?_assertEqual({options1, [<<"l2">>]}, select_route([{[<<"l1">>, '...'],h, options1}, {[<<"l1">>,<<"l2">>],h, options2}], [<<"l1">>, <<"l2">>])),
  ?_assertEqual({options2, []}, select_route([{[<<"l1">>,<<"l2">>],h, options2}, {[<<"l1">>, '...'],h, options1}], [<<"l1">>, <<"l2">>]))
  ].

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
  ?assertMatch({{hls_dvr_packetizer, playlist, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>}, 
    test_lookup_by_path("/live/livestream/index-1234567-3600.m3u8")).


test_offline_live_dvr_ts() ->
  set_config([{live, "live", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_handler, mpeg_file, [<<"test/files">>,1348748644,3600, _]}, [], <<"livestream">>}, 
    test_lookup_by_path("/live/livestream/archive-1348748644-3600.ts")).





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





test_archive_manifest() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_manifest, [<<"test/files">>,1234567,3600]}, _, <<"livestream">>},
    test_lookup_by_path("/livestream/archive/1234567/3600/manifest.f4m")).  

test_archive_dvr_manifest() ->
  set_config([{stream, "livestream", "fake://url", [{dvr, <<"test/files">>}]}]),
  ?assertMatch({{dvr_session, hds_manifest, [<<"test/files">>,1234567,now]}, _, <<"livestream">>},
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





