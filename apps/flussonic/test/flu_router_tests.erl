-module(flu_router_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


mr(Path) -> flu_router:route_media_request(Path).

media_request_test_() ->
  [
   {"hls_track_playlist", ?_assertEqual({hls_track_playlist, [<<"vod/hobbyt.mp4">>,<<"1,2">>]}, 
      mr(<<"/vod/hobbyt.mp4/tracks-1,2/index.m3u8">>))}
  ,{"hls_track_segment", ?_assertEqual({hls_track_segment, [<<"vod/hobbyt.mp4">>,<<"1,2">>,<<"15">>]}, 
    mr(<<"/vod/hobbyt.mp4/tracks-1,2/hls/segment15.ts">>))}

  ,{"hls_segment", ?_assertEqual({hls_segment, [<<"ort/recorded">>,<<"2012/09/15/23/15/48-04050">>]}, 
    mr(<<"/ort/recorded/2012/09/15/23/15/48-04050.ts">>))}

  ,{"archive_mp4", ?_assertEqual({archive_mp4, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]},
    mr(<<"/ort/recorded/archive-1362504585-3600.mp4">>))}

  ,{"archive_ts", ?_assertEqual({archive_ts, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]},
    mr(<<"/ort/recorded/archive-1362504585-3600.ts">>))}

  ,{"archive_mpegts", ?_assertEqual({archive_mpegts, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]},
    mr(<<"/ort/recorded/archive/1362504585/3600/mpegts">>))}

  ,{"archive_hds", ?_assertEqual({archive_hds, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/3600/manifest.f4m">>))}
  ,{"archive_hds", ?_assertEqual({archive_hds, [<<"ort/recorded">>,<<"1362504585">>,<<"now">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/now/manifest.f4m">>))}


  ,{"archive_bootstrap", ?_assertEqual({archive_bootstrap, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/3600/bootstrap">>))}
  ,{"archive_bootstrap", ?_assertEqual({archive_bootstrap, [<<"ort/recorded">>,<<"1362504585">>,<<"now">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/now/bootstrap">>))}

  ,{"archive_fragment", ?_assertEqual({archive_fragment, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>,<<"42">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/3600/0/Seg1-Frag42">>))}
  ,{"archive_fragment", ?_assertEqual({archive_fragment, [<<"ort/recorded">>,<<"1362504585">>,<<"now">>,<<"42">>]}, 
    mr(<<"/ort/recorded/archive/1362504585/now/0/Seg1-Frag42">>))}


  ,{"archive_hls", ?_assertEqual({archive_hls_long, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]},
    mr(<<"/ort/recorded/archive/1362504585/3600/index.m3u8">>))}
  ,{"archive_hls", ?_assertEqual({archive_hls_long, [<<"ort/recorded">>,<<"1362504585">>,<<"now">>]},
    mr(<<"/ort/recorded/archive/1362504585/now/index.m3u8">>))}

  ,{"archive_hls", ?_assertEqual({archive_hls, [<<"ort/recorded">>,<<"1362504585">>,<<"3600">>]}, 
    mr(<<"/ort/recorded/index-1362504585-3600.m3u8">>))}
  ,{"archive_hls", ?_assertEqual({archive_hls, [<<"ort/recorded">>,<<"1362504585">>,<<"now">>]}, 
    mr(<<"/ort/recorded/index-1362504585-now.m3u8">>))}

  ,{"timeshift_abs", ?_assertEqual({timeshift_abs, [<<"ort/recorded">>,<<"1362504585">>]},
    mr(<<"/ort/recorded/timeshift_abs/1362504585">>))}

  ,{"timeshift_rel", ?_assertEqual({timeshift_rel, [<<"ort/recorded">>,<<"3600">>]},
    mr(<<"/ort/recorded/timeshift_rel/3600">>))}

  ,{"hds_manifest", ?_assertEqual({hds_manifest,[<<"ort">>]}, mr(<<"/ort/manifest.f4m">>))}
  ,{"hds_manifest", ?_assertEqual({hds_manifest,[<<"ort/special">>]}, mr(<<"/ort/special/manifest.f4m">>))}

  ,{"hds_bootstrap", ?_assertEqual({hds_bootstrap,[<<"ort/special">>]}, mr(<<"/ort/special/bootstrap">>))}

  ,{"hds_track_fragment", ?_assertEqual({hds_track_fragment, [<<"vod/hobbyt.mp4">>,<<"1,2">>,<<"5">>]}, 
    mr(<<"/vod/hobbyt.mp4/hds/tracks-1,2/Seg0-Frag5">>))}

  ,{"hls_playlist", ?_assertEqual({hls_playlist, [<<"vod/hobbyt.mp4">>]}, mr(<<"/vod/hobbyt.mp4/index.m3u8">>))}

  ,{"hls_segment", ?_assertEqual({hls_file_segment, [<<"vod/hobbyt.mp4">>,<<"42">>]}, 
    mr(<<"/vod/hobbyt.mp4/hls/segment42.ts">>))}


  ,{"hls_mbr_playlist", ?_assertEqual({hls_mbr_playlist, [<<"vod/hobbyt.mp4">>]}, mr(<<"/vod/hobbyt.mp4/mbr.m3u8">>))}

  ,{"mpegts", ?_assertEqual({mpegts, [<<"ort">>]}, mr(<<"/ort/mpegts">>))}


  ].



routes() ->
  Config = [
     {stream, "ort", "url1"}
    ,{stream, "ort/good", "url11"}
    ,{stream, "ort-rec", "url2", [{dvr,"dvr2:/storage"}]}
    ,{live, "clients/15"}
    ,{live, "rec/32", [{dvr,"dvr2:/clients"}]}
    ,{file, "vod", "/movies"}
    ,{file, "hd", "/premium", [{sessions,"http://backend"}]}
  ],
  {ok, ParsedConfig} = flu_config:parse_config(Config, undefined),
  Router = flu_router:compile(ParsedConfig),
  Router.

r(Path) -> 
  case flu_router:route(Path, routes()) of
    {ok, {M,F,A,Meta}} -> {M,F,A,proplists:get_value(tag,Meta)};
    undefined -> undefined
  end.


route_test_() ->
  [
    {"hls_track_playlist", ?_assertMatch( {flu_file, hls_playlist, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>, [1,2]], hls},
      r(<<"/vod/hobbyt.mp4/tracks-1,2/index.m3u8">>))}

    ,{"hls_track_segment", ?_assertMatch( {flu_file, hls_segment, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>, [1,2], 5], hls_d},
      r(<<"/vod/hobbyt.mp4/tracks-1,2/hls/segment5.ts">>))}

    ,{"hls_stream_segment", ?_assertMatch( {flu_stream, hls_segment, [undefined, <<"ort">>, <<"2012/09/15/23/15/48-04050">>], hls_d},
      r(<<"/ort/2012/09/15/23/15/48-04050.ts">>))}
    ,{"hls_stream_segment", ?_assertMatch( {flu_stream, hls_segment, [<<"dvr2:/storage">>, <<"ort-rec">>, <<"2012/09/15/23/15/48-04050">>], hls_d},
      r(<<"/ort-rec/2012/09/15/23/15/48-04050.ts">>))}
    ,{"hls_stream_segment", ?_assertMatch( {flu_stream, hls_segment, [<<"dvr2:/clients">>, <<"rec/32/ustream">>, <<"2012/09/15/23/15/48-04050">>], hls_d},
      r(<<"/rec/32/ustream/2012/09/15/23/15/48-04050.ts">>))}

    ,{"archive-mp4", ?_assertMatch( {dvr_handler, mp4, [req, <<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], mp4},
      r(<<"/ort-rec/archive-1362504585-3600.mp4">>))}
    ,{"archive-mp4", ?_assertMatch( {dvr_handler, mp4, [req, <<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], mp4},
      r(<<"/rec/32/ustream/archive-1362504585-3600.mp4">>))}

    ,{"archive-ts", ?_assertMatch( {dvr_handler, ts_file, [req, <<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], hls_d},
      r(<<"/ort-rec/archive-1362504585-3600.ts">>))}
    ,{"archive-ts", ?_assertMatch( {dvr_handler, ts_file, [req, <<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], hls_d},
      r(<<"/rec/32/ustream/archive-1362504585-3600.ts">>))}

    ,{"archive-mpegts", ?_assertMatch( {dvr_handler, ts_stream, [req, <<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], hls_d},
      r(<<"/ort-rec/archive/1362504585/3600/mpegts">>))}
    ,{"archive-mpegts", ?_assertMatch( {dvr_handler, ts_stream, [req, <<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], hls_d},
      r(<<"/rec/32/ustream/archive/1362504585/3600/mpegts">>))}


    ,{"archive-hds", ?_assertMatch( {dvr_session, hds_manifest, [<<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], hds},
      r(<<"/ort-rec/archive/1362504585/3600/manifest.f4m">>))}
    ,{"archive-hds", ?_assertMatch( {dvr_session, hds_manifest, [<<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], hds},
      r(<<"/rec/32/ustream/archive/1362504585/3600/manifest.f4m">>))}

    ,{"archive-bootstrap", ?_assertMatch( {dvr_session, bootstrap, [<<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], hds_b},
      r(<<"/ort-rec/archive/1362504585/3600/bootstrap">>))}
    ,{"archive-bootstrap", ?_assertMatch( {dvr_session, bootstrap, [<<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], hds_b},
      r(<<"/rec/32/ustream/archive/1362504585/3600/bootstrap">>))}

    ,{"archive-fragment", ?_assertMatch( {dvr_session, hds_fragment, [<<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600, 42], hds_d},
      r(<<"/ort-rec/archive/1362504585/3600/0/Seg1-Frag42">>))}
    ,{"archive-fragment", ?_assertMatch( {dvr_session, hds_fragment, [<<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600, 42], hds_d},
      r(<<"/rec/32/ustream/archive/1362504585/3600/0/Seg1-Frag42">>))}

    ,{"archive-hls", ?_assertMatch( {flu_www, redirect, [<<"/ort-rec/index-1362504585-3600.m3u8">>], none},
      r(<<"/ort-rec/archive/1362504585/3600/index.m3u8">>))}
    ,{"archive-hls", ?_assertMatch( {flu_www, redirect, [<<"/rec/32/ustream/index-1362504585-3600.m3u8">>], none},
      r(<<"/rec/32/ustream/archive/1362504585/3600/index.m3u8">>))}
    ,{"archive-hls", ?_assertMatch( {dvr_session, hls_playlist, [<<"dvr2:/storage">>, <<"ort-rec">>, 1362504585,3600], hls},
      r(<<"/ort-rec/index-1362504585-3600.m3u8">>))}
    ,{"archive-hls", ?_assertMatch( {dvr_session, hls_playlist, [<<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585,3600], hls},
      r(<<"/rec/32/ustream/index-1362504585-3600.m3u8">>))}


    ,{"timeshift_abs", ?_assertMatch( {dvr_handler, timeshift_abs, [req, <<"dvr2:/storage">>, <<"ort-rec">>, 1362504585], hls_d},
      r(<<"/ort-rec/timeshift_abs/1362504585">>))}
    ,{"timeshift_abs", ?_assertMatch( {dvr_handler, timeshift_abs, [req, <<"dvr2:/clients">>, <<"rec/32/ustream">>, 1362504585], hls_d},
      r(<<"/rec/32/ustream/timeshift_abs/1362504585">>))}

    ,{"timeshift_rel", ?_assertMatch( {dvr_handler, timeshift_rel, [req, <<"dvr2:/storage">>, <<"ort-rec">>, 3600], hls_d},
      r(<<"/ort-rec/timeshift_rel/3600">>))}
    ,{"timeshift_rel", ?_assertMatch( {dvr_handler, timeshift_rel, [req, <<"dvr2:/clients">>, <<"rec/32/ustream">>, 3600], hls_d},
      r(<<"/rec/32/ustream/timeshift_rel/3600">>))}


    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_manifest, [<<"ort">>], hds}, r(<<"/ort/manifest.f4m">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_manifest, [<<"ort/good">>], hds}, r(<<"/ort/good/manifest.f4m">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_manifest, [<<"rec/32/ustream">>], hds}, r(<<"/rec/32/ustream/manifest.f4m">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_file, hds_manifest, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>], hds}, 
      r(<<"/vod/hobbyt.mp4/manifest.f4m">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_file, hds_manifest, [<<"/premium/arrow/s01e05.mp4">>, <<"hd/arrow/s01e05.mp4">>], hds},
      r(<<"/hd/arrow/s01e05.mp4/manifest.f4m">>) )}


    ,{"hds_manifest", ?_assertMatch( {flu_stream, bootstrap, [<<"ort">>], hds_b}, r(<<"/ort/bootstrap">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, bootstrap, [<<"ort/good">>], hds_b}, r(<<"/ort/good/bootstrap">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, bootstrap, [<<"rec/32/ustream">>], hds_b}, r(<<"/rec/32/ustream/bootstrap">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_file, bootstrap, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>], hds_b}, r(<<"/vod/hobbyt.mp4/bootstrap">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_file, bootstrap, [<<"/premium/arrow/s01e05.mp4">>, <<"hd/arrow/s01e05.mp4">>], hds_b},
      r(<<"/hd/arrow/s01e05.mp4/bootstrap">>) )}


    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_fragment, [<<"ort">>, 42], hds_d}, r(<<"/ort/hds/0/Seg1-Frag42">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_fragment, [<<"ort/good">>, 42], hds_d}, r(<<"/ort/good/hds/0/Seg1-Frag42">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_stream, hds_fragment, [<<"rec/32/ustream">>, 42], hds_d}, r(<<"/rec/32/ustream/hds/0/Seg1-Frag42">>) )}
    ,{"hds_manifest", ?_assertMatch( {flu_file,   hds_fragment, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>, 42], hds_d}, 
      r(<<"/vod/hobbyt.mp4/hds/0/Seg1-Frag42">>) )}


    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"ort">>], hls}, r(<<"/ort/index.m3u8">>))}
    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"ort/good">>], hls}, r(<<"/ort/good/index.m3u8">>))}
    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"ort-rec">>], hls}, 
      r(<<"/ort-rec/index.m3u8">>))}

    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"ort-rec">>], hls}, 
      r(<<"/ort-rec/playlist.m3u8">>))}


    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"clients/15/ustream1">>], hls}, 
      r(<<"/clients/15/ustream1/index.m3u8">>))}
    ,{"hls_playlist", ?_assertMatch( {flu_stream, hls_playlist, [<<"clients/15/ust/ream1">>], hls}, 
      r(<<"/clients/15/ust/ream1/index.m3u8">>))}
    ,{"hls_playlist", ?_assertMatch( undefined, r(<<"/clients/15/index.m3u8">>))}

    ,{"hls_playlist", ?_assertMatch( {flu_file, hls_playlist, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>], hls},
      r(<<"/vod/hobbyt.mp4/index.m3u8">>))}
    ,{"hls_playlist", ?_assertMatch( {flu_file, hls_playlist, [<<"/premium/arrow/s01e05.mp4">>, <<"hd/arrow/s01e05.mp4">>], hls}, 
      r(<<"/hd/arrow/s01e05.mp4/index.m3u8">>))}

    ,{"hls_file_segment", ?_assertMatch( {flu_file, hls_segment, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>, 42], hls_d},
      r(<<"/vod/hobbyt.mp4/hls/segment42.ts">>))}


    ,{"hls_mbr_playlist", ?_assertMatch( {flu_file, hls_mbr_playlist, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>], hls}, 
      r(<<"/vod/hobbyt.mp4/mbr.m3u8">>))}

    ,{"hds_track_fragment", ?_assertMatch( {flu_file, hds_fragment, [<<"/movies/hobbyt.mp4">>, <<"vod/hobbyt.mp4">>, [1,2], 5], hds_d}, 
      r(<<"/vod/hobbyt.mp4/hds/tracks-1,2/Seg0-Frag5">>))}
    

    ,{"mpegts", ?_assertMatch( {mpegts_handler, request, [req, <<"ort">>, _], hls_d}, 
      r(<<"/ort/mpegts">>))}
    ,{"mpegts", ?_assertMatch( {mpegts_handler, request, [req, <<"rec/32/ustream">>, _], hls_d}, 
      r(<<"/rec/32/ustream/mpegts">>))}




  % Now goes API

  ,{"root", ?_assertMatch({api_handler, mainpage, [req,_],_}, r(<<"/">>))}
  ,{"root", ?_assertMatch({api_handler, mainpage, [req,_],_}, r(<<"/admin">>))}

  ,{"api_sendlogs", ?_assertMatch({api_handler, sendlogs, [req,_],_}, r(<<"/erlyvideo/api/sendlogs">>))}
  ,{"api_reload", ?_assertMatch({api_handler, reload, [req,_],_}, r(<<"/erlyvideo/api/reload">>))}
  ,{"api_events", ?_assertMatch(undefined, r(<<"/erlyvideo/api/events">>))}
  ,{"api_streams", ?_assertMatch({api_handler, streams, [req,_],_}, r(<<"/erlyvideo/api/streams">>))}
  ,{"api_server", ?_assertMatch({api_handler, server, [req,_],_}, r(<<"/erlyvideo/api/server">>))}
  ,{"api_sessions", ?_assertMatch({api_handler, sessions, [req,_],_}, r(<<"/erlyvideo/api/sessions">>))}
  ,{"api_pulse", ?_assertMatch({api_handler, pulse, [req,_],_}, r(<<"/erlyvideo/api/pulse">>))}
  ,{"api_stream_health", ?_assertMatch({api_handler, health, [req,<<"ort/good">>,_],_}, r(<<"/erlyvideo/api/stream_health/ort/good">>))}
  ,{"api_stream_restart", ?_assertMatch({api_handler, stream_restart, [req,<<"ort/good">>,_],_}, r(<<"/erlyvideo/api/stream_restart/ort/good">>))}
  ,{"api_media_info", ?_assertMatch({api_handler, media_info, [req,<<"ort/good">>,_],_}, r(<<"/erlyvideo/api/media_info/ort/good">>))}
  ,{"api_dvr_status", ?_assertMatch({api_handler, dvr_status, [req,2012,9,15,<<"ort/good">>,_],_}, 
    r(<<"/erlyvideo/api/dvr_status/2012/9/15/ort/good">>))}


  ].









