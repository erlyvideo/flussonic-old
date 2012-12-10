-module(m3u8_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../include/m3u8.hrl").


parse(Name) ->
  {ok, Bin} = file:read_file("../test/files/playlist-"++Name++".m3u8"),
  m3u8:parse(Bin).



parse_playlist_file_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 0,
    type = vod,
    entries = [
      #m3u8_entry{duration = 4000, number = 0, url = <<"hls/segment1.ts">>},
      #m3u8_entry{duration = 4000, number = 1, url = <<"hls/segment2.ts">>}
    ]
  }, parse("file")).

parse_playlist_live_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 2,
    type = live,
    entries = [
      #m3u8_entry{duration = 7290, number = 2, url = <<"2012/12/07/09/37/05-07292.ts">>},
      #m3u8_entry{duration = 6000, number = 3, url = <<"2012/12/07/09/37/13-06000.ts">>}
    ]
  }, parse("live")).

parse_playlist_event_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 0,
    type = event,
    entries = [
      #m3u8_entry{duration = 5880, number = 0, url = <<"2012/12/07/09/42/17-05875.ts">>},
      #m3u8_entry{duration = 5880, number = 1, url = <<"2012/12/07/09/42/23-05875.ts">>}
    ]
  }, parse("event")).

parse_playlist_server1_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 0,
    type = live,
    entries = [
      #m3u8_entry{number = 0, url = <<"priv/bunny.mp4">>},
      #m3u8_entry{number = 1, url = <<"priv/mbr.mp4">>}
    ]
  }, parse("server1")).

parse_playlist_server2_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 0,
    type = live,
    entries = [
      #m3u8_entry{number = 0, url = <<"ort">>, duration = 20000},
      #m3u8_entry{number = 1, url = <<"priv/bunny.mp4">>}
    ]
  }, parse("server2")).

parse_playlist_mbr_test() ->
  ?assertMatch(#m3u8_mbr_playlist{
    playlists = [
      #m3u8_playlist{url = <<"tracks-1,4/index.m3u8">>, bitrate = 1196000},
      #m3u8_playlist{url = <<"tracks-2,4/index.m3u8">>, bitrate = 510000},
      #m3u8_playlist{url = <<"tracks-3,4/index.m3u8">>, bitrate = 155000}
    ]
  }, parse("mbr")).


fetch_playlist_file_test() ->
  ?assertMatch(#m3u8_playlist{
    sequence = 0,
    type = vod,
    entries = [
      #m3u8_entry{duration = 4000, number = 0, url = <<"../test/files/hls/segment1.ts">>},
      #m3u8_entry{duration = 4000, number = 1, url = <<"../test/files/hls/segment2.ts">>}
    ]
  }, m3u8:fetch("../test/files/playlist-file.m3u8")).


fetch_playlist_mbr_test() ->
  ?assertMatch(#m3u8_mbr_playlist{
    playlists = [
      #m3u8_playlist{url = <<"../test/files/tracks-1,4/index.m3u8">>, bitrate = 1196000},
      #m3u8_playlist{url = <<"../test/files/tracks-2,4/index.m3u8">>, bitrate = 510000},
      #m3u8_playlist{url = <<"../test/files/tracks-3,4/index.m3u8">>, bitrate = 155000}
    ]
  }, m3u8:fetch("../test/files/playlist-mbr.m3u8")).


fetch_invalid_playlist_test() ->
  ?assertMatch({error, _}, m3u8:fetch("non-existing-playlist")).




prepend_base_path_test() ->
  ?assertMatch(#m3u8_playlist{url = <<"playlist.txt">>, entries = [
    #m3u8_entry{url = <<"ort">>},
    #m3u8_entry{url = <<"priv/bunny.mp4">>}
  ]}, m3u8:prepend_base_path(#m3u8_playlist{entries = [
    #m3u8_entry{url = <<"ort">>},
    #m3u8_entry{url = <<"priv/bunny.mp4">>}]
  }, <<"playlist.txt">>)).

no_prepend_base_path_test() ->
  ?assertMatch(#m3u8_playlist{
    entries = [
      #m3u8_entry{duration = 4000, number = 0, url = <<"hls/segment1.ts">>},
      #m3u8_entry{duration = 4000, number = 1, url = <<"hls/segment2.ts">>}
    ]
  }, m3u8:fetch("../test/files/playlist-file.m3u8",[{relative,false}])).













