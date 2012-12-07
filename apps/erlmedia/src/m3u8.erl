-module(m3u8).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include("../include/m3u8.hrl").



-export([parse/1]).
-export([fetch/1]).


fetch(URL) when is_list(URL) ->
  fetch(list_to_binary(URL));

fetch(URL) when is_binary(URL) ->
  Playlist1 = case http_uri:parse(binary_to_list(URL)) of
    {error, no_scheme} ->
      case file:read_file(URL) of
        {ok, Bin} ->
          Playlist = parse(Bin),
          prepend_base_path(Playlist, URL);
        {error, _} = Error ->
          Error
      end;
    {ok, _} ->
      case http_stream:request_body(URL, [{keepalive,false},{no_fail,true}]) of
        {ok, {_,200,_,Bin}} ->
          Playlist = parse(Bin),
          prepend_base_url(Playlist, URL);
        {ok, Reply} ->
          {error, {fetch_manifest,URL, Reply}};
        {error, Error} ->
          {error, {fetch_manifest,URL, Error}}
      end
  end,
  Playlist1.






parse(Bin) when is_binary(Bin) ->
  Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
  detect_playlist_type(Lines).


detect_playlist_type([<<"#EXTM3U">>|Lines]) ->
  detect_playlist_type(Lines);

detect_playlist_type([<<"#EXT-X-VERSION:",_/binary>>|Lines]) ->
  detect_playlist_type(Lines);

detect_playlist_type([<<"#EXT-X-MEDIA:",_/binary>>|_] = Lines) ->
  read_mbr_playlist(Lines, #m3u8_mbr_playlist{});

detect_playlist_type([<<"#EXT-X-STREAM-INF:",_/binary>>|_] = Lines) ->
  read_mbr_playlist(Lines, #m3u8_mbr_playlist{});

detect_playlist_type(Lines) ->
  Playlist = read_playlist(Lines, #m3u8_playlist{}, undefined),
  Playlist.


read_mbr_playlist([<<"#EXT-X-MEDIA:",_/binary>>|Lines], Playlist) ->
  read_mbr_playlist(Lines, Playlist);

read_mbr_playlist([<<"#EXT-X-STREAM-INF:", Opts/binary>>, URL|Lines], #m3u8_mbr_playlist{playlists = SubLists} = Playlist) ->
  Options = [ list_to_tuple(binary:split(Opt, <<"=">>)) || Opt <- binary:split(Opts, <<",">>,[global])],
  Bitrate = to_i(proplists:get_value(<<"BANDWIDTH">>, Options)),
  read_mbr_playlist(Lines, Playlist#m3u8_mbr_playlist{playlists = [#m3u8_playlist{url = URL, bitrate = Bitrate}|SubLists]});

read_mbr_playlist([], #m3u8_mbr_playlist{playlists = SubLists} = Playlist) ->
  Playlist#m3u8_mbr_playlist{playlists = lists:reverse(SubLists)}.




read_playlist([<<"#EXT-X-MEDIA-SEQUENCE:",Sequence/binary>>|Lines], Playlist, _) ->
  SeqNumber = to_i(Sequence),
  read_playlist(Lines, Playlist#m3u8_playlist{sequence = SeqNumber}, SeqNumber);

read_playlist([<<"#EXT-X-PLAYLIST-TYPE:",Type_/binary>>|Lines], #m3u8_playlist{} = Playlist, Number) ->
  Type = case Type_ of
    <<"VOD">> -> vod;
    <<"EVENT">> -> event;
    _ -> Type_
  end,
  read_playlist(Lines, Playlist#m3u8_playlist{type = Type}, Number);

read_playlist([<<"#EXTINF:",_/binary>>|_] = Lines, #m3u8_playlist{} = Playlist, undefined) ->
  read_playlist(Lines, Playlist#m3u8_playlist{sequence = 0}, 0);

read_playlist([<<"#EXTINF:",Duration/binary>>|Lines], #m3u8_playlist{} = Playlist, Number) ->
  SegDuration = round(to_f(Duration)*1000),
  read_entry(Lines, Playlist, #m3u8_entry{duration = SegDuration, number = Number});

read_playlist([<<"#", _/binary>>|Lines], Playlist, Number) ->
  read_playlist(Lines, Playlist, Number);

read_playlist([_URL|_] = Lines, #m3u8_playlist{} = Playlist, undefined) ->
  read_playlist(Lines, #m3u8_playlist{sequence = 0} = Playlist, 0);

read_playlist([URL|Lines], #m3u8_playlist{entries = Entries} = Playlist, Number) ->
  read_playlist(Lines, Playlist#m3u8_playlist{entries = [#m3u8_entry{url = URL, number = Number}|Entries]}, Number+1);

read_playlist([], #m3u8_playlist{entries = Entries} = Playlist, _) ->
  Playlist#m3u8_playlist{entries = lists:reverse(Entries)}.



read_entry([<<"#", _/binary>>|Lines], Playlist, Entry) ->
  read_entry(Lines, Playlist, Entry);

read_entry([URL|Lines], #m3u8_playlist{entries = Entries} = Playlist, #m3u8_entry{number = N} = Entry) ->
  read_playlist(Lines, Playlist#m3u8_playlist{entries = [Entry#m3u8_entry{url = URL}|Entries]}, N+1);

read_entry([], Playlist, #m3u8_entry{number = N}) ->
  read_playlist([], Playlist, N).



% read_playlist([_Line|Lines], playlist) ->
%   read_playlist(Lines, playlist);

% read_playlist([], #playlist{segments = Segments} = playlist) ->
%   playlist#playlist{segments = lists:reverse(Segments)}.

% read_mbr_playlist([<<"#EXT-X-STREAM-INF:",Info/binary>>, URL|Lines], #mbr{playlists = playlists} = MBR, BaseUrl) ->
%   Bitrate = case re:run(Info, "BANDWIDTH=(\\d+)", [{capture,all_but_first,list}]) of
%     {match, [B]} -> to_f(B) div 1000;
%     nomatch -> undefined
%   end,
%   playlist = #playlist{url = prepend_base_url(URL, BaseUrl), bitrate = Bitrate},
%   read_mbr_playlist(Lines, MBR#mbr{playlists = playlists ++ [playlist]}, BaseUrl);

% read_mbr_playlist([], MBR, _BaseUrl) ->
%   MBR.


prepend_base_path(#m3u8_mbr_playlist{playlists = Playlists} = MbrPlaylist, URL) ->
  BasePath = filename:dirname(URL),
  MbrPlaylist#m3u8_mbr_playlist{url = URL, playlists = [
    Entry#m3u8_playlist{url = prepend_base_path(Path,BasePath)} || #m3u8_playlist{url = Path} = Entry <- Playlists]};

prepend_base_path(#m3u8_playlist{entries = Entries} = Playlist, URL) ->
  BasePath = filename:dirname(URL),
  Playlist#m3u8_playlist{url = URL, entries = [
    Entry#m3u8_entry{url = prepend_base_path(Path,BasePath)} || #m3u8_entry{url = Path} = Entry <- Entries]};

prepend_base_path(<<"/",_/binary>> = Path, _BasePath) -> Path;
prepend_base_path(Path, BasePath) -> <<BasePath/binary, "/", Path/binary>>.



prepend_base_url(#m3u8_mbr_playlist{playlists = Entries} = Playlist, URL) ->
  BaseUrl = filename:dirname(URL),
  Playlist#m3u8_mbr_playlist{url = URL, playlists = [
    Entry#m3u8_playlist{url = prepend_base_url(Path,BaseUrl)} || #m3u8_playlist{url = Path} = Entry <- Entries]};

prepend_base_url(#m3u8_playlist{entries = Entries} = Playlist, URL) ->
  BaseUrl = filename:dirname(URL),
  Playlist#m3u8_playlist{url = URL, entries = [
    Entry#m3u8_entry{url = prepend_base_url(Path,BaseUrl)} || #m3u8_entry{url = Path} = Entry <- Entries]};

prepend_base_url(<<"http://",_/binary>> = URL, _BaseUrl) -> URL;
prepend_base_url(URL, BaseUrl) -> <<BaseUrl/binary, "/", URL/binary>>.


to_i(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin)).

to_f(String) ->
  case re:run(String, "(\\d+\\.\\d+)", [{capture,all_but_first,list}]) of
    {match, [D]} -> list_to_float(D);
    nomatch ->
      case re:run(String, "(\\d+)", [{capture,all_but_first,list}]) of
        {match, [D]} -> list_to_integer(D);
        nomatch -> error({invalid_float_string,String})
      end
  end.











