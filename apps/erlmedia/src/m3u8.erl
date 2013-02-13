-module(m3u8).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include("../include/m3u8.hrl").



-export([parse/1]).
-export([fetch/1, fetch/2]).


-export([prepend_base_path/2]).

fetch(URL) ->
  fetch(URL, []).

fetch(URL, Options) when is_list(URL) ->
  fetch(list_to_binary(URL), Options);

fetch(URL, Options) when is_binary(URL) ->
  Relative = proplists:get_value(relative, Options, true),
  Playlist1 = case http_uri:parse(binary_to_list(URL)) of
    {error, no_scheme} ->
      case file:read_file(URL) of
        {ok, Bin} ->
          Playlist = parse(Bin),
          case Relative of
            true -> prepend_base_path(Playlist, URL);
            _ -> Playlist
          end;
        {error, _} = Error ->
          Error
      end;
    {ok, _} ->
      case http_stream:request_body(URL, [{keepalive,false},{no_fail,true}]) of
        {ok, {_,200,_,Bin}} ->
          Playlist = parse(Bin),
          case Relative of
            true -> prepend_base_url(Playlist, URL);
            _ -> Playlist
          end;
        {ok, Reply} ->
          {error, {fetch_manifest,URL, Reply}};
        {error, Error} ->
          {error, {fetch_manifest,URL, Error}}
      end
  end,
  Playlist1.






parse(Bin) when is_binary(Bin) ->
  Lines1 = [L || L <- binary:split(Bin, [<<"\r">>, <<"\n">>], [global]), L =/= <<>>],
  Lines2 = tokenize(Lines1),
  detect_playlist_type(Lines2).


tokenize([<<"#EXTM3U">>|Lines]) ->
  tokenize(Lines);

tokenize([<<"#EXT-X-VERSION:",_/binary>>|Lines]) ->
  tokenize(Lines);

tokenize([<<"#EXT-X-STREAM-INF:", Opts/binary>>|Lines]) ->
  Options = [ list_to_tuple(binary:split(Opt, <<"=">>)) || Opt <- binary:split(Opts, [<<" ">>, <<",">>],[global]), Opt =/= <<>>],
  Bitrate = to_i(proplists:get_value(<<"BANDWIDTH">>, Options)),
  [{stream_inf,[{bitrate,Bitrate}|Options]}|tokenize(Lines)];

tokenize([<<"#EXT-X-MEDIA-SEQUENCE:",Sequence/binary>>|Lines]) ->
  [{sequence,to_i(Sequence)}|tokenize(Lines)];

tokenize([<<"#EXT-X-PLAYLIST-TYPE:VOD">>|Lines]) ->
  [{type,vod}|tokenize(Lines)];

tokenize([<<"#EXT-X-PLAYLIST-TYPE:EVENT">>|Lines]) ->
  [{type,event}|tokenize(Lines)];

tokenize([<<"#EXT-X-MEDIA:", Media/binary>>|Lines]) ->
  [{media,parse_x_media(Media)}|tokenize(Lines)];

tokenize([<<"#EXTINF:",Duration/binary>>|Lines]) ->
  [{duration, to_i_1000(Duration)}|tokenize(Lines)];

tokenize([<<"#EXT-X-UTC:",UTC/binary>>|Lines]) ->
  [{utc, to_i(UTC)}|tokenize(Lines)];

tokenize([<<"#EXT-X-PROGRAM-DATE-TIME:",Y:4/binary,"-",M:2/binary,"-",D:2/binary,_,H:2/binary,":",Min:2/binary,":",S:2/binary,_/binary>>|Lines]) ->
  [{date,{{to_i(Y),to_i(M),to_i(D)},{to_i(H),to_i(Min),to_i(S)}}}|tokenize(Lines)];

tokenize([<<"#", _/binary>> = Line|Lines]) ->
  [{control,Line}|tokenize(Lines)];

tokenize([<<>>|Lines]) ->
  tokenize(Lines);

tokenize([URL|Lines]) ->
  [{url,URL}|tokenize(Lines)];

tokenize([]) ->
  [].




detect_playlist_type([{media,_}|_] = Lines) ->
  read_mbr_playlist(Lines, #m3u8_mbr_playlist{});

detect_playlist_type([{stream_inf,_}|_] = Lines) ->
  read_mbr_playlist(Lines, #m3u8_mbr_playlist{});

detect_playlist_type(Lines) ->
  Playlist = read_playlist(Lines, #m3u8_playlist{}, undefined),
  Playlist.


read_mbr_playlist([{media,_}|Lines], Playlist) ->
  read_mbr_playlist(Lines, Playlist);

read_mbr_playlist([{stream_inf,Opts}, {url,URL}|Lines], #m3u8_mbr_playlist{playlists = SubLists} = Playlist) ->
  Bitrate = proplists:get_value(bitrate,Opts),
  read_mbr_playlist(Lines, Playlist#m3u8_mbr_playlist{playlists = [#m3u8_playlist{url = URL, bitrate = Bitrate}|SubLists]});

read_mbr_playlist([], #m3u8_mbr_playlist{playlists = SubLists} = Playlist) ->
  Playlist#m3u8_mbr_playlist{playlists = lists:reverse(SubLists)}.



read_playlist([{sequence,Sequence}|Lines], #m3u8_playlist{} = Playlist, _) ->
  read_playlist(Lines, Playlist#m3u8_playlist{sequence = Sequence}, Sequence);

read_playlist([{type,Type}|Lines], #m3u8_playlist{} = Playlist, Number) ->
  read_playlist(Lines, Playlist#m3u8_playlist{type = Type}, Number);

read_playlist([{media,Entry}|Lines], #m3u8_playlist{entries = Entries} = Playlist, Number) ->
  read_playlist(Lines, Playlist#m3u8_playlist{entries = [Entry#m3u8_entry{number = Number}|Entries]}, Number+1);

read_playlist([{Tag, _}|_] = Lines, #m3u8_playlist{} = Playlist, Number) when 
  Tag == duration; Tag == utc; Tag == date; Tag == url ->
  read_entry(Lines, Playlist, #m3u8_entry{number = Number});

read_playlist([_|Lines], Playlist, Number) ->
  read_playlist(Lines, Playlist, Number);

read_playlist([], #m3u8_playlist{entries = Entries} = Playlist, _) ->
  Playlist#m3u8_playlist{entries = lists:reverse(Entries)}.


read_entry(Lines, #m3u8_playlist{} = Playlist, #m3u8_entry{number = undefined} = Entry) ->
  read_entry(Lines, Playlist, Entry#m3u8_entry{number = 0});

read_entry([{url,URL}|Lines], #m3u8_playlist{entries = Entries} = Playlist, #m3u8_entry{number = N} = Entry) ->
  read_playlist(Lines, Playlist#m3u8_playlist{entries = [Entry#m3u8_entry{url = URL}|Entries]}, N + 1);

read_entry([{utc,UTC}|Lines], Playlist, #m3u8_entry{} = Entry) ->
  read_entry(Lines, Playlist, Entry#m3u8_entry{utc = UTC});

read_entry([{date,Date}|Lines], Playlist, #m3u8_entry{} = Entry) ->
  UTC = calendar:datetime_to_gregorian_seconds(Date) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
  read_entry(Lines, Playlist, Entry#m3u8_entry{utc = UTC});

read_entry([{duration,Duration}|Lines], Playlist, #m3u8_entry{} = Entry) ->
  read_entry(Lines, Playlist, Entry#m3u8_entry{duration = Duration});

read_entry([], Playlist, #m3u8_entry{number = N}) ->
  read_playlist([], Playlist, N + 1).



parse_x_media(Media) when is_binary(Media) ->
  Attrs1 = [binary:split(A, <<"=">>) || A <- binary:split(Media, <<",">>, [global])],
  Attrs2 = [{K,unquote(V)} || [K,V] <- Attrs1],
  Attrs = Attrs2,
  #m3u8_entry{
    url = proplists:get_value(<<"URI">>, Attrs)
    ,duration = to_i_1000(proplists:get_value(<<"DURATION">>, Attrs))
    ,offset = to_i_1000(proplists:get_value(<<"OFFSET">>, Attrs))
    ,utc = to_i(proplists:get_value(<<"UTC">>, Attrs))
  }.



unquote(<<"\"", V/binary>>) -> re:replace(V, "^(.*)\"$", "\\1", [{return,binary}]);
unquote(V) -> V.



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

dirname(URL) when is_binary(URL) ->
  case filename:dirname(URL) of
    <<".">> -> <<>>;
    Dirname -> <<Dirname/binary, "/">>
  end.

prepend_base_path(#m3u8_mbr_playlist{playlists = Playlists} = MbrPlaylist, URL) when is_binary(URL) ->
  BasePath = dirname(URL),
  MbrPlaylist#m3u8_mbr_playlist{url = URL, playlists = [
    Entry#m3u8_playlist{url = prepend_base_path(Path,BasePath)} || #m3u8_playlist{url = Path} = Entry <- Playlists]};

prepend_base_path(#m3u8_playlist{entries = Entries} = Playlist, URL) when is_binary(URL) ->
  BasePath = dirname(URL),
  Playlist#m3u8_playlist{url = URL, entries = [
    Entry#m3u8_entry{url = prepend_base_path(Path,BasePath)} || #m3u8_entry{url = Path} = Entry <- Entries]};

prepend_base_path(<<"/",_/binary>> = Path, _BasePath) -> Path;
prepend_base_path(Path, BasePath) -> <<BasePath/binary, Path/binary>>.



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


to_i_1000(undefined) -> undefined;
to_i_1000(Bin) -> round(to_f(Bin)*1000).


to_i(undefined) -> undefined;
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











