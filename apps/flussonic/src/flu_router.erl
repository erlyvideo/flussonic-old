-module(flu_router).
-behaviour(cowboy_middleware).

-export([execute/2]).
-export([compile/1]).
-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).
-include("log.hrl").

-export([route/2]).

-export([route/1, route_media_request/1]).


-record(routes, {
  media = [],
  prefixes = [],
  api = []
}).



execute(Req, Env) ->
  {Path,Req1} = cowboy_req:path(Req),
  Router = proplists:get_value(router, Env),
  case route(Path, Router) of
    undefined ->
      {ok, Req1, Env};
    {ok, Routing} ->
      {ok, Req1, [{routing,Routing}|Env]}
  end.


% Known media requests:
%
% manifest.f4m
% bootstrap
% hds/tracks-1,2/Seg0-Frag5
% tracks-1,2/index.m3u8
% index.m3u8
% playlist.m3u8
% mbr.m3u8
% tracks-1,2/hls/segment5.ts
% hls/segment5.ts   -- ?
% archive-1362504585-3600.mp4
% archive-1362504585-3600.ts
% 2012/09/15/18/43/57-03956.ts
% 2012%2F09%2F15%2F18%2F43%2F57-03956.ts
% timeshift_abs/1362504585
% archive/1362504585/3600/mpegts
% archive/1362504585/3600/manifest.f4m
% archive/1362504585/3600/bootstrap
% archive/1362504585/3600/index.m3u8
% index-1362504585-3600.m3u8
% archive/1362504585/3600/0/Seg0-Frag5
%
%
%




compile(Config) ->
  MediaRequests1 = [
     {"/(?<name>.+)/tracks-(?<tracks>[\\d,]+)/index.m3u8", {hls_track_playlist, [name,tracks]}}
    ,{"/(?<name>.+)/tracks-(?<tracks>[\\d,]+)/hls/segment(?<segment>\\d+)\\.ts", {hls_track_segment, [name,tracks,segment]}}
    ,{"/(?<name>.+)/(?<path>\\d{4}/\\d{2}/\\d{2}/\\d{2}/\\d{2}/\\d{2}-\\d+)\\.ts", {hls_segment, [name,path]}}
    ,{"/(?<name>.+)/archive-(?<from>\\d+)-(?<duration>\\d+)\\.mp4", {archive_mp4,[name,from,duration]}} % require Req
    ,{"/(?<name>.+)/archive-(?<from>\\d+)-(?<duration>\\d+)\\.ts", {archive_ts,[name,from,duration]}} % require Req
    ,{"/(?<name>.+)/archive/(?<from>\\d+)/(?<duration>\\d+)/mpegts", {archive_mpegts,[name,from,duration]}} % require Req
    ,{"/(?<name>.+)/archive/(?<from>\\d+)/(?<duration>\\d+|now)/manifest.f4m", {archive_hds,[name,from,duration]}}
    ,{"/(?<name>.+)/archive/(?<from>\\d+)/(?<duration>\\d+|now)/bootstrap", {archive_bootstrap,[name,from,duration]}}
    ,{"/(?<name>.+)/archive/(?<from>\\d+)/(?<duration>\\d+|now)/\\d+/Seg\\d+-Frag(?<fragment>\\d+)", {archive_fragment,[name,from,duration,fragment]}}
    ,{"/(?<name>.+)/archive/(?<from>\\d+)/(?<duration>\\d+|now)/index.m3u8", {archive_hls_long,[name,from,duration]}}
    ,{"/(?<name>.+)/index-(?<from>\\d+)-(?<duration>\\d+|now).m3u8", {archive_hls,[name,from,duration]}}
    ,{"/(?<name>.+)/timeshift_abs/(?<from>\\d+)", {timeshift_abs,[name, from]}} % require Req
    ,{"/(?<name>.+)/timeshift_rel/(?<from>\\d+)", {timeshift_rel,[name, from]}} % require Req
    ,{"/(?<name>.+)/manifest.f4m", {hds_manifest, [name]}}    
    ,{"/(?<name>.+)/bootstrap", {hds_bootstrap, [name]}}    
    ,{"/(?<name>.+)/hds/\\d+/Seg\\d-Frag(?<fragment>\\d+)", {hds_fragment, [name, fragment]}}
    ,{"/(?<name>.+)/hds/tracks-(?<tracks>[\\d,]+)/Seg\\d-Frag(?<fragment>\\d+)", {hds_track_fragment, [name, tracks, fragment]}}
    ,{"/(?<name>.+)/index.m3u8", {hls_playlist, [name]}}
    ,{"/(?<name>.+)/hls/segment(?<segment>\\d+)\\.ts", {hls_file_segment, [name,segment]}}
    ,{"/(?<name>.+)/playlist.m3u8", {hls_playlist, [name]}}
    ,{"/(?<name>.+)/mbr.m3u8", {hls_mbr_playlist, [name]}}
    ,{"/(?<name>.+)/mpegts", {mpegts, [name]}}
  ],
  MediaRequests2 = [begin
    {ok, MP} = re:compile(Re),
    {MP, Spec}
  end || {Re,Spec} <- MediaRequests1],

  Prefixes = lists:flatmap(fun
    ({stream, Name, URL, Options}) -> [{Name, size(Name), false, {stream, URL, binarize(Options)}}];
    ({file, Prefix, Directory, Options}) -> [{Prefix, size(Prefix), true, {file, Directory, binarize(Options)}}];
    ({live, Prefix, Options}) -> [{Prefix, size(Prefix), true, {live, binarize(Options)}}];
    (_) -> []
  end, Config),


  ApiOptions = proplists:get_value(api, Config, []),

  #routes{media = MediaRequests2, prefixes = Prefixes, api = ApiOptions}.

binarize([]) -> [];
binarize([{dvr,Root}|Options]) -> [{dvr,to_b(Root)}|binarize(Options)];
binarize([Opt|Options]) -> [Opt|binarize(Options)].




route(Path) ->
  route(Path, undefined).

route(Path, undefined) ->
  route(Path, compile([]));


% Для принятия решения нужны две вещи:
% 1) распарсить медиа-запрос, получить его детали
% 2) по имени потока определить контекст: файл, стрим, паблиш и т.п.
%
% 2-й этап непростой, потому что варьируется между разными ситуациями:
% 1) статический стрим
% 2) динамический публикуемый стрим
% 3) файл
% 4) dvr
%
% Стримы и файлы имеют внешнее имя (vod/hobbyt.mp4, live/ustream, ort) и урл (/var/movies/video.mp4,
% dvr:/storage/dvr/ort, rtsp://) и т.п.

route(<<"/">>, #routes{api = Opts}) -> api(<<"mainpage">>, Opts);
route(<<"/admin">>, #routes{api = Opts}) -> api(<<"mainpage">>, Opts);
route(<<"/erlyvideo/api/", Api/binary>>, #routes{api = Opts}) -> api(Api, Opts);

route(Path, #routes{media = Media, prefixes = Prefixes}) ->
  case route_media_request(Path, Media) of
    undefined ->
      undefined;
    {_, _} = Routing ->
      case handler(Prefixes, Routing) of
        undefined -> undefined;
        MFA -> {ok, MFA}
      end
  end.


api(Command, Opts) ->
  case api0(Command, Opts) of
    undefined -> undefined;
    MFA -> {ok, MFA}
  end.

api0(<<"mainpage">>, Opts) ->
  {api_handler, mainpage, [req,Opts], [{tag,html}|Opts]};
api0(<<"sendlogs">>, Opts) ->
  {api_handler, sendlogs, [req,Opts], Opts};
api0(<<"reload">>, Opts) ->
  {api_handler, reload, [req,Opts], Opts};
api0(<<"streams">>, Opts) ->
  {api_handler, streams, [req,Opts], Opts};
api0(<<"sessions">>, Opts) ->
  {api_handler, sessions, [req,Opts], Opts};
api0(<<"server">>, Opts) ->
  {api_handler, server, [req,Opts], Opts};
api0(<<"pulse">>, Opts) ->
  {api_handler, pulse, [req,Opts], Opts};
api0(<<"stream_health/", Name/binary>>, Opts) ->
  {api_handler, health, [req,Name,Opts], Opts};
api0(<<"stream_restart/", Name/binary>>, Opts) ->
  {api_handler, stream_restart, [req,Name,Opts], Opts};
api0(<<"media_info/", Name/binary>>, Opts) ->
  {api_handler, media_info, [req,Name,Opts], Opts};
api0(<<"dvr_status/", Status/binary>>, Opts) ->
  case re:run(Status, "(?<year>\\d{4})/(?<month>\\d+)/(?<day>\\d+)/(?<name>.+)", [{capture,[year,month,day,name],binary}]) of
    {match, [Y,M,D,Name]} ->
      {api_handler, dvr_status, [req,to_i(Y),to_i(M),to_i(D),Name,Opts], Opts};
    nomatch ->
      {flu_www, bad_request, [req], []}
  end;
api0(_, _Opts) ->
  undefined.




route_media_request(Path) ->
  #routes{media = Media} = compile([]),
  route_media_request(Path, Media).

route_media_request(_Path, []) ->
  undefined;

route_media_request(Path, [{Re, {Request, MatchNames}}|Media]) ->
  case re:run(Path, Re, [{capture,MatchNames,binary}]) of
    nomatch ->
      route_media_request(Path, Media);
    {match, Matches} ->
      {Request, Matches}
  end.



handler(Prefixes, {Request, [Name|Matches]}) ->
  handler(Prefixes, Name, Request, Matches).

handler([], _, _, _) ->
  undefined;

handler([{Prefix,PrefixLen,IsPrefixed,Spec}|Prefixes], Name, Request, Matches) ->
  case Name of
    Prefix when not IsPrefixed -> 
      mfa(Request, Spec, Name, Matches);
    <<Prefix:PrefixLen/binary, "/", PathInfo/binary>> when IsPrefixed -> 
      mfa(Request, Spec, Name, [PathInfo|Matches]);
    _ ->
      handler(Prefixes, Name, Request, Matches)
  end.


mfa(hls_playlist, {stream, _URL, Options}, Name, []) ->
  {flu_stream, hls_playlist, [Name], [{tag,hls},{type,<<"hls">>},{name,Name}|Options]};

mfa(hls_playlist, {live, Options}, Name, _) ->
  {flu_stream, hls_playlist, [Name], [{tag,hls},{type,<<"hls">>},{name,Name}|Options]};

mfa(hls_playlist, {file, Directory, Options}, Name, [PathInfo|_]) ->
  {flu_file, hls_playlist, [<<Directory/binary, "/", PathInfo/binary>>, Name], [{tag,hls},{type,<<"hls">>},{name,Name}|Options]};

mfa(hls_mbr_playlist, {file, Directory, Options}, Name, [PathInfo]) ->
  {flu_file, hls_mbr_playlist, [<<Directory/binary, "/", PathInfo/binary>>, Name], [{tag,hls},{type,<<"hls">>},{name,Name}|Options]};

mfa(hds_track_fragment, {file, Directory, Options}, Name, [PathInfo,Tracks_,Fragment_]) ->
  Tracks = [to_i(Track) || Track <- binary:split(Tracks_, <<",">>, [global])],
  {flu_file, hds_fragment, [<<Directory/binary, "/", PathInfo/binary>>, Name, Tracks, to_i(Fragment_)], [{tag,hds_d},{name,Name}|Options]};

mfa(hls_track_playlist, {file, Directory, Options}, Name, [PathInfo,Tracks_]) ->
  Tracks = [to_i(Track) || Track <- binary:split(Tracks_, <<",">>, [global])],
  {flu_file, hls_playlist, [<<Directory/binary, "/", PathInfo/binary>>, Name, Tracks], [{tag,hls},{name,Name}|Options]};

mfa(hls_track_segment, {file, Directory, Options}, Name, [PathInfo,Tracks_, Segment_]) ->
  Tracks = [to_i(Track) || Track <- binary:split(Tracks_, <<",">>, [global])],
  {flu_file, hls_segment, [<<Directory/binary, "/", PathInfo/binary>>, Name, Tracks, to_i(Segment_)], [{tag,hls_d},{name,Name}|Options]};

mfa(hls_file_segment, {file, Directory, Options}, Name, [PathInfo, Segment_]) ->
  {flu_file, hls_segment, [<<Directory/binary, "/", PathInfo/binary>>, Name, to_i(Segment_)], [{tag,hls_d},{name,Name}|Options]};

mfa(hls_segment, {stream, _URL, Options}, Name, [Path]) ->
  {flu_stream, hls_segment, [dvr(Options), Name, Path], [{tag,hls_d},{name,Name}|Options]};

mfa(hls_segment, {live, Options}, Name, [_PathInfo, Path]) ->
  {flu_stream, hls_segment, [dvr(Options), Name, Path], [{tag,hls_d},{name,Name}|Options]};



mfa(archive_mp4, {stream, _, Options}, Name, [From_, Duration_]) ->
  {dvr_handler, mp4, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{tag,mp4},{type,<<"mp4">>},{name,Name}|Options]};
mfa(archive_mp4, {live, Options}, Name, [_PathInfo, From_, Duration_]) ->
  {dvr_handler, mp4, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{tag,mp4},{type,<<"mp4">>},{name,Name}|Options]};

mfa(archive_ts, {stream, _, Options}, Name, [From_, Duration_]) ->
  {dvr_handler, ts_file, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{type,<<"mpegts">>},{tag,hls_d},{name,Name}|Options]};
mfa(archive_ts, {live, Options}, Name, [_PathInfo, From_, Duration_]) ->
  {dvr_handler, ts_file, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{type,<<"mpegts">>},{tag,hls_d},{name,Name}|Options]};

mfa(archive_mpegts, {stream, _, Options}, Name, [From_, Duration_]) ->
  {dvr_handler, ts_stream, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};
mfa(archive_mpegts, {live, Options}, Name, [_PathInfo, From_, Duration_]) ->
  {dvr_handler, ts_stream, [req, dvr(Options), Name, to_i(From_), to_duration(Duration_)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};


mfa(archive_hds, {stream, _, Options}, Name, [From, Duration]) ->
  {dvr_session, hds_manifest, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hds},{type,<<"hds_dvr">>},{name,Name}|Options]};
mfa(archive_hds, {live, Options}, Name, [_PathInfo, From, Duration]) ->
  {dvr_session, hds_manifest, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hds},{type,<<"hds_dvr">>},{name,Name}|Options]};

mfa(archive_bootstrap, {stream, _, Options}, Name, [From, Duration]) ->
  {dvr_session, bootstrap, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hds_b},{type,<<"hds_dvr">>},{name,Name}|Options]};
mfa(archive_bootstrap, {live, Options}, Name, [_PathInfo, From, Duration]) ->
  {dvr_session, bootstrap, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hds_b},{type,<<"hds_dvr">>},{name,Name}|Options]};

mfa(archive_fragment, {stream, _, Options}, Name, [From, Duration, Fragment]) ->
  {dvr_session, hds_fragment, [dvr(Options), Name, to_i(From), to_duration(Duration), to_i(Fragment)], [{tag,hds_d},{name,Name}|Options]};
mfa(archive_fragment, {live, Options}, Name, [_PathInfo, From, Duration, Fragment]) ->
  {dvr_session, hds_fragment, [dvr(Options), Name, to_i(From), to_duration(Duration), to_i(Fragment)], [{tag,hds_d},{name,Name}|Options]};

mfa(archive_hls_long, {stream, _, Options}, Name, [From, Duration]) ->
  {flu_www, redirect, [<<"/", Name/binary, "/index-", From/binary, "-", Duration/binary, ".m3u8">>], [{tag,none},{name,Name}|Options]};
mfa(archive_hls_long, {live, Options}, Name, [_PathInfo, From, Duration]) ->
  {flu_www, redirect, [<<"/", Name/binary, "/index-", From/binary, "-", Duration/binary, ".m3u8">>], [{tag,none},{name,Name}|Options]};

mfa(archive_hls, {stream, _, Options}, Name, [From, Duration]) ->
  {dvr_session, hls_playlist, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hls},{type,<<"hls_dvr">>},{name,Name}|Options]};
mfa(archive_hls, {live, Options}, Name, [_PathInfo, From, Duration]) ->
  {dvr_session, hls_playlist, [dvr(Options), Name, to_i(From), to_duration(Duration)], [{tag,hls},{type,<<"hls_dvr">>},{name,Name}|Options]};


mfa(timeshift_abs, {stream, _, Options}, Name, [From]) ->
  {dvr_handler, timeshift_abs, [req, dvr(Options), Name, to_i(From)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};
mfa(timeshift_abs, {live, Options}, Name, [_PathInfo, From]) ->
  {dvr_handler, timeshift_abs, [req, dvr(Options), Name, to_i(From)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};


mfa(timeshift_rel, {stream, _, Options}, Name, [From]) ->
  {dvr_handler, timeshift_rel, [req, dvr(Options), Name, to_i(From)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};
mfa(timeshift_rel, {live, Options}, Name, [_PathInfo, From]) ->
  {dvr_handler, timeshift_rel, [req, dvr(Options), Name, to_i(From)], [{pid,self()},{type,<<"mpegts_dvr">>},{tag,hls_d},{name,Name}|Options]};


mfa(hds_manifest, {stream, _, Options}, Name, []) ->
  {flu_stream, hds_manifest, [Name], [{tag,hds},{type,<<"hds">>},{name,Name}|Options]};
mfa(hds_manifest, {live, Options}, Name, [_PathInfo]) ->
  {flu_stream, hds_manifest, [Name], [{tag,hds},{type,<<"hds">>},{name,Name}|Options]};
mfa(hds_manifest, {file, Directory, Options}, Name, [PathInfo]) ->
  {flu_file, hds_manifest, [<<Directory/binary, "/", PathInfo/binary>>, Name], [{tag,hds},{type,<<"hds">>},{name,Name}|Options]};


mfa(hds_bootstrap, {stream, _, Options}, Name, []) ->
  {flu_stream, bootstrap, [Name], [{tag,hds_b},{type,<<"hds">>},{name,Name}|Options]};
mfa(hds_bootstrap, {live, Options}, Name, [_PathInfo]) ->
  {flu_stream, bootstrap, [Name], [{tag,hds_b},{type,<<"hds">>},{name,Name}|Options]};
mfa(hds_bootstrap, {file, Directory, Options}, Name, [PathInfo]) ->
  {flu_file, bootstrap, [<<Directory/binary, "/", PathInfo/binary>>, Name], [{tag,hds_b},{type,<<"hds">>},{name,Name}|Options]};


mfa(hds_fragment, {stream, _, Options}, Name, [Fragment]) ->
  {flu_stream, hds_fragment, [Name, to_i(Fragment)], [{tag,hds_d},{name,Name}|Options]};
mfa(hds_fragment, {live, Options}, Name, [_PathInfo, Fragment]) ->
  {flu_stream, hds_fragment, [Name, to_i(Fragment)], [{tag,hds_d},{name,Name}|Options]};

mfa(hds_fragment, {file, Directory, Options}, Name, [PathInfo, Fragment]) ->
  {flu_file, hds_fragment, [<<Directory/binary, "/", PathInfo/binary>>, Name, to_i(Fragment)], [{tag,hds_d},{name,Name}|Options]};


mfa(mpegts, {stream, _, Options}, Name, []) ->
  {mpegts_handler, request, [req, Name, Options], [{pid,self()},{tag,hls_d},{type,<<"mpegts">>},{name,Name}|Options]};
mfa(mpegts, {live, Options}, Name, [_PathInfo]) ->
  {mpegts_handler, request, [req, Name, Options], [{pid,self()},{tag,hls_d},{type,<<"mpegts">>},{name,Name}|Options]};


mfa(_Request, _Spec, _Name, _Matches) ->
  error({_Request, _Spec, _Name, _Matches}),
  undefined.








% merge(Opts1, Opts2) ->
%   lists:ukeymerge(1, lists:ukeysort(1, Opts1), lists:ukeysort(1,Opts2)).

dvr(Options) -> proplists:get_value(dvr, Options).


to_i(B) when is_binary(B) -> list_to_integer(binary_to_list(B));
to_i(B) when is_list(B) -> list_to_integer(B);
to_i(B) when is_integer(B) -> B.

to_b(B) when is_binary(B) -> B;
to_b(L) when is_list(L) -> list_to_binary(L);
to_b(undefined) -> undefined.


to_duration(B) ->
  case re:run(B, "^(\\d+)$", []) of
    {match, _} -> to_i(B);
    nomatch -> binary_to_existing_atom(to_b(B), latin1)
  end.






start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(router, {

}).

init([]) ->
  {ok, #router{
  }}.

handle_info(_Msg, #router{} = State) ->
  ?D(_Msg),
  {noreply, State}.

terminate(_,_) -> ok.



