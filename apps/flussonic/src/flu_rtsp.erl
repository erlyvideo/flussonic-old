%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        rtsp
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(flu_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-export([read/3, read2/3]).
-export([record/3, announce/3, describe/3, play/3]).


read(Stream, URL_, Options) ->
  URL = re:replace(URL_, "rtsp1://", "rtsp://", [{return,list}]),
  {ok, Proxy} = flussonic_sup:start_stream_helper(Stream, publish_proxy, {flu_publish_proxy, start_link, [undefined, self()]}),
  {ok, RTSP, #media_info{streams = Streams} = MediaInfo} = old_rtsp_socket:read(URL, [{consumer,Proxy}|Options]),
  Proxy ! {set_source, RTSP},
  Streams1 = [Info#stream_info{track_id = TrackId + 199} || #stream_info{track_id = TrackId} = Info <- Streams],
  {ok, RTSP, MediaInfo#media_info{streams = Streams1}}.
  
read2(Stream, URL, Options) ->
  % {ok, Proxy} = flussonic_sup:start_stream_helper(Stream, publish_proxy, {flu_publish_proxy, start_link, [undefined, self()]}),
  URL1 = re:replace(URL, "rtsp2://", "rtsp://", [{return,list}]),
  % {ok, RTSP} = rtsp_reader:start_link(URL1, [{consumer,self()}|Options]),
  % Proxy ! {set_source, RTSP},
  {ok, RTSP} = flussonic_sup:start_stream_helper(Stream, rtsp_reader, {rtsp_reader, start_link, [URL1, [{consumer,self()}|Options]]}),
  try rtsp_reader:media_info(RTSP) of
    {ok, #media_info{streams = Streams} = MediaInfo} -> 
      {ok, RTSP, MediaInfo#media_info{streams = [S || #stream_info{codec = Codec} = S <- Streams, lists:member(Codec,[h264,mp3,aac])]}}
  catch
    exit:{normal,Reason} -> {error, Reason}
  end.


announce(URL, Headers, MediaInfo) ->
  try announce0(URL, Headers, MediaInfo) of
    Reply -> Reply
  catch
    throw:Error -> {error, Error};
    Class:Error ->
      error_logger:error_msg("Error in flu_rtsp:announce/3: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
      {error, error}
  end.


announce0(URL, Headers, #media_info{} = MediaInfo) ->
  ?D({rtsp_announce, URL}),
  
  Env = flu_config:get_config(),
  {Prefix, Options} = case [Entry || {live,_Pref,_Options} = Entry <- Env] of
    [{live, _Prefix, Opts}|_] -> {_Prefix, Opts};
    [] -> []
  end,

  {rtsp, _Auth, _Host, _Port, "/" ++ StreamName1, _Query} = http_uri2:parse(URL),
  StreamName = case re:run(StreamName1, "(.*)\\.sdp", [{capture,all_but_first,binary}]) of
    {match, [S]} -> S;
    nomatch -> StreamName1
  end,
  
  case proplists:get_value(publish_password, Env) of
    undefined ->
      ok;
    PasswordSpec ->
      case proplists:get_value('Authorization', Headers) of
        <<"Basic ", Basic64/binary>> ->
          Basic = binary_to_list(base64:decode(Basic64)),
          Basic == PasswordSpec orelse throw(authentication);
        undefined ->
          throw(authentication)
      end
  end,


  {ok, Recorder} = flu_stream:autostart(StreamName, [{clients_timeout,false},{static,false}|Options]),
  Recorder ! MediaInfo,
  gen_tracker:setattr(flu_streams, StreamName, [{play_prefix,Prefix}]),
  flu_stream:set_source(Recorder, self()),
  
  {ok, Proxy} = flussonic_sup:start_stream_helper(StreamName, publish_proxy, {flu_publish_proxy, start_link, [self(), Recorder]}),
  erlang:monitor(process, Recorder),
  {ok, Proxy}.
  

record(URL, _Headers, _Body) ->
  ?D({record, URL}),
  ok.

media_info(Stream) ->
  media_info(Stream, 10).

media_info(_Stream, 0) ->
  {error, no_media_info};

media_info(Stream, Count) ->
  case flu_stream:media_info(Stream) of
    undefined ->
      timer:sleep(200),
      media_info(Stream, Count - 1);
    MediaInfo ->
      {ok, MediaInfo}
  end.


describe(URL, Headers, _Body) ->
  {rtsp, _, _Host, _Port, "/"++Path, _} = http_uri2:parse(URL),
  case Path of
    "archive" -> describe_archive(Headers);
    _ -> 
      case flu_media:find_or_open(list_to_binary(Path)) of
        {ok, {file, File}} -> lager:info("RTSP DESCRIBE file ~p", [Path]), {ok, flu_file:media_info(File)};
        {ok, {stream, Stream}} -> lager:info("RTSP DESCRIBE stream ~p", [Path]), media_info(Stream);
        {error, enoent} -> lager:info("RTSP DESCRIBE 404 ~p", [Path]), {error, enoent}
      end
  end.



to_i(List) -> list_to_integer(List).

parse_time(Time) ->
  Time1 = cowboy_http:urldecode(list_to_binary(Time)),
  {match, [Y,Mon,D,H,Min,S]} = re:run(Time1, "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})", [{capture,all_but_first,list}]),
  dvr_minute:timestamp({{to_i(Y),to_i(Mon),to_i(D)}, {to_i(H),to_i(Min),to_i(S)}}).

dvr_session(Headers) ->
  Name = list_to_binary(proplists:get_value("id",Headers)),
  {ok, {stream, Name, Options}} = flu_media:lookup(Name),
  {dvr, Root} = lists:keyfind(dvr, 1, Options),
  {_, From_} = lists:keyfind("from", 1, Headers),
  From = parse_time(From_),
  {_, To_} = lists:keyfind("to", 1, Headers),
  To = parse_time(To_),
  {ok, Session} = dvr_session:autostart(Name, From, To - From, [{root,Root}]),
  {ok, Session}.

describe_archive(Headers) ->
  {ok, Session} = dvr_session(Headers),
  Reply = dvr_session:media_info(Session),
  Reply.


  % {Host, Path} = hostpath(URL),
  % {Module, Function} = ems:check_app(Host, auth, 3),
  % case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
  %   undefined ->
  %     {error, authentication};
  %   _Session ->
  %     {ok, Media} = media_provider:open(Host, Path),
  %     {ok, Media}
  % end.

to_b(undefined) -> undefined;
to_b(List) when is_list(List) -> list_to_binary(List). 

play(URL, Headers, _Body) ->
  {rtsp, _, _Host, _Port, "/"++Path, _} = http_uri2:parse(URL),
  case Path of
    "archive" -> play_archive(Headers);
    _ -> play_media(Path, Headers)
  end.

play_media(Path, Headers) ->
  Name = to_b(Path),
  Ip = to_b(proplists:get_value(ip,Headers)),
  Token = to_b(proplists:get_value("token",Headers)),
  Identity = [{name,Name},{ip, Ip},{token,Token}],
  Params = [{pid,self()},{type,<<"rtsp">>}],
  case flu_media:find_or_open(Name, [{identity,Identity},{session,Params}]) of
    {ok, {stream, Pid}} ->
      flu_stream:subscribe(Name),    
      {ok, {stream, Pid}};
    {ok, {file, Pid}} ->
      {ok, _Ticker} = monotone_ticker:start_link(self(), {flu_file, read_gop, Pid}),
      {ok, {file, Pid}};
    {error, Code} when is_integer(Code) ->
      {fail, Code, <<>>};
    {error, bad_token} ->
      {fail, 403, <<"no_token">>};
    {error, Error} ->
      {fail, 500, io_lib:format("~250p", [Error])}
  end.


% rtsp://127.0.0.1:1554/archive?id=ort&from=2012-12-11%2013:08:00&to=2012-12-11%2013:12:00

play_archive(Headers) ->
  {ok, Session} = dvr_session(Headers),
  {ok, _Ticker} = monotone_ticker:start_link(self(), {dvr_session, read_frame, Session}),
  {ok, {file, Session}}.






















