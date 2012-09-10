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

-export([read/3]).
-export([record/3, announce/3, describe/3, play/3]).


read(Stream, URL, Options) ->
  {ok, Proxy} = flussonic_sup:start_stream_helper(Stream, publish_proxy, {flu_publish_proxy, start_link, [undefined, self()]}),
  {ok, RTSP, #media_info{streams = Streams} = MediaInfo} = rtsp_socket:read(URL, [{consumer,Proxy}|Options]),
  Proxy ! {set_source, RTSP},
  Streams1 = [Info#stream_info{track_id = TrackId + 199} || #stream_info{track_id = TrackId} = Info <- Streams],
  {ok, RTSP, MediaInfo#media_info{streams = Streams1}}.
  

hostpath(URL) ->
  {HostPort, Path} = http_uri2:extract_path_with_query(URL),
  {ems:host(HostPort), string:strip(Path,both,$/)}.


announce(URL, Headers, MediaInfo) ->
  try announce0(URL, Headers, MediaInfo) of
    Reply -> Reply
  catch
    throw:Error -> {error, Error};
    Class:Error ->
      error_logger:error_msg("Error in flu_rtsp:announce/3: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
      {error, error}
  end.


announce0(URL, Headers, #media_info{streams = Streams} = MediaInfo1) ->
  Streams1 = [Info#stream_info{track_id = TrackId + 199} || #stream_info{track_id = TrackId} = Info <- Streams],
  MediaInfo = MediaInfo1#media_info{streams = Streams1},
  ?D({rtsp_announce, URL}),
  
  {ok, Env} = application:get_env(flussonic, config),
  {Prefix, Options} = case [Entry || {live,_Pref,_Options} = Entry <- Env] of
    [{live, _Prefix, Opts}|_] -> {_Prefix, Opts};
    [] -> []
  end,
  
  {rtsp, _Auth, _Host, _Port, "/" ++ StreamName1, _Query} = http_uri2:parse(URL),
  StreamName = case re:run(StreamName1, "(.*)\\.sdp", [{capture,all_but_first,binary}]) of
    {match, [S]} -> S;
    nomatch -> list_to_binary(StreamName1)
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
  gen_tracker:setattr(flu_streams, StreamName, [{play_prefix,list_to_binary(Prefix)}]),
  flu_stream:set_source(Recorder, self()),
  
  {ok, Proxy} = flussonic_sup:start_stream_helper(StreamName, publish_proxy, {flu_publish_proxy, start_link, [self(), Recorder]}),
  erlang:monitor(process, Recorder),
  {ok, Proxy}.
  

record(URL, _Headers, _Body) ->
  ?D({record, URL}),
  ok.

describe(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  {Module, Function} = ems:check_app(Host, auth, 3),
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Session ->
      {ok, Media} = media_provider:open(Host, Path),
      {ok, Media}
  end.

play(URL, _Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  % {Module, Function} = ems:check_app(Host, auth, 3),
  ems_log:access(Host, "RTSP PLAY ~s ~s", [Host, Path]),
  {ok, Media} = media_provider:play(Host, Path, [{stream_id,1}, {client_buffer,50}, {burst_size, 1}]),
  ems_network_lag_monitor:watch(self()),
  {ok, Media}.
