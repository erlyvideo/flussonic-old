%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        mpegts handler
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
-module(mpegts_handler).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/3, handle/2, terminate/2]).
-export([input/2, read_loop/4]).
-export([output/2, write_loop/4]).


-include_lib("cowboy/include/http.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include("log.hrl").

-record(mpegts, {
  pid,
  name,
  options,
  method
}).

init({_Any,http}, Req, Opts) ->
  {PathInfo, Req1} = cowboy_http_req:path_info(Req),
  Name = flu:join(PathInfo, "/"),
  {Method, Req2} = cowboy_http_req:method(Req1),
  {ok, Req2, #mpegts{name = Name, method = Method, options = Opts}}.

handle(Req, #mpegts{name = RawName, options = Options, method = 'GET'} = State) ->
  Socket = Req#http_req.socket,
  Transport = Req#http_req.transport,
  
  Name = media_handler:check_sessions(Req, RawName, Options),

  case flu_stream:autostart(Name, Options) of
    {ok, Pid} ->
      Mpegts = mpegts:init(),
      {Mpegts1, <<>>} = mpegts:encode(Mpegts, flu_stream:media_info(Name)),
      flu_stream:subscribe(Pid, Options),
      ?D({mpegts_play,Name}),
      Transport:send(Socket, "HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\nConnection: close\r\n\r\n"),
      case (catch write_loop(Socket, Transport, Mpegts1, false)) of
        {'EXIT', Error} -> ?D({exit,Error,erlang:get_stacktrace()});
        _Else -> ?D(_Else)
      end,
      ?D({mpegts_play,Name,stop}),
      {ok, Req, State};
    _ ->
      {ok, R1} = cowboy_http_req:reply(404, [], "No stream found\n", Req),
      {ok, R1, undefined}
  end.  

terminate(_,_) -> ok.


input(Req, Env) ->
  Socket = Req#http_req.socket,
  Transport = Req#http_req.transport,
  StreamName = proplists:get_value(path, Env),
  {ok, Req1} = cowboy_http_req:reply(200, Req),
  
  ?D({mpegts_input,StreamName}),
  
  {ok, Recorder} = flu_stream:autostart(StreamName,Env),
  {ok, Reader} = mpegts_reader:init([]),
  
  ?MODULE:read_loop(Recorder, Reader, Transport, Socket),
  ?D({exit,mpegts_reader}),
  {ok, Req1}.
  
  
read_loop(Recorder, Reader, Transport, Socket) ->
  case Transport:recv(Socket, 16*1024, 10000) of
    {ok, Bin} ->
      {ok, Reader1, Frames} = mpegts_reader:decode(Bin, Reader),
      [Recorder ! Frame || Frame <- Frames],
      ?MODULE:read_loop(Recorder, Reader1, Transport, Socket);
    Else ->
      Else
  end.

    
output(Req, Env) ->
  Socket = Req#http_req.socket,
  Transport = Req#http_req.transport,
  
  case flu_media:find_or_open(Env) of
    {ok, {stream, StreamName}} ->
      Mpegts = mpegts:init(),
      {Mpegts1, <<>>} = mpegts:encode(Mpegts, flu_stream:media_info(StreamName)),
      flu_stream:subscribe(StreamName, Env),
      ?D({mpegts_play,StreamName, Mpegts1}),
      Transport:send(Socket, "HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\nConnection: close\r\n\r\n"),
      case (catch write_loop(Socket, Transport, Mpegts1, false)) of
        {'EXIT', Error} -> ?D({exit,Error,erlang:get_stacktrace()});
        _Else -> ?D(_Else)
      end,
      ?D({mpegts_play,StreamName,stop}),
      {ok, Req};
    _ ->
      unhandled
  end.  

write_loop(Socket, Transport, Mpegts, Started) ->
  receive
    #video_frame{flavor = config} = F ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      {Mpegts1, <<>>} = mpegts:encode(Mpegts, F),
      ?MODULE:write_loop(Socket, Transport, Mpegts1, Started);
    #video_frame{flavor = keyframe} = F when Started == false ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      {Mpegts1, Data} = mpegts:encode(Mpegts, F),
      Transport:send(Socket, Data),
      ?MODULE:write_loop(Socket, Transport, Mpegts1, true);
    #video_frame{} when Started == false ->
      ?MODULE:write_loop(Socket, Transport, Mpegts, Started);    
    #video_frame{} = F ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      case mpegts:encode(Mpegts, F) of
        {Mpegts1, <<>>} -> 
          ?MODULE:write_loop(Socket, Transport, Mpegts1, Started);
        {Mpegts1, Data} ->
          case Transport:send(Socket, Data) of
            ok -> ?MODULE:write_loop(Socket, Transport, Mpegts1, Started);
            Else -> Else
          end
      end;
    Message ->
      ?D(Message)
  end.
  
        
        
            
