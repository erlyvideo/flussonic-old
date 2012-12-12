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
-export([read_loop/4]).
-export([write_loop/4]).


-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-record(mpegts, {
  pid,
  name,
  options,
  method
}).

init({_Any,http}, Req, Opts) ->
  {PathInfo, Req1} = cowboy_req:path_info(Req),
  Name = case proplists:get_value(name, Opts) of
    undefined when PathInfo =/= undefined -> flu:join(PathInfo, "/");
    Name_ -> Name_
  end,
  flu_stream:autostart(Name, Opts),
  {Method, Req2} = cowboy_req:method(Req1),
  {ok, Req2, #mpegts{name = Name, method = Method, options = Opts}}.

handle(Req, State) ->
  try handle0(Req, State) of
    Reply -> Reply
  catch
    throw:{return,Code,Text} ->
      {ok, R1} = cowboy_req:reply(Code, [], Text, Req),
      {ok, R1, undefined}
  end.


handle0(Req, #mpegts{name = RawName, options = Options, method = <<"GET">>} = State) ->
  Socket = cowboy_req:get(socket,Req),
  Transport = cowboy_req:get(transport,Req),
  
  {ok, {Name,_}} = media_handler:check_sessions(Req, RawName, Options),

  OurName = iolist_to_binary(io_lib:format("mpegts_client(~s)", [Name])),
  erlang:put(name, OurName),

  {ok, Pid} = case flu_stream:autostart(Name, Options) of
     {ok, P} -> {ok, P};
     _ -> throw({return, 404, "No stream found\n"})
  end,
  Streams = case flu_stream:media_info(Name) of
    undefined -> throw({return, 404, "Stream not started\n"});
    #media_info{streams = Streams_} -> Streams_
  end,

  Mpegts = mpegts:init([{resync_on_keyframe,true}]),
  flu_stream:subscribe(Pid, Options),
  ?D({mpegts_play,Name}),
  inet:setopts(Socket, [{send_timeout,10000},{sndbuf,1200000}]),
  Transport:send(Socket, "HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\nConnection: close\r\n\r\n"),
  Started = length([S || #stream_info{content = video} = S <- Streams]) == 0,
  case (catch write_loop(Socket, Transport, Mpegts, Started)) of
    {'EXIT', Error} -> ?D({exit,Error,erlang:get_stacktrace()});
    ok -> ok;
    {error, closed} -> ok;
    _Else -> ?D(_Else)
  end,
  ?D({mpegts_play,Name,stop}),
  {ok, Req, State};

handle0(Req, #mpegts{name = StreamName, options = Options, method = <<"PUT">>} = State) ->
  Socket = cowboy_http:get(socket,Req),
  Transport = cowboy_http:get(transport,Req),
  
  ?D({mpegts_input,StreamName}),
  
  {ok, Recorder} = flu_stream:autostart(StreamName, Options),
  {ok, Reader} = mpegts_decoder:init(),
  
  ?MODULE:read_loop(Recorder, Reader, Transport, Socket),
  ?D({exit,mpegts_reader}),
  {ok, Req, State}.


terminate(_,_) -> ok.

  
  
read_loop(Recorder, Reader, Transport, Socket) ->
  case Transport:recv(Socket, 16*1024, 10000) of
    {ok, Bin} ->
      {ok, Reader1, Frames} = mpegts_decoder:decode(Bin, Reader),
      [Recorder ! Frame || Frame <- Frames],
      ?MODULE:read_loop(Recorder, Reader1, Transport, Socket);
    Else ->
      Else
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
      ok = Transport:send(Socket, Data),
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
    {'DOWN', _, _, _, _} ->
      ok;
    Message ->
      ?D(Message)
  after
    20000 ->
      timeout
  end.
  

