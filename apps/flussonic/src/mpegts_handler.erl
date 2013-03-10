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

-export([request/3]).
-export([read_loop/3]).
-export([write_loop/3]).
-export([null_packet/1]).


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



request(Req, Name, Options) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Peer, Req2} = cowboy_req:peer_addr(Req1),

  put(name, {mpegts_play,Name,Peer}),

  try handle0(Req2, #mpegts{name = Name, method = Method, options = Options}) of
    {ok, Req3, _} -> {done, Req3};
    ok -> done
  catch
    throw:stop -> done;
    exit:normal -> done;
    exit:timeout -> done;
    exit:enotconn -> done
  end.




handle0(Req, #mpegts{name = Name, options = Options, method = <<"GET">>} = _State) ->
  [Transport, Socket] = cowboy_req:get([transport, socket], Req),

  OurName = iolist_to_binary(io_lib:format("mpegts_client(~s)", [Name])),
  erlang:put(name, OurName),

  {ok, Pid} = flu_stream:autostart(Name, Options),
  ?D({mpegts_play,Name}),
  inet:setopts(Socket, [{send_timeout,10000},{sndbuf,1200000}]),
  Transport:send(Socket, "HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\nConnection: close\r\n\r\n"),

  #media_info{streams = Streams} = await_media_info(Name, Req),

  Mpegts = mpegts:init([{resync_on_keyframe,true}]),
  flu_stream:subscribe(Pid, [{proto,tcp_mpegts},{socket,Socket}|Options]),
  Started = length([S || #stream_info{content = video} = S <- Streams]) == 0,
  ?MODULE:write_loop(Req, Mpegts, Started);

handle0(Req, #mpegts{name = StreamName, options = Options, method = <<"POST">>}) ->
  proplists:get_value(publish_enabled, Options) == true orelse throw({return,403,<<"publish not enabled">>}),


  case proplists:get_value(password, Options) of
    undefined -> ok;
    GoodPassword ->
      {Password, _} = cowboy_req:qs_val(<<"password">>, Req),
      case iolist_to_binary(GoodPassword) of
        Password -> ok;
        _ ->
          lager:info("invalid MPEG-TS publish password: ~p", [Password]),
          throw({return,403,<<"invalid password">>})
      end
  end,

  {TE, Req1} = cowboy_req:header(<<"transfer-encoding">>, Req),
  TE == <<"chunked">> orelse throw({return, 401, <<"need body">>}),

  {ok, Req2} = cowboy_req:reply(200, [], <<>>, Req1),

  {ok, Recorder} = flu_stream:autostart(StreamName, Options),
  flu_stream:set_source(Recorder, self()),
  {ok, Req3} = ?MODULE:read_loop(Recorder, mpegts_decoder:init(), Req2),
  flu_stream:set_source(Recorder, undefined),
  ?D({exit,mpegts_reader}),
  {ok, Req3, undefined}.


await_media_info(Name, Req) ->
  case flu_stream_data:get(Name, media_info) of
    undefined ->
      null_packet(Req),
      Socket = cowboy_req:get(socket, Req),
      inet:setopts(Socket, [{active,once}]),
      receive
        {tcp_closed, Socket} -> throw(stop)
      after
        1000 ->
          inet:setopts(Socket, [{active,false}]),
          await_media_info(Name, Req)
      end;
    {ok, #media_info{} = MI} -> MI
  end.



read_chunk(Req) ->
  cowboy_req:stream_body(Req).

  
read_loop(Recorder, Reader, Req) ->
  case read_chunk(Req) of
    {ok, Chunk, Req1} ->
      {ok, Reader1, Frames} = mpegts_decoder:decode(Chunk, Reader),
      [flu_stream:send_frame(Recorder, Frame) || Frame <- Frames],
      % {ok, <<"\r\n">>} = gen_tcp:recv(Socket, 2),
      ?MODULE:read_loop(Recorder, Reader1, Req1);
    {done, Req1} ->
      {ok, Req1};
    {error, Error} ->
      lager:error("http error capturing mpegts over http: ~p", [Error]),
      {ok, Req}
  end.
    

write_loop(Req, Mpegts, Started) ->
  receive
    #video_frame{} = Frame ->
      handle_frame(Frame, Req, Mpegts, Started);
    {'DOWN', _, _, _, _} ->
      ok;
    #media_info{} = MI ->
      {Mpegts1, Data} = mpegts:encode(Mpegts, MI),
      tcp_send(Req, Data),
      ?MODULE:write_loop(Req, Mpegts1, Started);
    refresh_auth ->
      % TODO add rechecking session info
      ?MODULE:write_loop(Req, Mpegts, Started);
    Message ->
      ?D(Message)
  after
    1000 ->
      null_packet(Req),
      ?MODULE:write_loop(Req, Mpegts, Started)
  end.
  
handle_frame(#video_frame{} = Frame, Req, Mpegts, Started) ->
  case Frame of
    #video_frame{flavor = config} = F ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      {Mpegts1, Data} = mpegts:encode(Mpegts, F),
      tcp_send(Req, Data),
      ?MODULE:write_loop(Req, Mpegts1, Started);
    #video_frame{flavor = keyframe} = F when Started == false ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      {Mpegts1, Data} = mpegts:encode(Mpegts, F),
      tcp_send(Req, Data),
      ?MODULE:write_loop(Req, Mpegts1, true);
    #video_frame{} when Started == false ->
      ?MODULE:write_loop(Req, Mpegts, Started);    
    #video_frame{} = F ->
      % ?D({F#video_frame.flavor, F#video_frame.codec, round(F#video_frame.dts)}),
      case mpegts:encode(Mpegts, F) of
        {Mpegts1, <<>>} ->
          ?MODULE:write_loop(Req, Mpegts1, Started);
        {Mpegts1, Data} ->
          tcp_send(Req, Data),
          ?MODULE:write_loop(Req, Mpegts1, Started)
      end
  end.


null_packet(Req) ->
  case get(null_counter) of undefined -> put(null_counter, 0); _ -> ok end,
  Counter = (get(null_counter) + 1) rem 16,
  % Null header size is 4 bytes
  % Adaptation header is 1 byte
  % Adaptaion length is 1 byte
  % Whole size is 188 bytes
  % Padding is 188 - 4 - 1 -1 = 182 bytes
  Null = mpegts:null(Counter),
  tcp_send(Req, Null),
  ok.


tcp_send(Req, Bin) ->
  [Transport, Socket] = cowboy_req:get([transport, socket], Req),
  case Transport:send(Socket, Bin) of
    ok -> ok;
    {error, closed} -> exit(normal);
    {error, timeout} -> exit(timeout);
    {error, Error} -> exit(Error)
  end.


