%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        file frame source. This must be used only for streams, not for delivering file to clients
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
-module(udp_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, handle_info/2, terminate/2, update_options/2]).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").

-record(udp, {
  options,
  socket,
  host,
  port,
  name,
  media_info,
  mpegts
}).

-define(PACKET_COUNT, 7).
-define(CHUNK_SIZE, 7*188).

init(Options) ->
  Name = proplists:get_value(name, Options),
  {ok, UDP} = update_options(Options, #udp{name = Name}),
  {ok, UDP}.


update_options(Options, #udp{socket = undefined, name = Name, media_info = MI} = UDP) ->
  Mpegts0 = mpegts:init(),
  URL = proplists:get_value(udp, Options),
  {udp, _, Host1, Port, _, _} = http_uri2:parse(URL),
  {ok, Host} = inet_parse:address(Host1),
  {ok, Socket} = gen_udp:open(0, [{broadcast,true},{reuseaddr,true},{sndbuf,1024*1024}]),
  ?DBG("UDP packetizer for stream \"~s\" to url \"~s\"", [Name, URL]),
  Mpegts = case MI of
    undefined -> Mpegts0;
    _ -> 
      {Mpegts0_, Data} = mpegts:encode(Mpegts0, MI),
      gen_udp:send(Socket, Host, Port, Data),
      Mpegts0_
  end,
  {ok, UDP#udp{options = Options, mpegts = Mpegts, host = Host, port = Port, socket = Socket}};

update_options(Options, #udp{socket = Socket} = UDP) ->
  gen_udp:close(Socket),
  update_options(Options, UDP#udp{socket = undefined}).

handle_info(#video_frame{} = Frame, #udp{mpegts = Mpegts, socket = Socket, host = Host, port = Port} = State) ->
  {Mpegts1, Data1} = mpegts:encode(Mpegts, Frame),
  Data = iolist_to_binary(Data1),
  case Data of
    <<>> ->
      {noreply, State#udp{mpegts = Mpegts1}};
    _ ->  
      Chunks = split_chunks(Data),
      [gen_udp:send(Socket, Host, Port, Chunk) || Chunk <- Chunks],
      {noreply, State#udp{mpegts = Mpegts1}}
  end;

handle_info(#media_info{} = MI, #udp{} = UDP) ->
  {noreply, UDP#udp{media_info = MI}};

handle_info(_Frame, State) ->
  {noreply, State}.

terminate(_,#udp{socket = Socket}) ->
  gen_udp:close(Socket),
  ok.

split_chunks(Data) ->
  PacketNumber = size(Data) div 188,
  GoodChunkCount = PacketNumber div ?PACKET_COUNT,
  ChunkSize = ?CHUNK_SIZE,
  Chunks = [Chunk || <<Chunk:ChunkSize/binary>> <= Data],
  length(Chunks) == GoodChunkCount orelse erlang:error({invalid_packets,length(Chunks),GoodChunkCount}),
  Offset = GoodChunkCount*?CHUNK_SIZE,
  if Offset == size(Data) -> Chunks;
  true ->  
    <<_:Offset/binary, LastChunk/binary>> = Data,
    Chunks ++ [LastChunk]
  end.
