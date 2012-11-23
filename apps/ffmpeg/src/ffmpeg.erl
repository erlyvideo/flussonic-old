%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        multibitrate packetizer
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ffmpeg).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").

-type(output_option() :: 
  {width,Width::non_neg_integer()}
  |{height,Height::non_neg_integer()}
  |{bitrate,Bitrate::non_neg_integer()}
  |{FFkey::string(),FFvalue::string()}
  ).

-record(init_output, {
  content = undefined :: frame_content(),
  codec = undefined :: frame_codec(),
  track_id = undefined :: non_neg_integer(),
  options = [] :: [output_option()]
}).

-record(init_input, {
  content = undefined :: frame_content(),
  codec = undefined :: frame_codec(),
  config = <<>> :: binary()
}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-export([start_worker/0, start_worker/1, send/2, fetch/1, fetch/2, close/1]).
-export([init_decoder/2, send_frame/2]).
-export([init_encoder/2]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


start_worker() ->
  start_worker(filename:join(code:lib_dir(ffmpeg, priv), "flussonic_ffmpeg")).

start_worker(Path) ->
  case string:tokens(Path, ":") of
    [Host, Port] ->
      {ok, Socket} = gen_tcp:connect(Host, list_to_integer(Port), [binary,{packet,4},{send_timeout,3000},{active,false}]),
      {socket, Socket};
    _ ->
      Port = erlang:open_port({spawn_executable, Path}, [{packet,4},{arg0, "flussonic_ffmpeg"},binary,exit_status]),
      {program, Port}
  end.


close({socket, Sock}) ->
  gen_tcp:close(Sock);

close({program, Port}) ->
  erlang:port_close(Port).


send({socket, Sock}, Term) ->
  gen_tcp:send(Sock, erlang:term_to_binary(Term));

send({program, Port}, Term) ->
  erlang:port_command(Port, erlang:term_to_binary(Term)),
  ok.


send_frame(_Worker, #video_frame{flavor = config}) ->
  {error, config_forbidden};

% send_frame(Worker, #video_frame{codec = h264, flavor = Flavor, body = Body} = Frame) ->
%   send(Worker, Frame)

send_frame(Worker, #video_frame{} = Frame) ->
  send(Worker, Frame),
  Reply = fetch(Worker),
  Reply.



init_decoder(Port, #media_info{streams = Streams}) ->
  [begin
    send(Port, #init_input{content = Content, codec = Codec, config = Config}),
    case fetch(Port) of
      ready -> ok;
      Else -> error({init_input,Codec,TrackId,Else})
    end
  end || #stream_info{content = Content, codec = Codec, config = Config, track_id = TrackId} <- Streams],
  ok.


ev_to_av(h264) -> libx264;
ev_to_av(aac) -> libfaac;
ev_to_av(Codec) -> Codec.

av_to_ev(libx264) -> h264;
av_to_ev(libfaac) -> aac;
av_to_ev(Codec) -> Codec.

init_encoder(Port, #media_info{streams = Streams}) ->
  Streams1 = lists:map(fun
    (#stream_info{content = video, params = #video_params{width = W, height = H}, codec = C, bitrate = B, options = O} = S) ->
      S#stream_info{options = [{"preset","fast"},{width,W},{height,H},{bitrate,B}|O], codec = ev_to_av(C)};
    (#stream_info{content = audio, codec = C, bitrate = B, options = O, params = #audio_params{sample_rate = SR, channels = Ch}} = S) ->
      S#stream_info{codec = ev_to_av(C), options = [{bitrate,B},{sample_rate,SR},{channels,Ch}|O]}
  end, Streams),
  Configs = lists:flatmap(fun(#stream_info{content = Content, codec = Codec, options = Options, track_id = TrackId}) ->
    % ?debugFmt("init_encoder: ~p", [{Content, Codec, TrackId, Options}]),
    send(Port, #init_output{content = Content, codec = Codec, track_id = TrackId, options = Options}),
    case fetch(Port) of
      ready -> [];
      #video_frame{} = F -> [F];
      Else -> error({init_output,Codec,TrackId,Else})
    end
  end, Streams1),
  {ok, Configs}.


fetch(Port) ->
  fetch(Port, 2000).

fetch({socket, Sock}, Timeout) ->
  case gen_tcp:recv(Sock, 0, Timeout) of
    {ok, Data} -> transform_reply(erlang:binary_to_term(Data));
    {error, closed} -> closed;
    {error, _} = Error -> Error
  end;

fetch({program, Port}, Timeout) ->
  receive
    {Port, {exit_status, 0}} -> closed;
    {Port, {exit_status, Code}} -> {exit, Code};
    {Port, {data, Data}} -> transform_reply(erlang:binary_to_term(Data));
    {Port, Data} -> {ok, Data}
  after
    Timeout -> {error, timeout}
  end.


transform_reply(#video_frame{codec = Codec} = Frame) ->
  transform_frame(Frame#video_frame{codec = av_to_ev(Codec)});
transform_reply(A) ->
  A.


transform_frame(#video_frame{codec = h264, body = Body, flavor = config} = Reply) ->
  NALs = [NAL || NAL <- binary:split(Body, [<<1:32>>, <<1:24>>], [global]), size(NAL) > 0],
  H264 = lists:foldl(fun(NAL, H_) -> {H1_,_} = h264:decode_nal(NAL, H_), H1_ end, h264:init(), NALs),
  % ?debugFmt("<  config ~p", [Reply#video_frame.dts]),
  Reply#video_frame{body = h264:decoder_config(H264)};
transform_frame(#video_frame{codec = h264, body = Annexb} = Reply) ->
  NALs = [NAL || NAL <- binary:split(Annexb, [<<1:32>>, <<1:24>>], [global]), size(NAL) > 0],
  Body = iolist_to_binary([[<<(size(NAL)):32>>, NAL] || NAL <- NALs]),
  % ?debugFmt("<~8s ~p ~p ~p", [Reply#video_frame.flavor, Reply#video_frame.dts, Reply#video_frame.pts,
  %   [{h264:type(NAL), size(NAL)} || NAL <- NALs]       ]),
  Reply#video_frame{body = Body};
transform_frame(#video_frame{} = Frame) ->
  Frame.


-record(ffmpeg, {
  port
}).

init([]) ->
  Port = start_worker(),
  self() ! init,
  {ok, #ffmpeg{port = Port}}.


handle_info(init, #ffmpeg{port = _Port} = FFmpeg) ->
  {noreply, FFmpeg};

handle_info(Info, State) ->
  {stop, {error, {unknown_info, Info}}, State}.


handle_call(Call, _From, State) ->
  {stop, {error, {unknown_call, Call}}, State}.


terminate(_,_) ->
  ok.

