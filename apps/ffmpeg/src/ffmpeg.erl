-module(ffmpeg).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-export([start_worker/0, start_worker/1, send/2, fetch/1, fetch/2, close/1]).
-export([init_decoder/2, send_frame/2]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


start_worker() ->
  start_worker(filename:join(code:lib_dir(ffmpeg, priv), "ffmpeg")).

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
  case fetch(Worker) of
    ok -> ok;
    #video_frame{} = Reply -> Reply;
    Else -> Else
  end.



init_decoder(Port, #media_info{streams = Streams}) ->
  [begin
    send(Port, {init_input, Content, Codec, Config, TrackId}),
    case fetch(Port) of
      ready -> ok;
      Else -> error({init_input,Codec,TrackId,Else})
    end
  end || #stream_info{content = Content, codec = Codec, config = Config, track_id = TrackId} <- Streams],
  ok.

fetch(Port) ->
  fetch(Port, 2000).

fetch({socket, Sock}, Timeout) ->
  case gen_tcp:recv(Sock, 0, Timeout) of
    {ok, Data} -> erlang:binary_to_term(Data);
    {error, closed} -> closed;
    {error, _} = Error -> Error
  end;

fetch({program, Port}, Timeout) ->
  receive
    {Port, {exit_status, 0}} -> closed;
    {Port, {exit_status, Code}} -> {exit, Code};
    {Port, {data, Data}} -> erlang:binary_to_term(Data);
    {Port, Data} -> {ok, Data}
  after
    Timeout -> {error, timeout}
  end.


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

