%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP socket module.
%%% Designed to look like rtmp mode of usual TCP socket. If you have used {packet, http}, you will
%%% find many common behaviours.
%%% 
%%% When working on server side, you should accept Socket::port() with:
%%% <pre><code>
%%% {ok, RTMP} = rtmp_socket:accept(Socket).
%%% receive
%%%   {rtmp, RTMP, connected} ->
%%%     rtmp_socket:setopts(RTMP, [{active, once}]),
%%%     loop(RTMP)
%%% end
%%% loop(RTMP) ->
%%%   receive
%%%     {rtmp, RTMP, disconnect, Statistics} -> 
%%%       ok;
%%%     {rtmp, RTMP, #rtmp_message{} = Message} ->
%%%       io:format("Message: ~p~n", [Message]),
%%%       loop(RTMP)
%%%   end.
%%% </code></pre>
%%%
%%% You are strongly advised to use {active, once} mode, because in any other case it is very easy
%%% to crash whole your server with OOM killer.
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_socket).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/4, init_server/4]).
-export([connect/1, connect/2, start_client/2, init_client/2]).

-export([getopts/2, setopts/2, getstat/2, getstat/1, send/2, get_socket/1]).
-export([status/3, status/4, prepare_status/2, prepare_status/3, invoke/2, invoke/4, prepare_invoke/3, notify/4, prepare_notify/3]).
-export([notify_audio/3, notify_video/3]).

-export([start_server/3, start_server/4, stop_server/1]).
-export([close/1]).
  

-export([set_options/2]).


-export([handle_call/3, handle_info/2, terminate/2]).


%% @spec (Port::integer(), Name::atom(), Callback::atom()) -> {ok, Pid::pid()}
%% @doc Starts RTMP listener on port Port, registered under name Name with callback module Callback.
%% Callback must export one function: create_client/1
%% create_client(RTMPSocket::pid()) -> {ok, Pid::pid()}
%%
%% This function receives RTMPSocket, that is ready to send messages and after this callback function returns, this socket
%% will send rtmp_message as it is defined in overview.
%% @end
-spec(start_server(Port::integer(), Name::atom(), Callback::atom()) -> {ok, Pid::pid()}).
start_server(Port, Name, Callback) ->
  start_server(Port, Name, Callback, []).

start_server(Port, Name, Callback, Args) ->
  application:start(ranch),
  Spec = ranch:child_spec(Name, 10, ranch_tcp, [{port, Port}], rtmp_socket, [Callback, Args]),
  case supervisor:start_child(rtmp_sup, Spec) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid}
  end.




stop_server(Name) ->
  ranch:stop_listener(Name).


start_link(ListenerPid, Socket, _Transport, [Callback, Args]) ->
  % {ok, RTMP} = rtmp_sup:start_rtmp_socket(accept),
  proc_lib:start_link(?MODULE, init_server, [ListenerPid, Socket, Callback, Args]).


init_server(ListenerPid, Socket, Callback, Args) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),

  {ok, {IP, Port}} = inet:peername(Socket),
  ok = inet:setopts(Socket, [{packet, raw}, binary, {send_timeout, 10000}]),

  {ok, Handshake} = case gen_tcp:recv(Socket, ?HS_BODY_LEN + 1, 3000) of
    {ok, Bin1} when size(Bin1) == ?HS_BODY_LEN + 1 -> {ok, Bin1};
    {error, timeout} -> exit(handshake_timeout)
  end,

  State1 = case rtmp_handshake:server(Handshake) of
    {uncrypted, Reply} ->
      case gen_tcp:send(Socket, Reply) of
        ok -> ok;
        {error, timeout} -> exit(handshake_timeout)
      end,
      #rtmp_socket{};
    {crypted, Reply, KeyIn, KeyOut} ->
      case gen_tcp:send(Socket, Reply) of
        ok -> ok;
        {error, timeout} -> exit(handshake_timeout)
      end,
      #rtmp_socket{key_in = KeyIn, key_out = KeyOut}
  end,

  {ok, Bin2} = gen_tcp:recv(Socket, ?HS_BODY_LEN),
  size(Bin2) == ?HS_BODY_LEN orelse exit(too_short_handshake),


  {ok, Pid} = erlang:apply(Callback, create_client, [self()|Args]),
  erlang:monitor(process,Pid),

  State2 = State1#rtmp_socket{socket = Socket, address = IP, port = Port, 
      channels = {}, out_channels = {}, consumer = Pid, active = false},

  inet:setopts(Socket, [{active,once}]),
  gen_server:enter_loop(rtmp_socket, [], State2, ?RTMP_TIMEOUT).





%% @spec (Socket::port()) -> {ok, RTMP::pid()}
%% @doc Accepts client connection on socket Socket, starts RTMP decoder, passes socket to it
%% and returns pid of newly created RTMP socket.
%% @end
-spec(connect(Socket::port()|string()) -> {ok, RTMPSocket::pid()}).
connect(ServerSpec) ->
  connect(ServerSpec, [{timeout, 10000}]).

-spec(connect(Socket::port()|string(), Options::list()) -> {ok, RTMPSocket::pid()}).
connect(ServerSpec, Options) when is_binary(ServerSpec) ->
  connect(binary_to_list(ServerSpec), Options);

connect(ServerSpec, Options) when is_list(ServerSpec) ->
  rtmp_sup:start_rtmp_client(ServerSpec, Options ++ [{consumer,self()}]).



%% @private
start_client(ServerSpec, Options) ->
  proc_lib:start_link(?MODULE, init_client, [ServerSpec, Options]).
  
  
%% @private  
init_client(ServerSpec, Options) ->
  try init_client0(ServerSpec, Options)
  catch
    throw:{error, _} = Error -> proc_lib:init_ack(Error);
    exit:normal -> ok;
    Class:Error -> proc_lib:init_ack({error, {Class, Error, erlang:get_stacktrace()}})
  end.

init_client0(ServerSpec, Options) ->
  {_, Consumer} = lists:keyfind(consumer, 1, Options),
  erlang:monitor(process, Consumer),
  {Host, Port} = case re:run(ServerSpec, "rtmp://([^:]+):(\\d+)/", [{capture,all_but_first,list}]) of
    {match, [Host_, Port_]} ->
      {Host_, list_to_integer(Port_)};
    nomatch ->
      {match, [Host_]} = re:run(ServerSpec, "rtmp://([^/]+)/", [{capture,all_but_first,list}]),
      {Host_, 1935}
  end,
  Timeout = proplists:get_value(timeout, Options, 10000),
  {ok, Socket} = case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}], Timeout) of
    {ok, Sock} -> {ok, Sock};
    {error, _} = Err -> throw(Err)
  end,
  ok = inet:setopts(Socket, [{send_timeout,Timeout}]),

  ok = gen_tcp:send(Socket, rtmp_handshake:c1()),
  {ok, <<?HS_UNCRYPTED, S1:?HS_BODY_LEN/binary, _S2:?HS_BODY_LEN/binary>>} = gen_tcp:recv(Socket, ?HS_BODY_LEN*2 + 1, 5000),
  ok = gen_tcp:send(Socket, rtmp_handshake:c2(S1)),

  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  State1 = #rtmp_socket{socket = Socket, address = IP, port = Port, consumer = Consumer, channels = {}, out_channels = {}, active = false},
  proc_lib:init_ack({ok, self()}),
  gen_server:enter_loop(?MODULE, [], State1, ?RTMP_TIMEOUT).













get_socket(RTMP) -> 
  gen_server:call(RTMP, get_socket).

notify_audio(RTMP, StreamId, DTS) ->
  gen_server:call(RTMP, {notify_audio, StreamId, DTS}).

notify_video(RTMP, StreamId, DTS) ->
  gen_server:call(RTMP, {notify_video, StreamId, DTS}).

close(Socket) ->
  try gen_server:call(Socket, stop)
  catch
    exit:_ -> ok
  end.

  

%% @spec (RTMP::pid(), Options::[{Key, Value}]|{Key, Value}) -> ok
%% @doc Just the same as {@link inet:setopts/2. inet:setopts/2} this function changes state of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul><li><code>chunk_size</code> - change outgoing chunk size</li>
%%  <li><code>window_size</code> - ask remote client to send read acknowlegement after WindowSize bytes</li>
%%  <li><code>amf_version</code> - change AMF0 to AMF3, but only before first call</li>
%%  <li><code>consumer</code> - change messages consumer</li>
%%  <li><code>debug = true|false</code> - dump all important packets or no</li>
%% </ul>
%% @end
-spec(setopts(RTMP::rtmp_socket_pid(), Options::[{Key::atom(), Value::any()}]) -> ok).
setopts(RTMP, Options) ->
  try gen_server:call(RTMP, {setopts, Options})
  catch
    exit:{noproc,_} -> {error, noproc}
  end.

%% @spec (RTMP::pid(), Options::[{Key, Value}]|{Key, Value}) -> ok
%% @doc Just the same as {@link inet:getopts/2. inet:getopts/2} this function gets state of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul>
%%  <li><code>chunk_size</code> - get outgoing chunk size</li>
%%  <li><code>window_size</code> - get remote client read acknowlegement window size bytes</li>
%%  <li><code>amf_version</code> - get AMF0 or AMF3</li>
%%  <li><code>consumer</code> - get messages consumer</li>
%%  <li><code>address</code> - get remote client IP and port</li>
%% </ul>
%% @end
-spec(getopts(RTMP::rtmp_socket_pid(), Options::atom()|[Key::atom()]) -> any()).
getopts(RTMP, Options) ->
  gen_server:call(RTMP, {getopts, Options}).

%% @spec (RTMP::pid(), Stats::[Key]) -> Values::[{Key,Value}]
%% @doc Just the same as {@link inet:getstats/2. inet:getstats/2} this function gets statistics of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul>
%%  <li><code>recv_oct</code> - number of bytes received to the socket.</li>
%%  <li><code>send_oct</code> - number of bytes sent from the socket</li>
%% </ul>
%% @end
-spec(getstat(RTMP::rtmp_socket_pid(), Options::[Key::atom()]) -> ok).
getstat(RTMP, Options) ->
  gen_server:call(RTMP, {getstat, Options}).

%% @spec (RTMP::pid()) -> Values::[{Key,Value}]
%% @doc Just the same as {@link inet:getstats/1. inet:getstats/1} this function gets statistics of 
%% rtmp socket.<br/>
-spec(getstat(RTMP::rtmp_socket_pid()) -> ok).
getstat(RTMP) ->
  gen_server:call(RTMP, getstat).

  
%% @spec (RTMP::pid(), Message::rtmp_message()) -> ok
%% @doc Sends message to client.
%% @end
-spec(send(RTMP::rtmp_socket_pid(), Message::rtmp_message()) -> ok).
send(RTMP, Message) ->
  % io:format("Message ~p ~p ~p~n", [Message#rtmp_message.type, Message#rtmp_message.timestamp, Message#rtmp_message.stream_id]),
  % case process_info(RTMP, message_queue_len) of
  %   {message_queue_len, Length} when Length < 20 -> RTMP ! Message;
  %   {message_queue_len, Length} -> gen_fsm:sync_send_event(RTMP, Message, ?RTMP_TIMEOUT);
  %   _ -> ok
  % end,
  RTMP ! Message,
  ok.
  


notify(RTMP, StreamId, Name, Args) ->
  send(RTMP, prepare_notify(StreamId, Name, Args)).

prepare_notify(StreamId, Name, Args) ->
  Arg = {object, lists:ukeymerge(1, [{level, <<"status">>}], lists:keysort(1, Args))},
  #rtmp_message{type = metadata, channel_id = rtmp_lib:channel_id(audio, StreamId), stream_id = StreamId, body = [Name, Arg], timestamp = same}.


prepare_status(StreamId, Code) when is_list(Code) ->
  prepare_status(StreamId, list_to_binary(Code), <<"-">>);

prepare_status(StreamId, Code) when is_binary(Code) ->
  prepare_status(StreamId, Code, <<"-">>).


-spec(prepare_status(StreamId::non_neg_integer(), Code::any_string(), Description::any_string()) -> rtmp_message()).
prepare_status(StreamId, Code, Description) ->
  Arg = {object, [
    {code, Code}, 
    {level, <<"status">>}, 
    {description, Description}
  ]},
  prepare_invoke(StreamId, onStatus, [Arg]).


status(RTMP, StreamId, Code, Description) ->
  send(RTMP, prepare_status(StreamId, Code, Description)).

-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::any_string()) -> ok).
status(RTMP, StreamId, Code) when is_list(Code) ->
  status(RTMP, StreamId, list_to_binary(Code), <<"-">>);

status(RTMP, StreamId, Code) when is_binary(Code) ->
  status(RTMP, StreamId, Code, <<"-">>).

  
prepare_invoke(StreamId, Command, Args) when is_integer(StreamId) ->
  AMF = #rtmp_funcall{
      command = Command,
        type = invoke,
        id = 0,
        stream_id = StreamId,
        args = [null | Args ]},
  #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}.


invoke(RTMP, StreamId, Command, Args) ->
  send(RTMP, prepare_invoke(StreamId, Command, Args)).

invoke(RTMP, #rtmp_funcall{stream_id = StreamId, id = undefined} = AMF) ->
  InvokeId = gen_server:call(RTMP, next_invoke_id),
  send(RTMP, #rtmp_message{stream_id = StreamId, type = invoke, body = AMF#rtmp_funcall{id = InvokeId}}),
  {ok, InvokeId};

invoke(RTMP, #rtmp_funcall{stream_id = StreamId, id = InvokeId} = AMF) ->
  send(RTMP, #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}),
  {ok, InvokeId}.









handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(next_invoke_id, _From, #rtmp_socket{out_invoke_id = Id} = Socket) ->
  {reply, Id, Socket#rtmp_socket{out_invoke_id = Id + 1}, ?RTMP_TIMEOUT};

handle_call({getopts, Options}, _From, State) ->
  {reply, get_options(State, Options), State, ?RTMP_TIMEOUT};

handle_call({getstat, Options}, _From, State) ->
  {reply, get_stat(State, Options), State, ?RTMP_TIMEOUT};

handle_call(getstat, _From, State) ->
  {reply, get_stat(State), State, ?RTMP_TIMEOUT};

handle_call(get_socket, _From, #rtmp_socket{socket = Socket} = State) when is_port(Socket)->
  {reply, {rtmp,Socket}, State, ?RTMP_TIMEOUT};

handle_call({setopts, Options}, _From, State) ->
  NewState = set_options(State, Options),
  {reply, ok, NewState, ?RTMP_TIMEOUT};

handle_call({notify_audio, StreamId, DTS}, _From, Socket) ->
  {reply, ok, send_audio_notify(Socket, StreamId, DTS), ?RTMP_TIMEOUT};

handle_call({notify_video, StreamId, DTS}, _From, Socket) ->
  {reply, ok, send_video_notify(Socket, StreamId, DTS), ?RTMP_TIMEOUT};

handle_call(#rtmp_message{} = Message, _From, State) ->
  State1 = send_data(State, Message),
  {reply, ok, State1, ?RTMP_TIMEOUT};

handle_call(Call, _From, #rtmp_socket{} = Socket) ->
  {reply, {error, Call}, Socket, ?RTMP_TIMEOUT}.











handle_info({tcp, Socket, CryptedData}, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead, bytes_unack = BytesUnack, key_in = KeyIn} = State) ->
  State1 = flush_send(State),
  {NewKeyIn, Data} = case KeyIn of
    undefined -> {undefined, CryptedData};
    _ -> rtmpe:crypt(KeyIn, CryptedData)
  end,
  {noreply, handle_rtmp_data(State1#rtmp_socket{bytes_read = BytesRead + size(Data), bytes_unack = BytesUnack + size(Data), key_in = NewKeyIn, buffer = <<Buffer/binary, Data/binary>>}), ?RTMP_TIMEOUT};

handle_info({tcp_closed, Socket}, #rtmp_socket{socket = Socket, consumer = Consumer} = StateData) ->
  Consumer ! {rtmp, self(), disconnect, get_stat(StateData)},
  {stop, normal, StateData};

handle_info(#rtmp_message{} = Message, State) ->
  State1 = send_data(State, Message),
  State2 = flush_send(State1),
  {noreply, State2, ?RTMP_TIMEOUT};

handle_info({'DOWN', _, process, _Client, _Reason}, State) ->
  {stop, normal, State};

handle_info(timeout, #rtmp_socket{pinged = false} = State) ->
  State1 = send_data(State, #rtmp_message{type = ping}),
  {noreply, State1#rtmp_socket{pinged = true}, ?RTMP_TIMEOUT};
  
handle_info(timeout, #rtmp_socket{consumer = Consumer} = State) ->
  Consumer ! {rtmp, self(), timeout},
  {stop, normal, State};

handle_info(_Info, #rtmp_socket{} = Socket) ->
  {noreply, Socket, ?RTMP_TIMEOUT}.


terminate(_,_) ->
  ok.



  


-type(rtmp_option() ::active|amf_version|chunk_size|window_size|client_buffer|address).
% -type(rtmp_option_value() ::{rtmp_option(), any()}).
-spec(get_options(State::rtmp_socket(), Key::rtmp_option()|[rtmp_option()]) -> term()).

get_options(State, active) ->
  {active, State#rtmp_socket.active};

get_options(State, amf_version) ->
  {amf_version, State#rtmp_socket.amf_version};

get_options(State, chunk_size) ->
  {chunk_size, State#rtmp_socket.server_chunk_size};

get_options(State, window_size) ->
  {window_size, State#rtmp_socket.window_size};

get_options(State, url) ->
  {url, State#rtmp_socket.url};

get_options(State, client_buffer) ->
  {client_buffer, State#rtmp_socket.client_buffer};

get_options(State, debug) ->
  {debug, State#rtmp_socket.debug};

get_options(State, address) ->
  {address, {State#rtmp_socket.address, State#rtmp_socket.port}};

get_options(_State, []) ->
  [];
  
get_options(State, [Key | Options]) ->
  [get_options(State, Key) | get_options(State, Options)].

get_stat(State) ->
  get_stat(State, [recv_oct, send_oct]).

get_stat(State, recv_oct) ->
  {recv_oct, State#rtmp_socket.bytes_read};

get_stat(State, send_oct) ->
  {send_oct, State#rtmp_socket.bytes_sent};

get_stat(_State, []) ->
  [];

get_stat(State, [Key | Options]) ->
  [get_stat(State, Key) | get_stat(State, Options)].

set_options(State, [{amf_version, Version} | Options]) ->
  set_options(State#rtmp_socket{amf_version = Version}, Options);

set_options(#rtmp_socket{socket = Socket, buffer = Data} = State, [{active, Active} | Options]) ->
  State1 = flush_send(State#rtmp_socket{active = Active}),
  State2 = case Active of
    false -> 
      inet:setopts(Socket, [{active, false}]),
      State1;
    true ->
      inet:setopts(Socket, [{active, true}]),
      State1;
    once when size(Data) > 0 ->
      handle_rtmp_data(State1);
    once ->
      activate_socket(State),
      State1
  end,
  set_options(State2, Options);

set_options(#rtmp_socket{} = State, [{debug, Debug} | Options]) ->
  io:format("Set debug to ~p~n", [Debug]),
  set_options(State#rtmp_socket{debug = Debug}, Options);

set_options(#rtmp_socket{} = State, [{url, URL} | Options]) ->
  set_options(State#rtmp_socket{url = URL}, Options);

set_options(#rtmp_socket{consumer = undefined} = State, [{consumer, Consumer} | Options]) ->
  erlang:monitor(process, Consumer),
  set_options(State#rtmp_socket{consumer = Consumer}, Options);

set_options(State, [{chunk_size, ChunkSize} | Options]) ->
  State1 = send_data(State, #rtmp_message{type = chunk_size, body = ChunkSize}),
  set_options(State1#rtmp_socket{server_chunk_size = ChunkSize}, Options);

set_options(State, []) -> State.
  





% flush_send(State) -> flush_send([], State).

flush_send(State) ->
  receive
    #rtmp_message{} = Message ->
      NewState = send_data(State, Message),
      % ?D({out, Message}),
      % {NewState, Data} = rtmp:encode(State, Message),
      % flush_send([Data | Packet], NewState)
      flush_send(NewState)
  after
    0 -> State
  end.
  
flush_all() ->
  receive
    _Msg -> flush_all()
  after
    0 -> ok
  end.
      
  
activate_socket(#rtmp_socket{socket = Socket}) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}]);
activate_socket(#rtmp_socket{socket = Socket}) when is_pid(Socket) ->
  ok.


send_audio_notify(Socket, StreamId, DTS) ->
  send_data(Socket#rtmp_socket{sent_audio_notify = true}, rtmp_lib:empty_audio(StreamId, DTS)).

send_video_notify(Socket, StreamId, DTS) ->
  Msg = [
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<87,0>>, ts_type = new},
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<23,2,0,0,0>>, ts_type = new},
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<87,1>>, ts_type = delta}
  ],
  
  lists:foldl(fun(M, State1) ->
    send_data(State1, M)
  end, Socket#rtmp_socket{sent_video_notify = true}, Msg).
      

send_data(#rtmp_socket{sent_audio_notify = false} = Socket, 
          #rtmp_message{type = audio, timestamp = DTS, stream_id = StreamId, body = Body} = Message) when size(Body) > 0 ->
  State1 = send_audio_notify(Socket, StreamId, DTS),
  send_data(State1, Message#rtmp_message{ts_type = new});

send_data(#rtmp_socket{sent_video_notify = false} = Socket, #rtmp_message{type = video, timestamp = DTS, stream_id = StreamId} = Message) ->
  State2 = send_video_notify(Socket, StreamId, DTS),
  send_data(State2, Message);
  
send_data(#rtmp_socket{sent_audio_notify = true} = Socket, #rtmp_message{type = stream_end} = Message) ->
  send_data(Socket#rtmp_socket{sent_audio_notify = false}, Message);

send_data(#rtmp_socket{sent_video_notify = true} = Socket, #rtmp_message{type = stream_end} = Message) ->
  send_data(Socket#rtmp_socket{sent_video_notify = false}, Message);

send_data(#rtmp_socket{socket = Socket, key_out = KeyOut} = State, Message) ->
  case State#rtmp_socket.debug of
    true -> print_rtmp_message(out, Message);
    _ -> ok
  end,
  {NewState, Data} = case Message of
    #rtmp_message{} ->
      % ?D({Socket,Message#rtmp_message.type, Message#rtmp_message.timestamp, Message#rtmp_message.ts_type}),
      rtmp:encode(State, Message);
    _ -> 
      {State, Message}
  end,
  {NewState1, Crypt} = case KeyOut of
    undefined -> {NewState, Data};
    _ -> 
      {NewKeyOut, OutCrypt} = rtmpe:crypt(KeyOut, Data),
      {NewState#rtmp_socket{key_out = NewKeyOut}, OutCrypt}
  end,
  % (catch rtmp_stat_collector:out_bytes(self(), iolist_size(Crypt))),
  if
    is_port(Socket) ->
      case gen_tcp:send(Socket, Crypt) of
        ok -> ok;
        {error, timeout} -> flush_all(),
          lager:warning("RTMP client from ~p is exiting due to slow connection", [State#rtmp_socket.address]),
          throw({stop, normal, NewState1});
        {error, closed} -> throw({stop, normal, NewState1});
        {error, Else} -> flush_all(), throw({stop, {network_error,Else}, NewState1})
      end;
    is_pid(Socket) ->
      rtmpt:write(Socket, Crypt)
  end,
  NewState1.



handle_rtmp_data(#rtmp_socket{buffer = <<>>} = State) ->
  % ?D({empty_input, erlang:get_stacktrace()}),
  State;

handle_rtmp_data(#rtmp_socket{bytes_unack = Bytes, window_size = Window} = State) when Bytes >= Window ->
  State1 = send_data(State, #rtmp_message{type = ack_read}),
  handle_rtmp_data(State1);

handle_rtmp_data(#rtmp_socket{buffer = Data} = State) ->
  got_rtmp_message(rtmp:decode(State, Data)).


print_rtmp_message(InOut, #rtmp_message{channel_id = Channel, ts_type = TSType, timestamp = TS, type = Type, stream_id = StreamId, body = Body}) ->
  DecodedBody = case Type of
    video when size(Body) > 10 -> erlang:setelement(5, flv:decode_video_tag(Body), size(Body));
    audio when size(Body) > 0 -> erlang:setelement(7, flv:decode_audio_tag(Body), size(Body));
    _ -> Body
  end,
  io:format("~p ~p ~p ~p ~p ~p ~p~n", [InOut, Channel, TSType, TS, Type, StreamId, DecodedBody]),
  ok;

print_rtmp_message(InOut, Msg) ->
  io:format("~p ~p~n", [InOut, Msg]),
  ok.
  


got_rtmp_message({#rtmp_socket{debug = true}, Msg, _} = Message) ->
  print_rtmp_message(in, Msg),
  handle_rtmp_message(Message);

got_rtmp_message(Decoded) ->
  handle_rtmp_message(Decoded).


handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, #rtmp_message{type = window_size, body = Size} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{window_size = Size, buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer, pinged = true} = State, #rtmp_message{type = pong} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{pinged = false, buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, #rtmp_message{type = ping, body = Timestamp} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  send_data(State, #rtmp_message{type = pong, body = Timestamp}),
  rtmp_message_sent(State#rtmp_socket{buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{buffer = Rest});

handle_rtmp_message({#rtmp_socket{} = State, Rest}) -> 
  activate_socket(State),
  State#rtmp_socket{buffer = Rest}.

rtmp_message_sent(#rtmp_socket{active = true} = State) ->
  activate_socket(State),
  handle_rtmp_data(State);

rtmp_message_sent(#rtmp_socket{active = _} = State) ->
  handle_rtmp_data(State);

rtmp_message_sent(State) -> 
  State.



