-module(websocket_client).

-behaviour(gen_server).

%% API
-export([start_link/3,write/2,close/1]).
-export([call/2, read/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([onmessage/2, onopen/1, onclose/1]).

%% Ready States

%% Behaviour definition
-export([behaviour_info/1]).

behaviour_info(callbacks) ->  [{onmessage,2},{onopen,1},{onclose,1}];
behaviour_info(_) -> undefined.


-type readystate() :: initial | headers | connecting | open | closed.

-record(state, {
  socket :: gen_tcp:socket(),
  readystate :: readystate(),
  url :: string(),
  headers = [],
  mod :: atom(),
  state :: any(),
  args :: any(),
  buffer = <<>> ::binary()
}).

onopen(Args) ->
  {ok, Args}.

onmessage({Type, Message}, Pid) when is_pid(Pid) ->
    Pid ! {Type, self(), Message},
    {ok, Pid};

onmessage({text, Message}, State) ->
  io:format("Text message: ~s~n", [Message]),
  {ok, State};


onmessage({binary, Message}, State) ->
  io:format("Bin message: ~p~n", [erlang:binary_to_term(Message)]),
  {ok, State}.

onclose(State) ->
  {ok, State}.


call(Socket, Msg) ->
    write(Socket, Msg),
    read(Socket).


read(Socket) ->
    receive
        {Type, Socket, Message} -> {Type, Message}
    after
        5000 -> erlang:exit(timeout)
    end.

start_link(URL, Mod, Args) ->
  gen_server:start_link(?MODULE, [URL, Mod,Args], []).

init([URL, Mod, Args]) when is_binary(URL) ->
  init([binary_to_list(URL), Mod, Args]);

init([URL, Mod, Args]) ->
  {ok, {http, _Auth, Host, Port, Path, _Query}} = http_uri:parse(URL),

  {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,once}]),
  
  ok = gen_tcp:send(Socket, initial_request(Host, Path)),
  inet:setopts(Socket, [{packet, http_bin},{active,once}]),
    
  {ok,#state{url = URL, socket=Socket,mod=Mod, state = undefined, args = Args}}.

%% Write to the server
write(Websocket, Data) when is_pid(Websocket) ->
  Websocket ! {send, Data}.

%% Close the socket
close(Websocket) when is_pid(Websocket) ->
  Websocket ! close.

handle_cast(_Cast, State) ->
  {noreply, State}.



handle_info({send, Msg}, #state{socket = Socket} = State) ->
  ok = gen_tcp:send(Socket, frame(Msg)),
  {noreply, State};

handle_info(close, State) ->
  State1 = close_callback(State),
  {stop,normal,State1#state{readystate=closed}};

%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,_Message}}, #state{socket = Socket} = State) ->
  inet:setopts(Socket, [{active,once}]),
  {noreply, State#state{readystate = headers}};

%% Extract the headers
handle_info({http,Socket,{http_header, _, 'Upgrade', _, <<"websocket">>}}, #state{socket = Socket, headers = Headers} = State) ->
  inet:setopts(Socket, [{active,once}]),
  {noreply, State#state{headers = [{'Upgrade',<<"WebSocket">>}|Headers], readystate = connecting}};


handle_info({http,Socket,{http_header, _, Name, _, Value}}, #state{socket = Socket, headers = Headers} = State) ->
  % io:format("~p~n", [{Name, Value, State#state.readystate}]),
  inet:setopts(Socket, [{active,once}]),
  {noreply, State#state{headers = [{Name,Value}|Headers]}};

handle_info({http, _Socket,http_eoh}, #state{readystate = headers} = State) ->
  {stop, no_websocket, State};

%% Once we have all the headers check for the 'Upgrade' flag 
handle_info({http,Socket,http_eoh}, #state{readystate = connecting, mod = Mod, args = Args} = State) ->
	inet:setopts(Socket, [{packet, raw},{active,once}]),
	{ok, S} = Mod:onopen(Args),
	{noreply, State#state{readystate = open, state = S}};

%% Handshake complete, handle packets
handle_info({tcp, Socket, Data}, #state{readystate = open, mod = Mod, state = S, buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active,once}]),
  
  case unframe(<<Buffer/binary, Data/binary>>) of
    {ok, {ping, _Message}, Rest} ->
      gen_tcp:send(Socket, [<<1:1, 0:3, 10:4, 0:1, 0:7>>]), % PONG
      handle_info({tcp, Socket, <<>>}, State#state{buffer = Rest});
    {ok, {pong, _Message}, Rest} ->
      handle_info({tcp, Socket, <<>>}, State#state{buffer = Rest});
    {ok, Message, Rest} ->
    	case Mod:onmessage(Message, S) of
    	  {ok, S1} ->
    	    handle_info({tcp, Socket, <<>>}, State#state{state = S1, buffer = Rest});
    	  {stop, Reason, S1} ->
    	    {stop, Reason, State#state{state = S1}}
    	end;
    {more, Rest} ->
      {noreply, State#state{buffer = Rest}}
  end;

handle_info({tcp_closed, _Socket}, #state{} = State) ->
  State1 = close_callback(State),
  {stop,normal,State1};

handle_info({tcp_error, _Socket, _Reason},State) ->
  {stop,tcp_error,State}.

handle_call(_Request,_From,State) ->
  {reply,{error, _Request}, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
close_callback(#state{mod = M, state = S} = State) ->
  case erlang:function_exported(M, onclose, 1) of
    true -> M:onclose(S);
    false -> ok
  end,
  State.
    

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_request(Host,Path) ->
  [
  "GET ", Path ," HTTP/1.1\r\n",
	"Host: ", Host, "\r\n",
	"Origin: http://", Host, "/\r\n",
  "Upgrade: Websocket\r\nConnection: Upgrade\r\n",
  "Connection: Upgrade\r\n",
  "Accept: application/x-bert\r\n",
  "Sec-Websocket-Version: 13\r\n",
  "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n",
	"\r\n"
	].

opcode_id(text) -> 1;
opcode_id(binary) -> 2;
opcode_id(ping) -> 9;
opcode_id(pong) -> 10.

frame({Opcode,Msg}) ->
  OpcodeId = opcode_id(Opcode),
  <<1:1, 0:3, OpcodeId:4, 0:1, (hybi_pack(Msg))/bits>>.

hybi_pack(Message) when byte_size(Message) =< 125 ->
  <<(byte_size(Message)):7, Message/bits>>;

hybi_pack(Message) when byte_size(Message) =< 65535 ->
  <<126:7, (byte_size(Message)):16, Message/bits>>;

hybi_pack(Message) when byte_size(Message) > 65535 ->
  <<127:7, (byte_size(Message)):64, Message/bits>>.



unframe(<<0, Data/binary>>) ->
  case binary:split(Data, <<255>>) of
    [Message, Rest] ->
      {ok, {text, Message}, Rest};
    [Rest] ->
      {more, Rest}
  end;

unframe(<<1:1, 0:3, Opcode:4, 0:1, Msg/bits>> = Input) ->
  case hybi_unpack(Msg) of
    {ok, Message, Rest} ->
      {ok, {opcode(Opcode), Message}, Rest};
    {more, _Rest} ->
      {more, Input}
  end;

unframe(Rest) ->
  {more, Rest}.


opcode(1) -> text;
opcode(2) -> binary;
opcode(8) -> close;
opcode(9) -> ping;
opcode(10) -> pong.

hybi_unpack(<<127:7, Len:64, Message:Len/binary, Rest/binary>>) ->
  {ok, Message, Rest};

hybi_unpack(<<126:7, Len:16, Message:Len/binary, Rest/binary>>) ->
  {ok, Message, Rest};

hybi_unpack(<<Len:7, Message:Len/binary, Rest/binary>>) ->
  {ok, Message, Rest};

hybi_unpack(Rest) ->
  {more, Rest}.

