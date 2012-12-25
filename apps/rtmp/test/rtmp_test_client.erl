-module(rtmp_test_client).
-include("../include/rtmp.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([create_client/2, init/1, handle_info/2, terminate/2]).

-record(state, {
  stream = 0,
  socket,
  options = []
}).

create_client(Socket, Options) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Socket, Options], []),
  unlink(Pid),
  {ok, Pid}.

init([Socket, Options]) ->
  erlang:monitor(process, Socket),
  {ok, #state{socket = Socket, options = Options}}.


handle_info({rtmp, Socket, Message}, #state{stream = S} = State) ->
  rtmp_socket:setopts(Socket, [{active,once}]),
  State1 = case Message of
    #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"connect">>, args = Args}} ->
      [{object, Info}|_] = Args,
      App = proplists:get_value(app, Info),
      case App of
        <<"rtmpapp">> -> 
          rtmp_lib:accept_connection(Socket),
          State;
        _ ->
          rtmp_lib:reject_connection(Socket),
          throw({stop, normal, State})
      end;
    #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"test_call">>, args = [null, Num]} = AMF} ->
      rtmp_lib:reply(Socket, AMF#rtmp_funcall{args = [Num + 1]}),
      State;
    #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"createStream">>} = AMF} ->
      rtmp_lib:reply(Socket, AMF#rtmp_funcall{args = [S + 1]}),
      State#state{stream = S + 1};
    #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"play">>, stream_id = StreamId, args = [null, Name|_]} = AMF} ->
      Type = case Name of
        <<"stream">> -> live;
        <<"file.mp4">> -> file;
        <<"rejected">> ->  rtmp_lib:fail(Socket, AMF#rtmp_funcall{args = [null, 403, <<"denied">>]}), throw({stop, normal, State});
        _ -> rtmp_lib:play_failed(Socket, StreamId), throw({stop, normal, State})
      end,
      rtmp_lib:play_start(Socket, StreamId, 0, Type),
      State;
    #rtmp_message{type = invoke, body = #rtmp_funcall{stream_id = StreamId, command = <<"publish">>, args = [null, Name|_]}} ->
      rtmp_lib:notify_publish_start(Socket, StreamId, 0, Name),
      State;
    _ ->
      ?debugFmt("rtmp ~240p", [Message]),
      State
  end,
  {noreply, State1};

handle_info({rtmp, _Socket, disconnect, _}, State) ->
  {stop, normal, State};

handle_info({'DOWN', _,_,_,_}, State) ->
  {stop, normal, State}.


terminate(_,_) ->
  ok.
