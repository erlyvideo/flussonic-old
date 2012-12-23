-module(rtmp_test_client).
-include("../include/rtmp.hrl").

-export([create_client/2, init/1, handle_info/2, terminate/2]).

create_client(Socket, Options) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Socket, Options], []),
  unlink(Pid),
  {ok, Pid}.

init([Socket, Options]) ->
  erlang:monitor(process, Socket),
  {ok, {Socket, Options}}.


handle_info({rtmp, Socket, Message}, State) ->
  case Message of
    #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"test_call">>, args = [null, Num]} = AMF} ->
      rtmp_lib:reply(Socket, AMF#rtmp_funcall{args = [Num + 1]});
    _ ->
      ok
  end,
  {noreply, State};

handle_info({rtmp, _Socket, disconnect, _}, State) ->
  {stop, normal, State};

handle_info({'DOWN', _,_,_,_}, State) ->
  {stop, normal, State}.


terminate(_,_) ->
  ok.
