-module(fake_rtsp_server).
-compile(export_all).


start_link(ListenerPid, Socket, _Transport, _) ->
  {ok, RTSP} = proc_lib:start_link(?MODULE, init, [ListenerPid, Socket]),
  {ok, RTSP}.


-record(fake, {
  socket
}).

init(ListenerPid, Socket) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),

  gen_server:enter_loop(fake_rtsp, [], #fake{socket = Socket}).


