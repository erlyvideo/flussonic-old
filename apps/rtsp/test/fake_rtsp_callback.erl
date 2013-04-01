-module(fake_rtsp_callback).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").





describe("rtsp://localhost:8854/stream1", _Headers, _) ->
  {ok, flu_test:media_info()}.

play("rtsp://localhost:8854/stream1", _Headers, _Body) ->
  [self() ! F || F <- flu_test:gop(1)],
  [self() ! F || F <- flu_test:gop(2)],
  [self() ! F || F <- flu_test:gop(3)],
  {ok, {stream, self()}}.



announce(<<"rtsp://localhost:1554/mystream.sdp">>, _Headers, _Body) ->
  Self = self(),
  Pid = spawn(fun() -> erlang:monitor(process, Self), fake_media() end),
  {ok, Pid}.


fake_media() ->
  receive
    {'DOWN',_,_,_,_} -> ok;
    M -> ?debugFmt("zz ~p",[M]), ?MODULE:fake_media()
  end.
