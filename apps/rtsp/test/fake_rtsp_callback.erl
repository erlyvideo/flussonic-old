-module(fake_rtsp_callback).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").



h264_aac_frames() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  Configs = video_frame:config_frames(mp4_reader:media_info(R)),
  Frames = Configs ++ read_frames(R, 1),
  file:close(F),
  Frames.

read_frames(R, N) ->
  case mp4_reader:read_gop(R, N) of
    {ok, Gop} ->
      Gop ++ read_frames(R, N+1);
    {error, _} ->
      []
  end.

h264_aac_media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  MI = mp4_reader:media_info(R),
  file:close(F),
  MI.



describe("rtsp://localhost:8854/stream1", _Headers, _) ->
  {ok, h264_aac_media_info()}.

play("rtsp://localhost:8854/stream1", _Headers, _Body) ->
  [self() ! F || F <- h264_aac_frames()],
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
