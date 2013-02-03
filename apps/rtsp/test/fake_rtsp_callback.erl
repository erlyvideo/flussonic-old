-module(fake_rtsp_callback).
-compile(export_all).



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

