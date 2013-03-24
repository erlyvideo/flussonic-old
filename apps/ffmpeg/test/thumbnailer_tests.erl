-module(thumbnailer_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").



jpeg_test() ->
  flu_test:log(),
  {ok, Pid} = thumbnailer:start_link([]),
  ok = thumbnailer:media_info(Pid, flu_test:media_info()),
  Frame = hd([F || #video_frame{flavor = keyframe} = F <- flu_test:gop(5)]),
  {ok, _Jpeg} = thumbnailer:jpeg(Pid, Frame),
  file:write_file("a.jpg", _Jpeg),
  erlang:exit(Pid, normal),
  ok.

