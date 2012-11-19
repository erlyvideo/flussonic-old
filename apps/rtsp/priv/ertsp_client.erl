#!/usr/bin/env ERL_LIBS=..:../../deps escript

-mode(compile).

-define(D(X), io:format("DEBUG:~p ~p~n",[?LINE, X])).
-include_lib("erlmedia/include/video_frame.hrl").

main([URL]) ->
  RTSP = rtsp_reader:start_link(URL, [{consumer, self()}]),
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  application:set_env(lager,error_logger_redirect,false),
  lager:start(),
  read_frames(RTSP).
  
read_frames(RTSP) ->
  receive
    Frame when element(1, Frame) == video_frame ->
      Type = element(2, Frame),
      DTS = element(3, Frame),
      ?D({Type, DTS}),
      read_frames(RTSP);
    Else ->
      ?D({"Unknown", Else})
  end.