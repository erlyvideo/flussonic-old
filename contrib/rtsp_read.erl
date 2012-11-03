#!/usr/bin/env ERL_LIBS=apps:deps escript


-mode(compile).
-include_lib("erlmedia/include/video_frame.hrl").


main([URL]) ->
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  lager:start(),
  {ok, Reader} = rtsp_reader:start_link(URL, [{consumer,self()}]),
  erlang:monitor(process, Reader),
  loop().


loop() ->
  receive
    #video_frame{codec = Codec, flavor = Flavor, dts = DTS} ->
      io:format("~.4s ~.8s ~B", [Codec, Flavor, round(DTS)]),
      loop();
    {'DOWN', _,_,_,_} ->
      ok
  end.
