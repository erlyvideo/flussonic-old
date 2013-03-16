#!/usr/bin/env ERL_LIBS=apps:deps escript


-mode(compile).
-include_lib("erlmedia/include/video_frame.hrl").

main([]) ->
  io:format("~s rtsp://host/path~n", [escript:script_name()]),
  erlang:halt(1);

main([URL]) ->
  application:load(lager),
  application:set_env(lager,crash_log,undefined),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  lager:start(),
  {ok, Reader} = rtsp_reader:start_link(URL, [{consumer,self()}]),
  erlang:monitor(process, Reader),
  loop().


loop() ->
  receive
    #video_frame{codec = Codec, flavor = Flavor, dts = DTS} ->
      io:format("~.4s ~.8s ~.1f~n", [Codec, Flavor, DTS]),
      loop();
    {'$gen_call', From, #video_frame{codec = Codec, flavor = Flavor, dts = DTS}} ->
      gen_server:reply(From, ok),
      io:format("~.4s ~.8s ~.1f~n", [Codec, Flavor, DTS]),
      loop();
    #gop{opened_at = Opened, duration = D, frames = Bin} ->
      {ok, Frames} = mpegts_decoder:decode_file(Bin),
      io:format("Push gop ~p, ~p: ~B frames\n", [Opened, D, length(Frames)]),
      loop();
    Else ->
      io:format("~p~n",[Else]),
      ok
    % {'DOWN', _,_,_,_} ->
    %   ok
  end.
