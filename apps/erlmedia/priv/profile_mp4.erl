#!/usr/bin/env ERL_LIBS=../../deps:.. escript

-mode(compile).
-include("../include/mp4.hrl").
-include("../include/video_frame.hrl").

main([Path]) ->
  main([Path, "mp4_new"]);

main([Path, Handler_|Args]) ->
  make:all([load]),
  application:load(lager),
  application:set_env(lager,handlers,[{lager_console_backend,info}]),
  application:set_env(lager,error_logger_redirect,true),
  application:set_env(lager,crash_log,undefined),
  lager:start(),
  Module = list_to_atom(Handler_),

  {ok, M} = mmap:open(Path, [binary]),

  Tracks = [1,2],

  {T, R} = timer:tc(fun() -> 
    {ok, R1} = Module:open({mmap, M}, []),
    % Module:keyframes(R1, [1,2]),
    % Module:keyframes(R1, [6,7]),
    R1#mp4_media{handler = Module}
  end),
  io:format("Time ~B ms, mem ~B MB~n", [T div 1000, size(term_to_binary(R)) div 1048576]),

  {ok, Manifest} = hds:file_manifest(mp4_reader, R),
  io:format("Manifest ~p bytes~n", [iolist_size(Manifest)]),

  % io:format("MI: ~240p~n", [mp4_reader:media_info(R)]),

  io:format("Keyframes length: ~240p~n", [length(video_frame:reduce_keyframes(Module:keyframes(R, Tracks)))]),

  N = case Args of
    [GopNum] -> list_to_integer(GopNum);
    [] -> length(video_frame:reduce_keyframes(Module:keyframes(R, Tracks))) * 5 div 6
  end,
  {T3, {ok, Gop}} = timer:tc(fun() -> Module:read_gop(R, N, Tracks) end),
  io:format("Gop number ~B, frames ~B: ~B us~n", [N, length(Gop), T3]),
  [io:format("~4.. s ~8.. s ~8.. B ~4.. B ~8.16.0B ~B~n", [Codec, Flavor, round(DTS), round(PTS - DTS), erlang:crc32(Body), size(Body)]) || 
  #video_frame{codec = Codec, flavor = Flavor, dts = DTS, pts = PTS, body = Body} <- Gop],
  ok.

