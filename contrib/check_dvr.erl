#!/usr/bin/env ERL_LIBS=apps escript

-mode(compile).

-include_lib("erlmedia/include/video_frame.hrl").


main(Paths) ->
  
  Root = filename:join(filename:dirname(escript:script_name()), ".."),
  [code:add_pathz(Path_) || Path_ <- filelib:wildcard(Root ++ "/apps/*/ebin")],
  
  lists:foldl(fun(Path, StartDTS) ->
    dump_file(Path, StartDTS)
  end, undefined, Paths),
  ok.

dump_file(Path, StartDTS) ->
  io:format("--> ~s~n", [Path]),
  {ok, Frames} = mpegts_reader:read_file(Path),
  RealStartDTS = case StartDTS of
    undefined -> (hd(Frames))#video_frame.dts;
    _ -> StartDTS
  end,
  
  dump_frames(Frames, RealStartDTS).


dump_frames([], DTS) ->
  DTS;

dump_frames([#video_frame{dts = DTS, codec = Codec, flavor = Flavor}|Frames], PrevDTS) ->
  io:format("~4.. B ~4s ~s~n", [round(DTS - PrevDTS), Codec, Flavor]),
  dump_frames(Frames, DTS).
