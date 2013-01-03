#!/usr/bin/env ERL_LIBS=apps:deps escript

-mode(compile).

-include_lib("erlmedia/include/video_frame.hrl").


main([]) ->
  io:format("~s http://host/stream/hds/tracks-1,2/Seg1-Frag15~n", [escript:script_name()]),
  halt(1);

main([URL]) ->
  {ok, {_, 200, _, HDS}} = http_stream:request_body(URL, [{keepalive,false},{timeout,10000}]),
  Frames = flv:read_all_frames(HDS),
  [dump_frame(Frame) || Frame <- Frames],
  ok.

 
dump_frame(#video_frame{content = Content, stream_id = StreamId, codec = Codec, flavor = Flavor, dts = DTS, pts = PTS, body = Body}) ->
  Info = case Codec of
    _ when Flavor == config -> io_lib:format("~240p", [Body]);
    _ when Content == video -> io_lib:format("~p ctime, ~240p bytes: ~240p", [round(PTS - DTS), size(Body), limit(Body, 20)]);
    _ when Content == audio -> "";
    _ when Content == metadata -> io_lib:format("~240p", [Body])
  end,
    
  io:format("~8.s ~8.B ~5.s ~9.s ~15.B ~s~n", [Content, StreamId, Codec, Flavor, round(DTS), Info]).

limit(Body, Limit) ->
  case Body of
    <<Data:Limit/binary, _/binary>> -> Data;
    _ -> Body
  end.
