#!/usr/bin/env ERL_LIBS=apps escript

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/aac.hrl").

-record(dumper, {
  f,
  id,
  samples,
  sample_rate,
  aac_count
}).

main([Path]) ->
  {ok, F} = file:open(Path, [binary, read]),
  Offset = flv:data_offset(),
  dump(#dumper{f = F, id = Offset}).
  
dump(#dumper{f = F, id = Id, aac_count = ACount, samples = Samples, sample_rate = Rate} = D) ->
  try flv:read_frame({file, F}, Id) of
    #video_frame{flavor = config, codec = aac, body = Body, next_id = Next} = Frame ->
      dump_frame(Frame),
      #aac_config{samples_per_frame = Samples1, sample_rate = Rate1} = aac:decode_config(Body),
      dump(D#dumper{id = Next, samples = Samples1, sample_rate = Rate1, aac_count = 1});
    #video_frame{next_id = Next, codec = aac} = Frame ->
      dump_frame(Frame#video_frame{pts = ACount * Samples * 1000 / Rate}),
      dump(D#dumper{id = Next, aac_count = ACount+ 1});
    #video_frame{next_id = Next} = Frame ->
      dump_frame(Frame),
      dump(D#dumper{id = Next});
    eof ->
      ok
  catch
    Class:Error ->
      {ok, Size} = file:position(F, eof),
      {ok, Bin} = file:pread(F, Id, Size - Id),
      io:format("ERROR ~p:~p ~p~n~p~n", [Class,Error, erlang:get_stacktrace(), Bin])    
  end.

limit(Body, Limit) ->
  case Body of
    <<Data:Limit/binary, _/binary>> -> Data;
    _ -> Body
  end.

dump_frame(#video_frame{content = video}) -> ok;

dump_frame(#video_frame{content = Content, stream_id = StreamId, codec = Codec, flavor = Flavor, dts = DTS, pts = PTS, body = Body, sound = Sound}) ->
  Info = case Codec of
    _ when Flavor == config -> io_lib:format("~240p", [Body]);
    _ when Content == video -> io_lib:format("~p ctime, ~240p bytes: ~240p", [round(PTS - DTS), size(Body), limit(Body, 20)]);
    _ when Content == audio -> io_lib:format("~p delta", [round(PTS - DTS)]);
    _ when Content == metadata -> io_lib:format("metadata", [])
  end,
    
  io:format("~8.s ~8.B ~5.s ~9.s ~15.B ~s~n", [Content, StreamId, Codec, Flavor, round(DTS), Info]).

