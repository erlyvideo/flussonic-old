#!/usr/bin/env ERL_LIBS=/opt/flussonic/apps:/opt/flussonic/deps escript

-mode(compile).

main([Path, Out]) ->
  code:add_pathz("/opt/flussonic/apps/flussonic/ebin"),
  code:add_pathz("/opt/flussonic/deps/lager/ebin"),
  {ok, M} = file:open(Path,[binary,read,raw]),
  {ok, F} = file:open(Out, [write,binary,raw]),
  dump(M, 0, F),
  file:close(F).

dump(M, Pos, F) ->
  case M of
    <<_:Pos/binary, 0:32, Atom:4/binary, _/binary>> -> d(Atom,Pos,eof);
    <<_:Pos/binary, 1:32, Atom:4/binary, Length:64, _/binary>> ->
      d(Atom, Pos, Length - 16),
      write(Atom, F, M, Pos, Length),
      dump(M, Pos + Length, F);
    <<_:Pos/binary, Length:32, Atom:4/binary, _/binary>> ->
      d(Atom, Pos, Length - 8),
      write(Atom, F, M, Pos, Length),
      dump(M, Pos + Length, F);
    _ when size(M) =< Pos ->
      ok
  end.

d(Atom, Pos, Length) ->
  io:format("~16.. B ~4.. s ~p~n", [Pos, Atom, Length]).

write(<<"mdat">>, _, _, _, _) -> ok;
write(_, F, M, Pos, Length) ->
  <<_:Pos/binary, Bin:Length/binary, _/binary>> = M,
  file:write(F,Bin).
