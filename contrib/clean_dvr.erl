#!/usr/bin/env escript

-mode(compile).

list(Path) ->
  case file:list_dir(Path) of
    {ok, []} -> empty;
    {ok, Entries} -> lists:sort([list_to_binary(E) || E <- Entries]);
    {error, _} -> []
  end.

main([Path]) ->
  years(list_to_binary(Path), list(Path)).

years(_Path, []) -> ok;
years(Path, [Year|Years]) ->
  P = <<Path/binary, "/", Year/binary>>, 
  months(P, list(P)),
  years(Path, Years).

months(_, []) -> ok;
months(Path, [Month|Months]) ->
  P = <<Path/binary, "/", Month/binary>>,
  days(P, list(P)),
  months(Path, Months).

days(_Path, []) -> ok;
days(Path, [Day|Days]) ->
  P = <<Path/binary, "/", Day/binary>>,
  hours(P, list(P)),
  days(Path, Days).


hours(Path, empty) -> remove_r(Path);
hours(_Path, []) -> ok;
hours(Path, [Hour|Hours]) ->
  P = <<Path/binary, "/", Hour/binary>>,
  minutes(P, list(P)),
  hours(Path, Hours).

minutes(Path, empty) -> remove_r(Path);
minutes(_Path, []) -> ok;
minutes(Path, [Minute|Minutes]) ->
  P = <<Path/binary, "/", Minute/binary>>,
  seconds(P, list(P)),
  minutes(Path, Minutes).

seconds(Path, empty) -> remove_r(Path);
seconds(_Path, []) -> ok;
seconds(Path, [Second|Seconds]) ->
  case filename:extension(Second) of
    <<".ts">> ->
      io:format("delet ~s/~s~n", [Path, Second]),
      remove(<<Path/binary, "/", Second/binary>>);
    _ ->
      io:format(" skip ~s/~s~n", [Path, Second])
  end,
  seconds(Path, Seconds).

remove(Path) ->
  case file:delete(Path) of
    ok -> remove_r(filename:dirname(Path));
    _ -> ok
  end.

remove_r(Dir) ->
  case file:del_dir(Dir) of
    ok -> io:format("rmdir ~s~n", [Dir]), remove_r(filename:dirname(Dir));
    _ -> ok
  end.

