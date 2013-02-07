-module(mkv_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").

-compile(export_all).

read_atom_test5() ->
  {ok, Bin} = file:read_file("../../../priv/bunny.webm"),
  read_atoms(Bin, 0).

read_atoms(<<>>, _) ->
  ok;

read_atoms(Bin, Level) ->
  case mkv:next_atom(Bin) of
    {ok, Id, Tag, Rest} ->
      case mkv:type(Id) of
        master ->
          ?debugFmt([$~] ++ integer_to_list(Level*2) ++ "s~p", ["",Id]),
          read_atoms(Tag,Level+1),
          read_atoms(Rest,Level);
        uinteger ->
          ?debugFmt([$~] ++ integer_to_list(Level*2) ++ "s~p uint:~p", ["",Id, Tag]),
          read_atoms(Rest,Level);
        string ->
          ?debugFmt([$~] ++ integer_to_list(Level*2) ++ "s~p string:~p", ["",Id, Tag]),
          read_atoms(Rest,Level);
        Type ->
          ?debugFmt([$~] ++ integer_to_list(Level*2) ++ "s~p: ~s ~p", ["",Id, Type, size(Tag)])
      end,
      read_atoms(Rest,Level)
  end.


atom_with_size_test() ->
  ?assertEqual({ok, segment, stream, <<17,77,155>>}, mkv:next_atom(<<24,83,128,103,1,255,255,255,255,255,255,255,17,77,155>>)).
