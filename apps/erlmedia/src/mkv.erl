-module(mkv).
-export([next_atom/1, type/1]).


next_atom(<<1:1, ID:7,  DataWithSize/binary>>) -> next_atom_size(DataWithSize, (1 bsl 7) bor ID);
next_atom(<<1:2, ID:14, DataWithSize/binary>>) -> next_atom_size(DataWithSize, (1 bsl 14) bor ID);
next_atom(<<1:3, ID:21, DataWithSize/binary>>) -> next_atom_size(DataWithSize, (1 bsl 21) bor ID);
next_atom(<<1:4, ID:28, DataWithSize/binary>>) -> next_atom_size(DataWithSize, (1 bsl 28) bor ID);
next_atom(_) -> more.

next_atom_size(<<1:1, Size:7,  Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:2, Size:14, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:3, Size:21, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:4, Size:28, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:5, Size:35, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:6, Size:42, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:7, Size:49, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(<<1:8, 16#FFFFFFFFFFFFFF:56, Rest/binary>>, ID) -> {ok, id(ID), stream, Rest};
next_atom_size(<<1:8, Size:56, Data:Size/binary, Rest/binary>>, ID) -> {ok, id(ID), Data, Rest};
next_atom_size(_,_) -> more.

id(Id) -> mkv_types:id(Id).
type(Id) -> mkv_types:type(Id).

