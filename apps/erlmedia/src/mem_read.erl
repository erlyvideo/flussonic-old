-module(mem_read).

-export([pread/3]).

pread(File, Position, Size) when Position >= 0 andalso Position < size(File) ->
  case File of
    <<_:Position/binary, Bin:Size/binary, _/binary>> -> {ok, Bin};
    <<_:Position/binary, Bin/binary>> -> {ok, Bin}
  end;

pread(_File, Position, Size) when Position < 0 orelse Size < 0 ->
  {error,einval};

pread(File, Position, _Size) when Position >= size(File) ->
  eof.
