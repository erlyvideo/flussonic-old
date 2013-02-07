#!/usr/bin/env escript

main([]) ->
  code:add_pathz("../../deps/parsexml/ebin"),
  {ok, Bin} = file:read_file("priv/matroska.xml"),
  {<<"table">>, _, Elements} = parsexml:parse(Bin),
  Names = lists:map(fun({<<"element">>, Info, _}) ->
    <<"0x", Id/binary>> = proplists:get_value(<<"id">>, Info),
    Name = proplists:get_value(<<"name">>, Info),
    io_lib:format("id(16#~s) -> ~s;\n", [Id, underscore(Name)])
  end, Elements) ++ ["id(Id) -> integer_to_list(Id,16).\n\n"],

  Types = lists:map(fun({<<"element">>, Info, _}) ->
    Name = proplists:get_value(<<"name">>, Info),
    Type = proplists:get_value(<<"type">>, Info),
    io_lib:format("type(~s) -> ~s;\n", [underscore(Name), type(Type)])
  end, Elements) ++ ["type(_Id) -> undefined.\n\n"],

  file:write_file("src/mkv_types.erl", [
    "-module(mkv_types).\n"
    "-export([id/1,type/1]).\n\n",
    Names, Types
  ]),
  ok.


type(<<"utf-8">>) -> "utf8";
type(Type) -> Type.

underscore(Bin) when is_binary(Bin) ->
  underscore(binary_to_list(Bin));  

underscore(String) ->
  underscore(String, []).

underscore("CRC-32", _Acc) ->
  "crc_32";

underscore("EBML" ++ String, [$_|Acc]) when length(Acc) > 0 ->
  underscore(String, [$l,$m,$b,$e,$_|Acc]);

underscore("EBML" ++ String, Acc) when length(Acc) > 0 ->
  underscore(String, [$l,$m,$b,$e,$_|Acc]);

underscore([Char,$_,Char2|String], Acc) when 
  (Char >= $A andalso Char =< $Z) andalso (Char2 >= $A andalso Char2 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char2|String], [$_, string:to_lower(Char)|Acc]);


underscore([Char,Char1,Char2|String], Acc) when 
  not (Char >= $A andalso Char =< $Z) andalso (Char1 >= $A andalso Char1 =< $Z) andalso (Char2 >= $A andalso Char2 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char1,Char2|String], [$_, Char|Acc]);

underscore([Char,Char1|String], Acc) when 
  Char >= $A andalso Char =< $Z andalso not (Char1 >= $A andalso Char1 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char1|String], [string:to_lower(Char), $_|Acc]);

underscore([Char|String], Acc) when Char >= $A andalso Char =< $Z->
  underscore(String, [string:to_lower(Char)|Acc]);

underscore([Char|String], Acc) ->
  underscore(String, [Char|Acc]);

underscore([], Acc) ->
  lists:reverse(Acc).
