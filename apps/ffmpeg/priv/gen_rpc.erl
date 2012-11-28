#!/usr/bin/env escript

-mode(compile).


-record(gen, {
  module,
  enums = [],
  structs = [],
  aliases = [],
  headers = [
    "#include <ei.h>\n"
    "#include <stdint.h>\n"
    "#include <string.h>\n"
    "#include <stdlib.h>\n\n"
    "struct Binary{uint8_t *data; ssize_t size;};\n"
  ],
  parser = [
    "#include \"reader.h\"\n\n"
  ]
}).

main([Path]) ->
  {ok, Rows} = epp:parse_file(Path, [".."], []),

  #gen{headers = Headers, parser = Parser} = generate(Rows, #gen{}),


  % io:format("// HEADERS\n~s~n~n", [Headers]),
  file:write_file("c_src/reader.h", Headers),
  file:write_file("c_src/reader.c", Parser),
  % io:format("// PARSER\n~s~n~n", [Parser]),

  ok.


generate([{attribute,_,module, Module}|Rows], Gen) ->
  generate(Rows, Gen#gen{module = Module});

generate([{attribute,_,type,{Type,Desc,Opts}}|Rows], #gen{} = Gen) ->
  Opts == [] orelse error({dont_know_how_to_handle_opts,Type,Opts}),
  Gen1 = generate_type(Type, Desc, Gen),
  generate(Rows, Gen1);

generate([_|Rows], Gen) ->
  generate(Rows, Gen);

generate([], Gen) ->
  Gen.


% generate_type({record,Record}, Desc) ->

generate_type(Type, {type,_,union,[{atom,_,_}|_] = Variants}, #gen{enums = Enums, headers = Headers, parser = Parser} = Gen) ->
  Atoms = [Atom || {atom,_,Atom} <- Variants],
  T = atom_to_list(Type),
  Header = ["enum ", T, " {",
    string:join([io_lib:format("~s_~s = ~B", [Type,Atom,N]) || {Atom,N} <- lists:zip(Atoms,lists:seq(1,length(Atoms)))], ", " ),
  "};\n"
  "enum ",T," read_", T, "(char *buf, int *idx);\n"
  "int write_", T, "(ei_x_buff *x, enum ",T," t);\n"
  "\n"], 
  % io:format("~s: ~p~n", [Type, Atoms]),

  Reader = ["enum ",T," read_", T, "(char *buf, int *idx) {\n"
  "  char atom[MAXATOMLEN+1];\n"
  "  if(ei_decode_atom(buf,idx,atom) < 0) return 0;\n",
  [io_lib:format("  if(!strcmp(atom,\"~s\")) return ~s_~s;\n",[Atom,Type,Atom]) || Atom <- Atoms],
  "  return -1;\n" 
  "}\n"],

  Writer = ["int write_", T, "(ei_x_buff *x, enum ",T," t) {\n"
  "  switch(t) {\n",
  [io_lib:format("    case ~s_~s: ei_x_encode_atom(x,\"~s\"); break;\n", [Type,Atom,Atom]) || Atom <- Atoms],
  "    default: ei_x_encode_atom(x,\"undefined\");\n"
  "  }\n"
  "  return 0;\n"
  "}\n\n"],

  Gen#gen{enums = [{Type,Atoms}|Enums], headers = Headers ++ Header, parser = Parser ++ Reader ++ Writer};

generate_type({record,Record}, Desc, #gen{headers = Headers, parser = Parser, structs = Structs} = Gen) ->
  io:format("record ~s~n", [Record]),

  Fields = lists:map(fun
    ({typed_record_field,Field,{type,_,union, [{atom,_,undefined},{type,_,Type,_}]}}) -> 
      {hint(Type,Gen),field_name(Field),field_default(Field)};
    ({typed_record_field,Field,{type,_,union, [{type,_,Type,_},{atom,_,undefined}]}}) -> 
      {hint(Type,Gen),field_name(Field), field_default(Field)};
    ({typed_record_field,Field,{type,_,union, [{type,_,Type,_}]}}) ->
      {hint(Type,Gen),field_name(Field),field_default(Field)};
    ({typed_record_field,Field,{type,_,any,[]}}) ->
      {any, field_name(Field), undefined};
    ({typed_record_field,{record_field,_,{atom,_,Field},{Type,_,Default}},{type,_,union,_}}) ->
      {hint(Type,Gen),Field,Default};
    ({typed_record_field,Field,{type,_,Type,[]}}) ->
      {hint(Type,Gen),field_name(Field),field_default(Field)};
    ({typed_record_field,Field,{type,_,list,[{type,_,Type,_}]}}) ->
      case translate_type(Type,Gen) of
        undefined -> io:format("undefined list type: ~p~n",[Type]), {any,field_name(Field), undefined};
        _RealType -> {{list,hint(Type,Gen)},field_name(Field),[]}
      end
  end, Desc),

  StringFields = [{T,atom_to_list(F),D} || {T,F,D} <- Fields],
  R = atom_to_list(Record),

  io:format("Record fields: ~240p~n",[Fields]),

  Header = ["struct ", atom_to_list(Record), "{\n",
  lists:map(fun
    ({any,_,_}) ->
      "";
    ({{list,Type},Field,_}) ->
      io_lib:format("  int ~s_count;\n  ~s *~s;\n", [Field, translate_type(Type,Gen), Field]);
    ({Type,Field,_Default}) ->
      make_entry(translate_type(Type,Gen),Field)
  end,Fields),
  "};\n"
  "struct ",R,"* read_",R,"(char *buf, int *idx);\n"
  "int write_",R,"(ei_x_buff* x, struct ",R," r);\n"
  "\n"],
  Struct = {Record, StringFields},



  Reader = ["struct ",R,"* read_", R, "(char *buf, int *idx) {\n"
  "  // Declare all fields and unpack tuple header\n"
  "  ssize_t out_size = sizeof(struct ",R,");\n"
  "  struct ",R,"* r;\n"
  "  char header[MAXATOMLEN+1];\n"
  "  int arity=-1;\n"
  "  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;\n"
  "  if(ei_decode_atom(buf, idx, header) < 0) return NULL;\n"
  "  if(strcmp(header,\"",R,"\")) return NULL;\n"

  "  // Now need to jump through the message to calculate total memory\n"
  "  int idx1 = *idx;\n",
  lists:map(fun
    ({binary,_Field,_}) ->
      ["  {int t,s;\n  ei_get_type(buf,&idx1,&t,&s);\n"
      "  if(t != ERL_BINARY_EXT) return NULL;\n"
      "  out_size += s;}\n"];
    (_) ->
      "  ei_skip_term(buf,&idx1);\n"
  end, StringFields),

  "  // Now allocate structure\n"
  "  r = (struct ",R,"*)calloc(out_size,1);\n"
  "  void *endptr = (void *)r + sizeof(struct ",R,");\n\n"
  "  // Unpack all fields\n",
  lists:map(fun
    ({{enum,Type}, Field, _Default}) ->
      [io_lib:format("  r->~s = read_~s(buf,idx);\n", [Field, Type]),
      io_lib:format("  if(!r->~s) {free(r); return NULL;}\n",[Field])];
    ({float, Field, Default}) when is_float(Default) ->
      ["  if(ei_decode_double(buf,idx,&r->",Field,") < 0) {\n"
      "     ei_skip_term(buf,idx);\n"
      "     r->",Field," = ",io_lib:format("~.3f",[Default]),";\n  }\n"];
    ({float, Field, undefined}) ->
      ["  if(ei_decode_double(buf,idx,&r->",Field,") < 0) {\n"
      "     ei_skip_term(buf,idx);\n"
      "     r->",Field," = 0.0;\n  }\n"];
    ({Int, Field, Default}) when is_integer(Default) andalso (Int == non_neg_integer orelse Int == integer)->
      ["  if(ei_decode_long(buf,idx,&r->",Field,") < 0) {\n"
      "     ei_skip_term(buf,idx);\n"
      "     r->",Field," = ",io_lib:format("~B",[Default]),";\n  }\n"];
    ({Int, Field, undefined}) when (Int == non_neg_integer orelse Int == integer)->
      ["  if(ei_decode_long(buf,idx,&r->",Field,") < 0) {\n"
      "     ei_skip_term(buf,idx);\n"
      "     r->",Field," = 0;\n  }\n"];
    ({binary, Field, _}) ->
      ["  {int t,s;\n  ei_get_type(buf,idx,&t,&s);\n"
      "  if(t != ERL_BINARY_EXT) {free(r); return NULL;}\n"
      "  r->",Field,".size = s;}\n"
      "  r->",Field,".data = endptr;\n"
      "  endptr += r->",Field,".size;\n"
      "  ei_decode_binary(buf,idx,r->",Field,".data,&r->",Field,".size);\n"];
    ({Type, Field, _}) ->
      ["  ei_skip_term(buf,idx); // ",io_lib:format("~p ",[Type]),Field,"\n"]
  end, StringFields),
  "  return r;\n"
  "}\n\n"],


  Writer = ["int write_",R,"(ei_x_buff* x, struct ",R," r) {\n"
  "  ei_x_encode_tuple_header(x, ", integer_to_list(length(Fields)+1) , ");\n"
  "  ei_x_encode_atom(x, \"",R,"\");\n",
  lists:map(fun
    ({{enum,Type},Field,_}) -> ["  write_",atom_to_list(Type),"(x, r.",Field,");\n"];
    ({float, Field, _}) -> ["  ei_x_encode_double(x, r.",Field,");\n"];
    ({Int, Field, _}) when Int == non_neg_integer orelse Int == integer -> ["  ei_x_encode_double(x, r.",Field,");\n"];
    ({binary, Field, _}) -> ["  ei_x_encode_binary(x, r.",Field,".data, r.",Field,".size);\n"];
    (_) -> "  ei_x_encode_atom(x, \"undefined\");\n"
  end, StringFields),
  "  return 0;\n"
  "}\n\n"],

  Gen#gen{headers = Headers ++ Header, structs = Structs ++ [Struct], parser = Parser ++ Reader ++ Writer};

generate_type(Type, {type, _, record, [{atom,_,Record}]}, #gen{structs = Structs} = Gen) ->
  case lists:keyfind(Record, 1, Structs) of
    false -> io:format("Unknown record: ~p/~p~n",[Type,Record]);
    _ -> ok
  end,
  Gen;

generate_type(Type, {type, _, Alias, []}, #gen{aliases = Aliases} = Gen) when is_atom(Type) ->
  Gen#gen{aliases = add_alias(Type,Alias,Aliases)};

generate_type(Type, Desc, Gen) ->
  io:format("type: ~240p - ~240p~n", [Type, Desc]),
  Gen.

make_entry(undefined, _) -> "";
make_entry(Type, Field) -> io_lib:format("  ~s ~s;\n", [Type,Field]).




field_name({record_field,_,{atom,_,Name}}) -> Name;
field_name({record_field,_,{atom,_,Name},_}) -> Name.

field_default({record_field,_,_}) -> undefined;
field_default({record_field,_,_,{_Type,_,Value}}) -> Value.


hint(Type, #gen{enums = Enums, structs = Structs, aliases = Aliases}) ->
  case lists:keyfind(Type, 1, Enums) of
    false -> 
      case lists:keyfind(Type, 1, Structs) of
        false -> 
          case lists:keyfind(Type, 1, Aliases) of
            {Type,Alias} -> Alias;
            false -> Type
          end;
        _ -> {struct, Type}
      end;
    _ -> {enum,Type}
  end.


translate_type(float,_) -> double;
translate_type(non_neg_integer,_) -> long;
translate_type(integer,_) -> long;
translate_type(binary,_) -> "struct Binary";
translate_type(string,_) -> "char*";
translate_type({enum,Type}, _) -> io_lib:format("enum ~s", [Type]);
translate_type({struct,Type}, _) -> io_lib:format("struct ~s", [Type]);
translate_type(Type, #gen{}) -> io:format("unknown_translate_type '~s'~n",[Type]), undefined.



add_alias(Type, Alias, Aliases) ->
  case lists:keyfind(Alias, 1, Aliases) of
    {Alias, Alias1} -> add_alias(Type, Alias1, Aliases);
    false -> [{Type,Alias}|Aliases]
  end.




