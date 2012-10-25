-module(static_file).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("kernel/include/file.hrl").

-export([send/2, send_internal/2]).
-export([load_escript_files/0, read_internal/1, wildcard/1]).


wildcard(Re) ->
  {ok, Files} = application:get_env(flussonic, escript_files),
  Matched = [{Path, re:run(Path, Re) =/= nomatch} || {Path, _, _} <- Files],
  [Path || {Path, true} <- Matched].

send_internal(Req, Env) ->
  Path = proplists:get_value(path, Env),
  try_internal(Req, <<"wwwroot/",Path/binary>>).
  
clean(Path) ->
  P1 = re:replace(Path, "//", "/", [{return,list},global]),
  if
    P1 == Path -> P1;
    true -> clean(P1)
  end.
  
try_internal(Req, Path) ->
  case read_internal(Path) of
    {ok, Body} ->
      MimeType = case mimetypes:path_to_mimes(Path) of
        [Mime|_] -> Mime;
        [] -> <<"application/octet-stream">>
      end,
      {ok, _Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, MimeType}], Body, Req);
    {error, enoent} ->
      case read_internal(<<Path/binary, "/index.html">>) of
        {ok, Body} ->
          {ok, _Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req);
        {error, enoent} ->
          unhandled
      end
  end.    

read_internal(Path) when is_binary(Path) ->
  read_internal(binary_to_list(Path));

read_internal(Path1) when is_list(Path1) ->
  Path = clean(Path1),
  {ok, EscriptFiles} = application:get_env(flussonic, escript_files),
  case lists:keyfind(Path, 1, EscriptFiles) of
    false -> {error, enoent};
    {_Path, _Info, Bin} -> {ok, Bin}
  end.
  

load_escript_files() ->
  ok = application:load(flussonic),
  {ok, Opts} = escript:extract(escript:script_name(), [compile_source]),
  ArchiveBin = proplists:get_value(archive, Opts),
  {ok, Files} = zip:foldl(fun(InnerPath, GetInfo, GetBin, Acc) ->
    [{InnerPath, GetInfo(), GetBin()}|Acc]
  end, [], {escript:script_name(), ArchiveBin}),
  application:set_env(flussonic, escript_files, Files).


send(Req, Env) ->
  Root = proplists:get_value(root, Env),
  Path = proplists:get_value(path, Env),
  % {_Headers, _} = cowboy_req:headers(Req),
  try_file(Req, <<Root/binary, "/", Path/binary>>).
  
try_file(Req, FullPath) -> 
  case file:read_file_info(FullPath) of
    {ok, #file_info{size = Size, mtime = MTime, type = regular}} ->
      {ok, F} = file:open(FullPath, [binary,read,raw]),
      {ok, Req1} = cowboy_req:set_resp_header(<<"Content-Type">>, misultin_utility:get_content_type(binary_to_list(FullPath)), Req),
      {ok, Req3} = cowboy_req:set_resp_header(<<"Last-Modified">>, cowboy_clock:rfc2109(MTime), Req1),
      {ok, Req6} = case cowboy_req:header('Range', Req3) of
        {undefined, _} ->
          {ok, Req4} = cowboy_req:set_resp_header(<<"Content-Length">>, list_to_binary(integer_to_list(Size)), Req3),
          {ok, Req5} = cowboy_req:reply(200, Req4),
          file:sendfile(F, cowboy_req:get(socket, Req4), 0, Size, []),
          {ok, Req5};
        {<<"bytes=", Range/binary>>, _} ->
          {match, [Start,End]} = re:run(Range, "(\\d+)-(\\d+)", [{capture,all_but_first,list}]),
          {ok, Req4} = cowboy_req:set_resp_header(<<"Range">>, ["bytes=",Range,"/",integer_to_list(Size)], Req3),
          Offset = list_to_integer(Start),
          Bytes = lists:min([list_to_integer(End) + 1, Size]) - Offset,
          {ok, Req4_1} = cowboy_req:set_resp_header(<<"Content-Length">>, list_to_binary(integer_to_list(Bytes)), Req4),
          {ok, Req5} = cowboy_req:reply(206, Req4_1),
          file:sendfile(F, cowboy_req:get(socket,Req5), Offset, Bytes, []),
          {ok, Req5}
      end,    
      file:close(F),
      {ok, Req6};
    {ok, #file_info{type = directory}} ->
      try_file(Req, <<FullPath/binary, "/index.html">>);
    {error, enoent} ->
      unhandled;
    {error, eisdir} ->
      try_file(Req, <<FullPath/binary, "/index.html">>);
    {error, _Else} ->
      {ok, _Req2} = cowboy_req:reply(403, [], io_lib:format("403 Not accessible: ~p\n", [_Else]), Req)
  end.
  