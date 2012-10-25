-module(static_http_escript).
-author('Max Lapshin <max@maxidoors.ru>').


-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any,http}, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_, _) ->
  ok.


handle(Req, Opts) ->
  Path = case proplists:get_value(file, Opts) of
    undefined ->
      {PathInfo, _} = cowboy_req:path_info(Req),
      filename:join(PathInfo);
    File ->
      File
  end,
  {ok, Req2} = case static_file:send_internal(Req, [{path,Path}]) of
    unhandled ->
      {ok, _Req2} = cowboy_req:reply(404, [], "404 Not found\n", Req);
    {ok, Req2_} ->
      {ok, Req2_}
  end,
  {ok, Req2, state}.

