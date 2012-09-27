%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        flu_config
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of flussonic.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(flu_config).
-author('Max Lapshin <max@maxidoors.ru>').

-export([load_config/0, parse_routes/1]).
-export([lookup_config/0, parse_config/2]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-spec load_config() -> {ok, Config::list(), Path::file:filename()} | {error, Error::term()}.
load_config() ->
  case lookup_config() of
    {ok, Config1, ConfigPath} ->
      case parse_config(Config1, ConfigPath) of
        {ok, Config2} ->
          {ok, Config2, ConfigPath};
        {error, Error} ->
          {error, Error}
      end;
    {error, Error} ->
      {error, Error}
  end.


-spec lookup_config() -> {ok, Config::list(), Path::file:filename()} | {error, Error::term()}.
lookup_config() ->
  case os:getenv("FLU_CONFIG") of
    false ->
      ConfigPaths = ["priv", "/etc/flussonic", "priv/sample"],
      case file:path_consult(ConfigPaths, "flussonic.conf") of
        {ok, Env1, ConfigPath} ->
          {ok, Env1, ConfigPath};
        {error, Error} ->
          {error, Error}
      end;
    Path ->
      case file:consult(Path) of
        {ok, Env} ->
          {ok, Env, Path};
        {error, Error} ->
          {error, Error}
      end
  end.



parse_config(Config, ConfigPath) -> 
  Env2 = expand_options(load_includes(Config, ConfigPath)),
  {ok, Env2}.




load_includes(Env, ConfigPath) ->
  load_includes(Env, filename:dirname(ConfigPath), []).

load_includes([{include, Wildcard}|Env], Root, Acc) ->
  Files = filelib:wildcard(Wildcard, Root),
  Env1 = lists:foldr(fun(File, Env_) ->
    {ok, SubEnv, SubPath} = file:path_consult([Root], File),
    ?D({include,SubPath}),
    SubEnv ++ Env_
  end, Env, Files),
  load_includes(Env1, Root, Acc);

load_includes([Command|Env], Root, Acc) ->
  load_includes(Env, Root, Acc ++ [Command]);

load_includes([], _, Acc) ->
  Acc.




expand_options(Env) ->
  [expand_entry(Entry) || Entry <- Env].

expand_entry({rewrite, Path, URL}) -> {stream, list_to_binary(Path), list_to_binary(URL), [{static,false}]};
expand_entry({rewrite, Path, URL, Options}) -> {stream, list_to_binary(Path), list_to_binary(URL), [{static,false}|Options]};
expand_entry({stream, Path, URL}) -> {stream, list_to_binary(Path), list_to_binary(URL), [{static,true}]};
expand_entry({stream, Path, URL, Options}) -> {stream, list_to_binary(Path), list_to_binary(URL), [{static,true}|Options]};
expand_entry({mpegts, Prefix}) -> {mpegts, Prefix, []};
expand_entry({mpegts, Prefix, Options}) -> {mpegts, Prefix, Options};
expand_entry({live, Prefix}) -> {live, Prefix, []};
expand_entry({live, Prefix, Options}) -> {live, Prefix, Options};
expand_entry(Entry) -> Entry.

merge(Opts1, Opts2) ->
  lists:ukeymerge(1, lists:ukeysort(1, Opts1), lists:ukeysort(1,Opts2)).

merge(Opts1, Opts2, Opts3) ->
  merge(Opts1, merge(Opts2, Opts3)).


parse_routes(Env) ->
  GlobalOptions = [],
  parse_routes(Env, [], GlobalOptions).


parse_routes([{live, Prefix, Opts}|Env], Acc, GlobalOptions) ->
  Tokens = [list_to_binary(T) || T <- string:tokens(Prefix, "/")],
  parse_routes(Env, Acc ++ [
    {Tokens ++ ['...'], media_handler, merge(Opts, [{autostart,false},{module,flu_stream},{dynamic,true}], GlobalOptions)}
  ], GlobalOptions);

parse_routes([{stream, Path, URL, Options}|Env], Acc, GlobalOptions) ->
  {Tokens2, _, _} = cowboy_dispatcher:split_path(Path, fun(Bin) -> cowboy_http:urldecode(Bin, crash) end),
  
  Acc1 = [
    {Tokens2 ++ [<<"mpegts">>], mpegts_handler, merge([{name,Path},{url,URL}], Options, GlobalOptions)},
    {Tokens2 ++ ['...'], media_handler, merge([{name,Path},{url,URL},{module,flu_stream},{name_length,length(Tokens2)}], Options, GlobalOptions)}
  ],
  % Acc1 = lists:foldl(fun(Proto, Acc0) ->
  %   Acc0 ++ [{Tokens2 ++ Suffix, Handler, Options++[{name,Path},{url,URL},{module,flu_stream}]++Opts} || {Suffix, Handler, Opts} <- routes_for_proto(Proto)]
  % end, [], Protos),
  
  % ?D({Tokens2, URL, Protos, Acc1}),
  parse_routes(Env, Acc ++ Acc1, GlobalOptions);

parse_routes([{file, Prefix, Root}|Env], Acc, GlobalOptions) ->
  Tokens = [list_to_binary(T) || T <- string:tokens(Prefix, "/")],
  parse_routes(Env, Acc++ [
    {Tokens ++ ['...'], media_handler, merge([{module,flu_file},{root, Root}], GlobalOptions)}
  ], GlobalOptions);

parse_routes([{root, Root}|Env], Acc, GlobalOptions) ->
  parse_routes(Env, Acc ++ [
  {[], cowboy_http_static, [
    {directory, Root},
    {mimetypes, [{<<".html">>,[<<"text/html">>]}]},
    {file, <<"index.html">>}
  ]},
  {['...'], cowboy_http_static, [
    {directory,Root},
    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
  ]}], GlobalOptions);

parse_routes([{mpegts,Prefix,Options}|Env], Acc, GlobalOptions) ->
  Tokens = [list_to_binary(T) || T <- string:tokens(Prefix, "/")],
  parse_routes(Env, Acc ++ [
    {Tokens ++ ['...'], mpegts_handler, merge(Options, GlobalOptions)}
  ], GlobalOptions);

parse_routes([api|Env], Acc, GlobalOptions) ->
  parse_routes([{api,[]}|Env], Acc, GlobalOptions);

parse_routes([{api,Options}|Env], Acc, GlobalOptions) ->
  parse_routes(Env, Acc ++ [
    {[<<"erlyvideo">>,<<"api">>,<<"reload">>], api_handler, [{mode,reload}|Options]},
    {[<<"erlyvideo">>,<<"api">>,<<"streams">>], api_handler, [{mode,streams}|Options]},
    {[<<"erlyvideo">>,<<"api">>,<<"stream_health">>, '...'], api_handler, [{mode,health}|Options]},
    {[<<"erlyvideo">>,<<"api">>,<<"dvr_status">>, year, month, day, '...'], dvr_handler, [{mode,status}|Options]},
    {[<<"erlyvideo">>,<<"api">>,<<"dvr_previews">>, year, month, day, hour, minute, '...'], dvr_handler, [{mode,previews}|Options]}
  ], GlobalOptions);

parse_routes([sessions|Env], Acc, GlobalOptions) ->
  parse_routes(Env, Acc, [{sessions,true}|GlobalOptions]);

parse_routes([_Else|Env], Acc, GlobalOptions) ->
  parse_routes(Env, Acc, GlobalOptions);

parse_routes([], Acc, _GlobalOptions) ->
  Acc.

  