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

-export([set_config/1, get_config/0]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


set_config(Config) ->
  application:set_env(flussonic, config, Config).

get_config() ->
  case application:get_env(flussonic, config) of
    {ok, Config} -> Config;
    undefined -> []
  end.



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


to_b(String) when is_list(String) -> list_to_binary(String);
to_b(Binary) when is_binary(Binary) -> Binary;
to_b(undefined) -> undefined;
to_b(Atom) when is_atom(Atom) -> binary_to_atom(Atom, latin1).

expand_options(Env) ->
  [expand_entry(Entry) || Entry <- Env].

expand_entry({rewrite, Path, URL}) -> {stream, to_b(Path), to_b(URL), [{static,false}]};
expand_entry({rewrite, Path, URL, Options}) -> {stream, to_b(Path), to_b(URL), [{static,false}|Options]};
expand_entry({stream, Path, URL}) -> {stream, to_b(Path), to_b(URL), [{static,true}]};
expand_entry({stream, Path, URL, Options}) -> {stream, to_b(Path), to_b(URL), [{static,true}|Options]};
expand_entry({mpegts, Prefix}) -> {mpegts, to_b(Prefix), []};
expand_entry({mpegts, Prefix, Options}) -> {mpegts, to_b(Prefix), Options};
expand_entry({live, Prefix}) -> {live, to_b(Prefix), []};
expand_entry({live, Prefix, Options}) -> {live, to_b(Prefix), Options};
expand_entry({file, Prefix, Root}) -> {file, to_b(Prefix), to_b(Root), []};
expand_entry({file, Prefix, Root, Options}) -> {file, to_b(Prefix), to_b(Root), Options};
expand_entry(Entry) -> Entry.

merge(Opts1, Opts2) ->
  lists:ukeymerge(1, lists:ukeysort(1, Opts1), lists:ukeysort(1,Opts2)).

merge(Opts1, Opts2, Opts3) ->
  merge(Opts1, merge(Opts2, Opts3)).


parse_routes(Env) ->
  GlobalOptions = [],
  parse_routes(Env, [], GlobalOptions).


parse_routes([{live, Prefix, Opts}|Env], Acc, GlobalOptions) ->
  Tokens = tokens(Prefix),
  parse_routes(Env, Acc ++ [
    {Tokens ++ ['...'], media_handler, merge(Opts, [{autostart,false},{module,flu_stream},{dynamic,true}], GlobalOptions)}
  ], GlobalOptions);

parse_routes([{stream, Path, URL, Options}|Env], Acc, GlobalOptions) ->
  Tokens2 = tokens(Path),
  
  Acc1 = [
    {Tokens2 ++ [<<"mpegts">>], mpegts_handler, merge([{name,Path},{url,URL}], Options, GlobalOptions)},
    {Tokens2 ++ ['...'], media_handler, merge([{name,Path},{url,URL},{module,flu_stream},{name_length,length(Tokens2)}], Options, GlobalOptions)}
  ],
  % Acc1 = lists:foldl(fun(Proto, Acc0) ->
  %   Acc0 ++ [{Tokens2 ++ Suffix, Handler, Options++[{name,Path},{url,URL},{module,flu_stream}]++Opts} || {Suffix, Handler, Opts} <- routes_for_proto(Proto)]
  % end, [], Protos),
  
  % ?D({Tokens2, URL, Protos, Acc1}),
  parse_routes(Env, Acc ++ Acc1, GlobalOptions);

parse_routes([{file, Prefix, Root,Options}|Env], Acc, GlobalOptions) ->
  Tokens = tokens(Prefix),
  parse_routes(Env, Acc++ [
    {Tokens ++ ['...'], media_handler, merge([{module,flu_file},{root, Root}|Options], GlobalOptions)}
  ], GlobalOptions);

parse_routes([{root, Root}|Env], Acc, GlobalOptions) ->
  Module = case is_escriptized(Root) of
    true -> static_http_escript;
    false -> cowboy_http_static
  end,
  parse_routes(Env, Acc ++ [
  {[], Module, [
    {directory, Root},
    {mimetypes, [{<<".html">>,[<<"text/html">>]}]},
    {file, <<"index.html">>}
  ]},
  {['...'], Module, [
    {directory,Root},
    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
  ]}], GlobalOptions);

parse_routes([{mpegts,Prefix,Options}|Env], Acc, GlobalOptions) ->
  Tokens = tokens(Prefix),
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


tokens(String) ->
  {Tokens, _, _} = cowboy_dispatcher:split_path(String, fun(Bin) -> cowboy_http:urldecode(Bin, crash) end),
  Tokens.



is_escriptized(Root) ->
  case file:read_file_info(Root) of
    {error, enoent} ->
      case application:get_env(flussonic, escript_files) of
        {ok, _} -> true;
        _ -> false
      end;
    _ ->
      false
  end.



expand_entry_test_() ->
  [?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,false}]}]},
      parse_config([{rewrite, "stream1", "fake://stream1"}], undefined)),
  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,false},{dvr,"root"}]}]},
      parse_config([{rewrite, "stream1", "fake://stream1", [{dvr,"root"}]}], undefined)),

  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,true}]}]},
      parse_config([{stream, "stream1", "fake://stream1"}], undefined)),
  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,true},{dvr,"root"}]}]},
      parse_config([{stream, "stream1", "fake://stream1", [{dvr,"root"}]}], undefined)),

  ?_assertEqual({ok, [{mpegts, <<"stream">>, []}]},
      parse_config([{mpegts, "stream"}], undefined)),
  ?_assertEqual({ok, [{mpegts, <<"stream">>, [{sessions, "http://host"}]}]},
      parse_config([{mpegts, "stream", [{sessions, "http://host"}]}], undefined)),

  ?_assertEqual({ok, [{live, <<"live">>, []}]},
      parse_config([{live, "live"}], undefined)),
  ?_assertEqual({ok, [{live, <<"live">>, [{sessions, "http://host"}]}]},
      parse_config([{live, "live", [{sessions, "http://host"}]}], undefined)),

  ?_assertEqual({ok, [{file, <<"vod">>, <<"/movies">>, []}]}, 
      parse_config([{file, "vod", "/movies"}], undefined)),
  ?_assertEqual({ok, [{file, <<"vod">>, <<"/movies">>, [{sessions, "http://ya.ru/"}]}]}, 
      parse_config([{file, "vod", "/movies", [{sessions, "http://ya.ru/"}]}], undefined))
  ].

parse_route_test_() ->
  [
    ?_assertMatch([{[<<"live">>,<<"injest">>, '...'], media_handler, _}], 
      parse_routes([{live, <<"live/injest">>, []}])),
    ?_assertMatch([{[<<"vod">>,<<"mp4">>,'...'], media_handler, _}],
      parse_routes([{file, <<"vod/mp4">>, <<"/movies">>, []}]))
  ].



  