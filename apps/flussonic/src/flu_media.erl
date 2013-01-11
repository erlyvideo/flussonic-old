%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        
% This module must help different protocols to make lookups in flussonic config and launch streams and files.
% There are three basic types of media in flussonic: 
%
% 1) static stream that is already launched. It may be accessed via HTTP without any prefix
% 2) live stream that is published by users ondemand
% 3) files
%
% Files and streams live in different tracking tables, but they live in the same HTTP url namespace
%
% Each media has two different names: 
% 1) its outer name, which is visible via HTTP
% 2) its inner URL that points to real media
%
% Live medias has null URL, because they dont know when and how to reconnect.
% 
%
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
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
-module(flu_media).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([find/1, find_or_open/1, lookup/1]).
-export([find_or_open/2]).



find_or_open(Path, Headers) ->
  try find_or_open0(Path, Headers)
  catch
    throw:{error, Code} -> {error, Code};
    Class:Error ->
      ?debugFmt("find_or_open(~s) ~p:~p~n~240p~n",[Path, Class, Error, erlang:get_stacktrace()]),
      erlang:raise(Class, Error, erlang:get_stacktrace())
  end.


find_or_open0(Path, Headers) ->
  case lookup(Path) of
    {ok, {Type, Name, Opts}} ->
      case proplists:get_value(sessions, Opts) of
        undefined ->
          {ok, Reply} = autostart(Type, Name, Opts),
          {ok, Reply};
        AuthUrl ->
          Identity = proplists:get_value(identity, Headers),
          SessionParams = proplists:get_value(session, Headers),
          case flu_session:verify(AuthUrl, Identity, SessionParams) of
            {ok, _} ->
              {ok, Reply} = autostart(Type, Name, Opts),
              {ok, Reply};
            {error, Code, _Message} ->
              {error, Code}
          end
      end;
    {error, enoent} ->
      {error, 404}
  end.




find(Path) ->
  case gen_tracker:find(flu_streams, Path) of
    {ok, Pid} -> 
      {stream, Pid};
    undefined ->
      case gen_tracker:find(flu_files, Path) of
        undefined ->
          undefined;
        {ok, Pid} ->
          {file, Pid}
      end
  end.

find_or_open(Path) when is_list(Path) ->
  find_or_open(list_to_binary(Path));

find_or_open(Path) ->
  case gen_tracker:find(flu_streams, Path) of
    undefined ->
      case lookup(Path) of
        {ok, {Type, Name, Opts}} ->
          autostart(Type, Name, Opts);
        {error, _} = Error ->
          Error
      end;
    {ok,_Pid} ->
      {ok,{stream,Path}}
  end.
  
autostart(file, File, Opts) -> open_file(proplists:get_value(root,Opts), File, []);
autostart(Type, Stream, Opts) when Type == stream orelse Type == live -> 
  {ok, Media} = flu_stream:autostart(Stream, Opts),
  {ok, {stream, Media}}.


  % case proplists:get_value(type, Env) of
  %   <<"stream">> ->
  %     {ok, _Pid} = open_stream(Env),
  %     {ok, {stream, Path}};
  %   _ ->  
  %     find_or_open(file,Env)
  % end;

lookup(Path) when is_list(Path) ->
  lookup(list_to_binary(Path));
  
lookup(Path) ->
  lookup(Path, flu_config:get_config()).
  

lookup(Path, [{live, Prefix, Options}|Config]) ->
  PrefixLen = size(Prefix),
  case Path of
    <<Prefix:PrefixLen/binary, "/", _Stream/binary>> -> {ok, {live, Path, [{prefix,Prefix}|Options]}};
    _ -> lookup(Path, Config)
  end;

lookup(Path, [{file, Prefix, Root, Opts}|Config]) ->
  PrefixLen = size(Prefix),
  case Path of
    <<Prefix:PrefixLen/binary, "/", File/binary>> -> 
      {ok, {file, File, [{root, Root}|Opts]}};
    _ -> lookup(Path, Config)
  end;
  
lookup(Path, [{stream, Path, URL, Opts}|_Config]) ->
  {ok, {stream, Path, [{url,URL}|Opts]}};

lookup(Path, [_Option|Config]) ->
  lookup(Path, Config);

lookup(_Path, []) ->
  {error, enoent}.


open_file(Root, Path, _Env) ->
  case gen_tracker:find_or_open(flu_files, Path, fun() -> flussonic_sup:start_flu_file(Path, [{root,Root}]) end) of
  	{ok, File} ->
  	  {ok, {file,File}};
  	Error ->
  	  Error
  end.


