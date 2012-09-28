%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        common media
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
-export([find/1, find_or_open/1, lookup_path/1]).


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
      lookup_path(Path);
    {ok,_Pid} ->
      {ok,{stream,Path}}
  end.
  
  % case proplists:get_value(type, Env) of
  %   <<"stream">> ->
  %     {ok, _Pid} = open_stream(Env),
  %     {ok, {stream, Path}};
  %   _ ->  
  %     find_or_open(file,Env)
  % end;

lookup_path(Path) when is_list(Path) ->
  lookup_path(list_to_binary(Path));
  
lookup_path(Path) ->
  lookup_path(Path, flu_config:get_config()).
  

lookup_path(Path, [{live, Prefix1}|Config]) ->
  Prefix = list_to_binary(Prefix1),
  PrefixLen = size(Prefix),
  case Path of
    <<Prefix:PrefixLen/binary, "/", Stream/binary>> -> {ok, {stream, Stream}};
    _ -> lookup_path(Path, Config)
  end;

lookup_path(Path, [{file, Prefix1, Root}|Config]) ->
  Prefix = list_to_binary(Prefix1),
  PrefixLen = size(Prefix),
  case Path of
    <<Prefix:PrefixLen/binary, "/", File/binary>> -> open_file(iolist_to_binary([Root, "/", File]), File, []);
    _ -> lookup_path(Path, Config)
  end;
  
lookup_path(Path, [{stream, Path, URL, Opts}|_Config]) ->
  {ok, Media} = flu_stream:autostart(Path, [{url,URL}|Opts]),
  {ok, {stream, Media}};

lookup_path(Path, [_Option|Config]) ->
  lookup_path(Path, Config);

lookup_path(_Path, []) ->
  {error, enoent}.


open_file(FullPath, Path, Env) ->   
  Multibitrate = case proplists:get_value(multibitrate, Env) of
    <<"on">> -> true;
    _ -> false
  end,
  
  CanOpen = case FullPath of
    <<"http:/", _/binary>> -> true;
    _ -> case filelib:is_regular(FullPath) of
      true -> true;
      false ->
        case Multibitrate of
          false -> false;
          true -> case flu_file:mbr_files(FullPath) of
            [] -> false;
            _ -> mbr
          end 
        end  
    end
  end,
  case CanOpen of
    true ->
      case gen_tracker:find_or_open(flu_files, Path, fun() -> flussonic_sup:start_flu_file(FullPath, [{path,Path}]) end) of
      	{ok, File} ->
      	  {ok, {file,File}};
      	Error ->
      	  Error
            end;
    mbr ->
      {ok, {mbr, FullPath, Path}};
    false -> 
      {error, enoent}
  end.


