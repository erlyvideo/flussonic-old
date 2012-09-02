%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        Static stream watcher
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
-module(static_stream_watcher).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).
-include("log.hrl").


start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE, [], []).

init([]) ->
  erlang:send_after(1000, self(), recheck),
  ?D("start watcher"),
  {ok, state}.

% check if started
recheck(Stream, URL, StreamOpts) ->
  Opts = merge(StreamOpts, [{module, flu_stream}, {name, Stream}, {name_length, 1}, {url, URL}]),
  flu_stream:autostart(Stream, Opts).

handle_info(recheck, State) ->
  {ok, Env} = application:get_env(flussonic, config),
  % {http, HTTPPort} = lists:keyfind(http, 1, Env),
  Streams = [Name || {Name, _} <- flu_stream:list()],
  ToRestart = [A || {stream, Stream, _, _} = A <- Env, not lists:member(Stream, Streams)],
  [recheck(Stream, URL, Opts) || {stream, Stream, URL, Opts} <- ToRestart, proplists:get_bool(static, Opts)],
  % [http_stream:request_body("http://localhost:"++integer_to_list(HTTPPort)++"/"++binary_to_list(Stream)++"/index.m3u8", [{keepalive,false}]) || Stream <- ToRestart],
  erlang:send_after(3000, self(), recheck),
  {noreply, State}.

terminate(_,_) -> ok.

merge(Opts0, Opts1) -> lists:ukeymerge(1, lists:ukeysort(1, Opts0), lists:ukeysort(1, Opts1)).
