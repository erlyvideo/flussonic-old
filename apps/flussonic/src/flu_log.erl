%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        log files
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
-module(flu_log).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([session/2]).

-define(SESS_FMT_TOKENS, [{$s, session_id},
                          {$t, token},
                          {$i, ip},
                          {$n, name},
                          {$u, user_id},
                          {$f, flag},
                          {$y, type}, % :: <<"hds">>|<<"hls">>|<<"http">>
                          {$c, created_at},
                          {$e, expire_time},
                          {$l, last_access_time},
                          {$b, bytes_sent}]).
-define(SESS_FMT_DEFAULT, "%n %i %y").

%% @doc Send session status and stats to lager
-spec session(atom(), proplists:proplist()) -> ok.
session(Status, Stats) ->
  case proplists:get_value(sessions_log, flu_config:get_config()) of
    indefined -> ok; % no sessions log enabled
    Conf ->
      Fmt = case proplists:get_value(format, Conf) of
              undefined -> ?SESS_FMT_DEFAULT;
              Val -> Val
            end,
      lager:info([{log, sessions}], "~s ~s", [Status, session_format(Fmt, Stats)])
  end.

%% @doc format session stat to string
session_format(Fmt, Stats) -> session_format(Fmt, Stats, "").
session_format([$%|[FT|Fmt0]=Fmt], Stats, Log) ->
  case proplists:get_value(FT, ?SESS_FMT_TOKENS) of
    undefined -> session_format(Fmt, Stats, [$%|Log]); % unknown token, iterate it again
    Prop -> session_format(Fmt0, Stats, [prop_to_str(Prop, Stats)|Log])
  end;
session_format([Chr|Fmt], Stats, Log) -> session_format(Fmt, Stats, [Chr|Log]);
session_format([], _Stats, Log) -> lists:flatten(lists:reverse(Log)).

%% @doc get proplist value as string
prop_to_str(P, PL) ->
  case proplists:get_value(P, PL) of
    V when is_binary(V) -> binary_to_list(V);
    V when is_list(V) -> V;
    V -> io_lib:format("~p", [V])
  end.
