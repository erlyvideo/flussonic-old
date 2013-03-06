%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        media handler
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
-module(media_handler).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([check_sessions/3]).


%%% All sessions code is beneath

retrieve_token(Req0) ->
  case cowboy_req:qs_val(<<"token">>, Req0, undefined) of
    {undefined, Req1} -> 
      case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
        {undefined, Req1} ->  cowboy_req:cookie(<<"token">>, Req1, undefined);
        V -> V
      end;
    V -> V
  end.

check_sessions(Req, Name, Opts) ->
  case proplists:get_value(sessions, Opts) of
    undefined -> {ok, undefined};    % no backend specified
    URL -> check_sessions0(URL, Name, Req, proplists:get_value(type, Opts, <<"http">>), Opts)
  end.

check_sessions0(URL, Name0, Req0, Type, Opts) ->
  % case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
  case retrieve_token(Req0) of
    {undefined, _} -> throw({return, 403, "no_token_passed"}); % no token specified
    {Token, Req1} ->
      {PeerAddr, _} = cowboy_req:peer_addr(Req1),
      Ip = list_to_binary(inet_parse:ntoa(PeerAddr)),
      {Referer, _} = cowboy_req:header(<<"referer">>, Req1),
      Identity = [{token,Token},{name,Name0},{ip,Ip}],
      Options = [{type,Type}] ++ case Referer of
        undefined -> [];
        _ -> [{referer,Referer}]
      end ++ case proplists:get_value(pid, Opts) of
        undefined -> [];
        Pid -> [{pid,Pid}]
      end,
      case flu_session:verify(URL, Identity, Options) of
        {ok, _} ->
          {ok, Token};
        {error, Code, Reply} ->
          throw({return,Code, Reply})
      end
  end.




