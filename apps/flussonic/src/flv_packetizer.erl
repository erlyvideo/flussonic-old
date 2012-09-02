%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        flv packetizer
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
-module(flv_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([init/1, handle_info/2]).
-include_lib("erlmedia/include/video_frame.hrl").



init(Options) ->
  Name = proplists:get_value(name, Options),
  {ok, Writer} = flv_writer:init_file(binary_to_list(Name) ++ ".flv", []),
  {ok, Writer}.


handle_info(#video_frame{} = Frame, Writer) ->
  {ok, Writer1} = flv_writer:write_frame(Frame, Writer),
  {noreply, Writer1};

handle_info(_, Writer) ->
  {noreply, Writer}.

