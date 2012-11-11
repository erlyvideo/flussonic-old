%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        multibitrate packetizer
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(multibitrate_packetizer).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, handle_info/2, terminate/2, update_options/2]).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-record(mbr, {
  name,
  media_info
}).


init(Options) ->
  Name = proplists:get_value(name, Options),
  {ok, MBR} = update_options(Options, #mbr{name = Name}),
  {ok, MBR}.

update_options(_Options, #mbr{} = MBR) ->
  {ok, MBR}.

set_media_info(#media_info{} = MI, #mbr{} = MBR) ->
  MBR#mbr{media_info = MI}.


handle_info(#video_frame{}, #mbr{media_info = undefined} = State) ->
  {noreply, State};

handle_info(#media_info{} = MI, #mbr{} = MBR) ->
  {noreply, set_media_info(MI, MBR)};

handle_info(#video_frame{} = _Frame, #mbr{} = MBR) ->
  {noreply, MBR}.


terminate(_, #mbr{}) ->
  ok.






