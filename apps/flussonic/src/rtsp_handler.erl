%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        RTSP handler
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
-module(rtsp_handler).
-author('Max Lapshin <max@maxidoors.ru>').

-export([handle/2, handle/3]).


-export([read/2]).

read(URL, Options) ->
  Path = proplists:get_value(path, Options),
  gen_tracker:find_or_open(flu_streams, Path, fun() ->
    {ok, Recorder} = flussonic_sup:start_flu_stream(Path, Options),
    {ok,_RTSP,_MediaInfo} = rtsp_socket:read(URL, [{consumer,  Recorder},{timeout,15000},
                           {dump_traffic,proplists:get_value(dump_traffic,Options,true)},
                           {transport,proplists:get_value(transport,Options,tcp)},
  			 {tracks, proplists:get_value(tracks,Options)}]),
    % {ok, RTSP, _MediaInfo};
    
    % #media_info{video = [#stream_info{params = #video_params{width = Width, height = Height}}]} = MediaInfo,
    
    % {ok, Reader} = mpegts_sup:start_reader([{consumer, Recorder},{url, URL}|Options]),
    % mpegts_reader:set_socket(Reader, Socket),
    {ok, Recorder}
  end).
  

handle(Req, _Env) ->
  cowboy_http_req:reply(404, [], <<"404  not found\n">>, Req).
  
handle(Req, _Env, _Path) ->
  cowboy_http_req:reply(404, [], <<"404  not found\n">>, Req).
