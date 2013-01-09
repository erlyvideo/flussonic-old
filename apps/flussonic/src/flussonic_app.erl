%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        flussonic_app
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
-module(flussonic_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([load_config/0, unload_config/0]).
% -export([current_cowboy_port/1]).
-include("log.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	gen_tracker_sup:start_tracker(flu_files),
	gen_tracker_sup:start_tracker(flu_streams),
  % flu_session:start(),
  
  {ok, Pid} = flussonic_sup:start_link(),
  {ok, Pid}.
  
  
stop(_State) ->
  ok.


unload_config() ->
  flu_event:remove_handler(flu_session_log),
  ok.



% current_cowboy_port(Name) ->
%   try current_cowboy_port0(Name) of
%     Result -> Result
%   catch
%     _:_ -> undefined
%   end.
      

% current_cowboy_port0(Name) ->
%   {{cowboy_listener_sup,Name},Pid1,_,_} = lists:keyfind({cowboy_listener_sup,Name}, 1, supervisor:which_children(cowboy_sup)),
%   [state, _, _, ChildSpec |_] = tuple_to_list(ems_debug:get_state(Pid1)),
%   [child, _Pid2, cowboy_acceptors_sup, {_M,_F,A} |_] = tuple_to_list(lists:keyfind(cowboy_acceptors_sup, 3, ChildSpec)),
%   [_Count, cowboy_tcp_transport, Opts|_] = A,
%   proplists:get_value(port, Opts).

load_config() ->
  case flu_config:load_config() of
    {ok, Env1, ConfigPath} ->
      flu_config:set_config(Env1),
      {ok, Vsn} = application:get_key(flussonic, vsn),
	    error_logger:info_msg("Loading config for version ~s from ~s", [Vsn, ConfigPath]),
	    ok;
    {error, enoent} ->
      error_logger:error_msg("Can't find flussonic.conf in any folder~n"),
      timer:sleep(2000),
      erlang:halt(1);
    {error, Else} ->
      error_logger:error_msg("Can't open flussonic.conf: ~p~n", [Else]),
      timer:sleep(2000),
      erlang:halt(1)
	end,  
  Env = flu_config:get_config(),
  Routes = flu_config:parse_routes(Env),
  Dispatch = [{'_', Routes}],
  {http, HTTPPort} = lists:keyfind(http, 1, Env),
  application:start(cowboy),


  ProtoOpts = [{dispatch, Dispatch},{max_keepalive,4096}],
  
  stop_http(flu_http),
  start_http(flu_http, 100, 
    [{port,HTTPPort},{backlog,4096},{max_connections,8192}],
    ProtoOpts
  ),

  % (catch cowboy:stop_listener(http)),
  % TODO move from cowboy supervisor tree to flussonic supervisor tree
  % cowboy:start_listener(http, 100, 
  %   cowboy_tcp_transport, [{port,HTTPPort},{backlog,4096},{max_connections,8192}],
  %   cowboy_http_protocol, ProtoOpts
  % ),
  % end,
  
  
  [catch flu_stream:update_options(Stream, [{url,URL}|StreamOpts]) || {stream, Stream, URL, StreamOpts} <- Env],
  ConfigStreams = [Stream || {stream, Stream, _URL, _StreamOpts} <- Env],
  [catch flu_stream:non_static(Name) || {Name, _} <- flu_stream:list(), not lists:member(Name, ConfigStreams)],
  
  
  case proplists:get_value(rtmp, Env) of
    undefined -> rtmp_socket:stop_server(rtmp_listener1);
    RTMPPort ->
	  	?D({"Start RTMP server at port", RTMPPort}),
  	  rtmp_socket:start_server(RTMPPort, rtmp_listener1, flu_rtmp)
  end,
  case proplists:get_value(rtsp, Env) of
	  undefined -> rtsp_socket:stop_server(rtsp_listener1);
	  RTSPPort ->
	    ?D({"Start RTSP server at port", RTSPPort}),
	    rtsp:start_server(RTSPPort, rtsp_listener1, flu_rtsp)
	end,

  case proplists:get_value(sessions_log, Env) of
    undefined -> ok;
    SessionLog -> flu_event:add_handler(flu_session_log, SessionLog)
  end,

  [begin
    (catch Plugin:module_info()),
    case erlang:function_exported(Plugin, start, 1) of
      true -> Plugin:start(Options);
      false -> ok
    end
  end || {plugin, Plugin, Options} <- flu_config:get_config()],
  ok.


start_http(Ref, NbAcceptors, TransOpts, ProtoOpts)
    when is_integer(NbAcceptors) ->
  cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts).

stop_http(Ref) ->
  cowboy:stop_listener(Ref).
