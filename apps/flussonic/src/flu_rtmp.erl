%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        rtmp
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
-module(flu_rtmp).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-export([create_client/1]).
-export([init/1, handle_control/2, handle_rtmp_call/2, handle_info/2]).
-export([no_function/2, publish/2, play/2]).

-export([play_url/2]).


-export([clients/0]).

clients() ->
  case erlang:whereis(rtmp_session_sup) of
    undefined ->
      [];
    _ ->
      clients0()
  end.

clients0() ->
  Now = flu:now_ms(),
  Pids = [Pid || {_, Pid, _, _} <- supervisor:which_children(rtmp_session_sup)],
  Clients = [begin
    {dictionary, Info} = process_info(Pid, dictionary),
    Ip = case proplists:get_value(remote_ip, Info) of
      undefined -> [];
      Ip_ -> [{ip,Ip_}]
    end,
    case proplists:get_value(rtmp_play, Info) of
      undefined ->
        undefined;
      {_Type, Name, StartAt} ->
        Ip ++ [{type,<<"rtmp">>},{pid,Pid},{name,Name},{start_at, StartAt}, {duration, Now - StartAt}]
    end
  end || Pid <- Pids],
  [Client || Client <- Clients, Client =/= undefined].

play_url(Name, URL) ->
  {ok, Proxy} = flussonic_sup:start_stream_helper(Name, publish_proxy, {flu_publish_proxy, start_link, [fun() ->
    rtmp_lib:play(URL)
  end, self()]}),
  {ok, Proxy}.
  

create_client(RTMP) ->
  {ok, Pid} = supervisor:start_child(rtmp_session_sup, [?MODULE]),
  rtmp_session:set_socket(Pid, RTMP),
  {ok, Pid}.
  
init(Session) ->
  put(sent_bytes, 0),
  {ok, Session}.

handle_control({stream_died, _}, Session) ->
  self() ! exit,
  {ok, Session};

handle_control(_Control, Session) ->
  {ok, Session}.

handle_info({read_burst, StreamId, Fragment}, Session) ->
  Stream = rtmp_session:get_stream(StreamId, Session),
  Media = rtmp_stream:get(Stream, pid),
  case flu_file:get(Media, {hds_segment, Fragment}) of
    {ok, {Format,Reader,Id,StopDTS}} ->
      {noreply, Session1} = rtmp_session:handle_info({ems_stream, StreamId, burst_start}, Session),
      ReadFun = fun(Key) -> Format:read_frame(Reader, Key) end,
      {Duration, Acc} = accumulate_frames(ReadFun, Id, undefined, 0, [], StreamId, StopDTS),
      RTMP = rtmp_session:get(Session, socket),
      {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
      Bin = iolist_to_binary(Acc),
      put(sent_bytes, get(sent_bytes) + size(Bin)),
      gen_tcp:send(Socket, Bin),
      {noreply, Session2} = rtmp_session:handle_info({ems_stream, StreamId, burst_stop}, Session1),
      Sleep = if Fragment < 3 -> 0;
        true -> round(Duration)
      end,
      erlang:send_after(Sleep, self(), {read_burst, StreamId, Fragment+1}),
      {noreply, Session2};
    {error, no_segment} ->
      rtmp_session:handle_info({ems_stream, StreamId, play_complete, 0}, Session)
  end;

handle_info({'DOWN', _, _, _, _}, State) ->
  {stop, normal, State};


handle_info(_Info, State) ->
  ?D({_Info}),
  {noreply, State}.
  
accumulate_frames(ReadFun, Id, MinDTS, MaxDTS, Acc, StreamId, StopDTS) ->
  case ReadFun(Id) of
    eof -> {MaxDTS - MinDTS, lists:reverse(Acc)};
    #video_frame{dts = DTS} when DTS >= StopDTS -> {MaxDTS - MinDTS, lists:reverse(Acc)};
    #video_frame{next_id = Next, dts = DTS} = Frame ->
      FlvFrameGen = flv:rtmp_tag_generator(Frame),
      Min = case MinDTS of undefined -> DTS; _ -> MinDTS end,
      accumulate_frames(ReadFun, Next, Min, DTS, [FlvFrameGen(0, StreamId)|Acc], StreamId, StopDTS)
  end.



handle_rtmp_call(Session, AMF) ->
  {ok, Env} = application:get_env(flussonic, config),
  ChainList = proplists:get_value(rtmp_handlers, Env, [?MODULE]),
  case (catch call_mfa(ChainList, Session, AMF)) of
    reject ->
      rtmp_session:reject_connection(Session);
    Reply -> 
      Reply
  end.

mod_name(Mod) when is_tuple(Mod) -> element(1, Mod);
mod_name(Mod) -> Mod.

call_mfa([], Session, AMF) ->
  {unhandled, Session, AMF};

call_mfa([Module|Modules], Session, #rtmp_funcall{command = Command} = AMF) ->
  case code:is_loaded(mod_name(Module)) of
    false -> 
      case code:load_file(mod_name(Module)) of
        {module, _ModName} -> ok;
        _ -> erlang:error({cant_load_file, Module})
      end;
    _ -> ok
  end,
  % ?D({"Checking", Module, Command, ems:respond_to(Module, Command, 2)}),
  case erlang:function_exported(Module, Command, 2) of
    true ->
      case Module:Command(Session, AMF) of
        unhandled -> call_mfa(Modules, Session, AMF);
        {unhandled, NewState, NewAMF} -> call_mfa(Modules, NewState, NewAMF);
        Reply -> Reply
      end;
    false ->
      call_mfa(Modules, Session, AMF)
  end.

normalize_path(<<"mp4:", Path/binary>>) -> <<Path/binary, ".mp4">>;
normalize_path(<<"flv:", Path/binary>>) -> <<Path/binary, ".flv">>;
normalize_path(<<"f4v:", Path/binary>>) -> <<Path/binary, ".f4v">>;
normalize_path(Path) -> Path.

clear_path(<<"/", Path/binary>>) -> Path;
clear_path(Path) -> Path.

play(Session, #rtmp_funcall{args = [null, Path1 | _]} = AMF) ->
  Path2 = normalize_path(Path1),
  Path = clear_path(Path2),
  {StreamName0, _Args} = http_uri2:parse_path_query(Path),
  StreamName = list_to_binary(StreamName0),
  case flu_media:find_or_open(StreamName) of
    {ok, {Type, Media}} ->
      put(remote_ip, rtmp_session:get(Session, addr)),
      put(rtmp_play, {Type, Media, flu:now_ms()}),
      case Type of
        file -> play_file(Session, AMF, StreamName, Media);
        stream -> play_stream(Session, AMF, StreamName, Media)
      end;
    {error, _Error} ->
      RTMP = rtmp_session:get(Session, socket),
      rtmp_lib:fail(RTMP, AMF),
      Session
  end.

play_file(Session, #rtmp_funcall{stream_id = StreamId} = _AMF, StreamName, Media) ->
  erlang:monitor(process, Media),
  MediaInfo = flu_file:media_info(Media),
  Configs = video_frame:config_frames(MediaInfo) ++ [video_frame:meta_frame(MediaInfo)],

  Session1 = rtmp_session:set_stream(rtmp_stream:construct([{pid, Media}, {stream_id, StreamId}, {base_dts,0}, {name, StreamName}, {started, true}]), Session),
  RTMP = rtmp_session:get(Session1, socket),
  rtmp_lib:play_start(RTMP, StreamId, 0, file),
  
  Session2 = lists:foldl(fun(F, Sess) ->
    rtmp_session:send_frame(F#video_frame{stream_id = StreamId}, Sess)
  end, Session1, Configs),
  
  self() ! {read_burst, round(StreamId), 1},
  
  Session2.


play_stream(Session, #rtmp_funcall{stream_id = StreamId} = _AMF, StreamName, _StreamName) ->
  {ok, Media} = flu_stream:find(StreamName),
  erlang:monitor(process, Media),
  flu_stream:subscribe(Media, []),
  Session1 = rtmp_session:set_stream(rtmp_stream:construct([{pid, Media}, {stream_id, StreamId}, {name, StreamName}, 
    {started, false}, {options, [{media_type,stream}]}]), Session),
  Session1.


publish(Session, #rtmp_funcall{stream_id = StreamId, args = [null, false|_]} = _AMF) ->
  Stream = rtmp_session:get_stream(StreamId, Session),
  Pid = rtmp_stream:get(Stream, pid),
  erlang:exit(Pid),
  rtmp_session:set_stream(rtmp_stream:set(Stream, pid, undefined), Session);

publish(Session, #rtmp_funcall{stream_id = StreamId, args = [null, Name |_]} = _AMF) ->
  Prefix = binary_to_list(rtmp_session:get(Session, path)),
  {ok, Env} = application:get_env(flussonic, config),
  Options = case [Entry || {live,Pref,_Options} = Entry <- Env, Pref == Prefix] of
    [{live, Prefix, Opts}] -> Opts;
    [] -> []
  end,
  {match,[StreamName]}=re:run(Name,"([^?]+)\\?*",[{capture,all_but_first,binary}]),
  
  case proplists:get_value(publish_password, Env) of
    undefined ->
      ok;
    PasswordSpec ->
      [Login,Password] = string:tokens(PasswordSpec, ":"),
      {_RawName, Args} = http_uri2:parse_path_query(Name),

      UserLogin = proplists:get_value("login", Args),
      UserPassword = proplists:get_value("password", Args),
      case {UserLogin, UserPassword} of
        {Login, Password} -> 
          ok;
        _ ->
          error_logger:error_msg("Publish denied, wrong password: ~p, ~p~n", [UserLogin, UserPassword]),
          throw(reject)
      end
  end,
  
  {ok, Recorder} = flu_stream:autostart(StreamName, [{clients_timeout,false},{static,false}|Options]),
  gen_tracker:setattr(flu_streams, StreamName, [{play_prefix,rtmp_session:get(Session, path)}]),
  flu_stream:set_source(Recorder, self()),
  
  {ok, Proxy} = flussonic_sup:start_stream_helper(StreamName, publish_proxy, {flu_publish_proxy, start_link, [self(), Recorder]}),
  
  Ref = erlang:monitor(process, Recorder),
  Socket = rtmp_session:get(Session, socket),
  ?D({publish,StreamName,Name,StreamId}),
  
  rtmp_lib:notify_publish_start(Socket, StreamId, 0, Name),
  rtmp_session:set_stream(rtmp_stream:construct([{pid,Proxy},{recording_ref,Ref},{stream_id,StreamId},{started, true}, {recording, true}, {name, Name}]), Session).
  

no_function(_Session, _AMF) ->
  ?D({unhandled, _AMF}),
  unhandled.
