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
-export([no_function/2, publish/2, play/2, seek/2]).

-export([play_url/3]).


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

play_url(Name, URL, Options) ->
  RTMPOptions = proplists:get_value(rtmp_play, Options, []),
  {ok, Proxy} = flussonic_sup:start_stream_helper(Name, publish_proxy, {flu_publish_proxy, start_link, [URL, self(), RTMPOptions]}),
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

flush_burst_timer(Session) ->
  receive
    {read_burst, _, _, _} -> flush_burst_timer(Session)
  after
    0 ->
      case rtmp_session:get(Session, burst_timer) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
      end
  end.

handle_info({read_burst, StreamId, Fragment, BurstCount}, Session) ->
  flush_burst_timer(Session),
  Stream = rtmp_session:get_stream(StreamId, Session),
  Media = rtmp_stream:get(Stream, pid),
  case flu_file:read_gop(Media, Fragment) of
    {ok, Gop} ->
      {noreply, Session1} = rtmp_session:handle_info({ems_stream, StreamId, burst_start}, Session),

      Acc = [begin
        FlvFrameGen = flv:rtmp_tag_generator(Frame),
        FlvFrameGen(0, StreamId)
      end || Frame <- Gop],
      Bin = iolist_to_binary(Acc),
      Duration = (lists:last(Gop))#video_frame.dts - (hd(Gop))#video_frame.dts,

      RTMP = rtmp_session:get(Session, socket),
      {rtmp, Socket} = try rtmp_socket:get_socket(RTMP)
      catch
        exit:{_, _} -> throw({stop, normal, Session})
      end,
      put(sent_bytes, get(sent_bytes) + size(Bin)),
      gen_tcp:send(Socket, Bin),
      {noreply, Session2} = rtmp_session:handle_info({ems_stream, StreamId, burst_stop}, Session1),
      Sleep = if BurstCount > 0 -> 0;
        true -> round(Duration)
      end,
      BurstTimer = erlang:send_after(Sleep, self(), {read_burst, StreamId, Fragment+1, BurstCount - 1}),
      Session3 = rtmp_session:set(Session2, burst_timer, BurstTimer),
      {noreply, Session3};
    {error, no_segment} ->
      rtmp_session:handle_info({ems_stream, StreamId, play_complete, 0}, Session)
  end;

handle_info({'DOWN', _, _, _, _}, State) ->
  {stop, normal, State};

handle_info(#media_info{}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  ?D({_Info}),
  {noreply, State}.
  


handle_rtmp_call(Session, AMF) ->
  ChainList = proplists:get_value(rtmp_handlers, flu_config:get_config(), [?MODULE]),
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


seek(Session, #rtmp_funcall{args = [null,DTS], stream_id = StreamId} = _AMF) ->
  RTMP = rtmp_session:get(Session, socket),
  Stream = rtmp_session:get_stream(StreamId, Session),
  Name = rtmp_stream:get(Stream, name),
  Keyframes = flu_file:keyframes(Name),
  SkipGops = length(lists:takewhile(fun({TS,_}) -> TS < DTS end, Keyframes)),

  flush_burst_timer(Session),
  % ?debugFmt("seek to ~p frament",[SkipGops]),
  self() ! {read_burst, StreamId, SkipGops, 3},
  % ?debugFmt("seek(~p) ~s", [round(DTS), Name]),

  % rtmp_lib:reply(RTMP, AMF),

  rtmp_lib:seek_notify(RTMP, StreamId, DTS),
  Session.


play(Session, #rtmp_funcall{} = AMF) ->
  try play0(Session, AMF) of
    Session1 -> Session1
  catch
    throw:{fail, Args} ->
      RTMP = rtmp_session:get(Session, socket),
      rtmp_lib:fail(RTMP, AMF#rtmp_funcall{args = [null|Args]}),
      Session
  end.

to_b(undefined) -> undefined;
to_b(List) when is_list(List) -> list_to_binary(List);
to_b(Bin) when is_binary(Bin) -> Bin.


fmt(Fmt, Args) -> iolist_to_binary(io_lib:format(Fmt, Args)).

play0(Session, #rtmp_funcall{args = [null, Path1 | _]} = AMF) ->
  Path2 = normalize_path(Path1),
  Path = clear_path(Path2),
  {StreamName0, QsVals} = http_uri2:parse_path_query(Path),
  App = rtmp_session:get_field(Session, app),


  {Type, Args, Options} = case lookup_config(flu_config:get_config(), App, iolist_to_binary(StreamName0)) of
    {error, _} ->
      throw({fail, [404, fmt("failed to find in config ~s/~s", [App, StreamName0])]});
    {ok, Spec} -> Spec
  end,
  StreamName1 = case Type of
    file -> iolist_to_binary(StreamName0);
    stream -> iolist_to_binary(StreamName0);
    live -> Args
  end,

  StreamName = case proplists:get_value(sessions, Options) of
    undefined -> StreamName1;
    URL ->
      Token = case to_b(proplists:get_value("token", QsVals)) of
        undefined -> to_b(proplists:get_value("session", QsVals));
        Token_ -> Token_
      end,
      is_binary(Token) orelse throw({fail, [403, <<"no_token_passed">>]}),
      Ip = to_b(rtmp_session:get(Session, addr)),
      is_binary(Ip) orelse error({bad_ip, Ip, Session}),
      Identity = [{name,StreamName1},{ip, Ip},{token,Token}],
      Referer = rtmp_session:get_field(Session, pageUrl),
      case flu_session:verify(URL, Identity, [{pid,self()},{referer,Referer},{type,<<"rtmp">>}|Options]) of
        {ok, StreamName1_} -> StreamName1_;
        {error, Code, Message} ->
          lager:error("auth denied play(~s/~s) with token(~s): ~p:~p", [App, StreamName1, Token, Code, Message]),
          throw({fail, [403, Code, to_b(Message), App, StreamName1, <<"auth_denied">>]})
      end
  end,

  put(remote_ip, rtmp_session:get(Session, addr)),
  put(rtmp_play, {Type, StreamName, flu:now_ms()}),

  case find_or_open_media(Type, StreamName, Args, Options) of
    {ok, Media} when Type == file ->
      play_file(Session, AMF, StreamName, Media);
    {ok, Media} ->
      play_stream(Session, AMF, StreamName, Media);
    {error, _Error} ->
      lager:error("failed to play rtmp ~s//~s: ~p", [App, StreamName, _Error]),
      throw({fail, [500, fmt("failed to play rtmp ~s//~s: ~p", [App, StreamName, _Error])]})
  end.

lookup_config([{file,App,Root,Options}|_], App, _Path) ->
  {ok, {file, Root, Options}};

lookup_config([{live,App,Options}|_], App, Path) ->
  {ok, {live, iolist_to_binary([App,"/",Path]), Options}};

lookup_config([{stream,Path,URL,Options}|_], _App, Path) ->
  {ok, {stream, URL, Options}};

lookup_config([_|Config], App, Path) ->
  lookup_config(Config, App, Path);

lookup_config([], _, _) ->
  {error, not_found}.


find_or_open_media(file, Path, Root, Options) ->
  flu_file:autostart(Path, [{root,Root}|Options]);

find_or_open_media(stream, Path, URL, Options) ->
  flu_stream:autostart(Path, [{url,URL}|Options]);

find_or_open_media(live, Path, _, Options) ->
  flu_stream:autostart(Path, Options).

  


play_file(Session, #rtmp_funcall{stream_id = StreamId} = _AMF, StreamName, Media) ->
  erlang:monitor(process, Media),
  case flu_file:media_info(Media) of
    #media_info{} = MediaInfo ->
      Configs = video_frame:config_frames(MediaInfo) ++ [video_frame:meta_frame(MediaInfo)],

      Session1 = rtmp_session:set_stream(rtmp_stream:construct([{pid, Media}, {stream_id, StreamId}, {base_dts,0}, {name, StreamName}, {started, true}]), Session),
      RTMP = rtmp_session:get(Session1, socket),
      rtmp_lib:play_start(RTMP, StreamId, 0, file),
      
      Session2 = lists:foldl(fun(F, Sess) ->
        rtmp_session:send_frame(F#video_frame{stream_id = StreamId}, Sess)
      end, Session1, Configs),
      
      self() ! {read_burst, round(StreamId), 1, 3},
      
      Session2;
    {return, _Code, Msg} ->
      lager:error("failed to play file: ~s", [Msg]),
      throw(reject)
  end.


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

publish(Session, AMF) ->
  try publish0(Session, AMF)
  catch
    throw:{fail, Args} ->
      RTMP = rtmp_session:get(Session, socket),
      rtmp_lib:fail(RTMP, AMF#rtmp_funcall{args = [null|Args]}),
      throw(shutdown)
  end.


publish0(Session, #rtmp_funcall{stream_id = StreamId, args = [null, Name |_]} = _AMF) ->
  Prefix = rtmp_session:get_field(Session, app),
  Env = flu_config:get_config(),

  Options = case [Entry || {live,Pref,_Options} = Entry <- Env, Pref == Prefix] of
    [{live, Prefix, Opts}] -> Opts;
    [] ->
      lager:error("Tried to publish to invalid RTMP app ~s from addr ~p", [Prefix, rtmp_session:get(Session, addr)]),
      throw({fail, [403, <<Prefix/binary, "/", Name/binary>>, <<"no_application">>]})
  end,

  {match,[StreamName1]} = re:run(Name,"([^?]+)\\?*",[{capture,all_but_first,binary}]),
  StreamName = <<Prefix/binary, "/", StreamName1/binary>>,
  Env = flu_config:get_config(),

  {_RawName, Args} = http_uri2:parse_path_query(Name),
  
  case proplists:get_value(password, Options) of
    RequiredPassword when RequiredPassword =/= undefined ->
      case proplists:get_value("password", Args) of
        RequiredPassword -> ok;
        WrongPassword ->
          lager:error("Publish denied, wrong password: ~p", [WrongPassword]),
          throw({fail, [403, StreamName, <<"wrong_password">>]})
      end;
    undefined -> 
      case proplists:get_value(publish_password, Env) of
        undefined ->
          ok;
        PasswordSpec ->
          [Login,Password] = string:tokens(PasswordSpec, ":"),

          UserLogin = proplists:get_value("login", Args),
          UserPassword = proplists:get_value("password", Args),
          case {UserLogin, UserPassword} of
            {Login, Password} -> 
              ok;
            _ ->
              lager:error("Publish denied, wrong password: ~p, ~p", [UserLogin, UserPassword]),
              throw({fail, [403, StreamName, <<"wrong_login_password">>]})
          end
      end
  end,


  {ok, Recorder} = flu_stream:autostart(StreamName, [{clients_timeout,false},{static,false}|Options]),
  gen_tracker:setattr(flu_streams, StreamName, []),
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
