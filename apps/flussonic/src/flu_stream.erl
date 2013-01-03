%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        stream
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
-module(flu_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("flu_stream.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,hds_segment/2,hls_segment/2,hds_manifest/1,bootstrap/1,hls_playlist/1, hls_key/2]).
-export([media_info/1]).
-export([hds_manifest/2, rewrite_manifest/2]).

-export([subscribe/2, subscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([autostart/1]).
-export([autostart/2, list/0, json_list/0, publish/2]).

-export([get/2, get/3, pass_message/2, find/1]).
-export([non_static/1, static/1, update_options/2]).
-export([set_source/2]).

-define(RETRY_LIMIT, 10).
-define(TIMEOUT, 70000).
-define(SOURCE_TIMEOUT, 20000).

list() ->
  lists:sort([{Name, stream_info(Name, Attrs)} || {Name, Attrs} <- gen_tracker:list(flu_streams)]).

json_list() ->
  Streams = [[{name,Name}|parse_attr(Attr)] || {Name,Attr} <- list()],
  {ok, Vsn} = application:get_key(flussonic, vsn),
  [{streams,Streams},{version, list_to_binary(Vsn)}].

white_keys() ->
  [dvr, hls, hds, last_dts, lifetime, name, type, ts_delay, client_count, play_prefix, retry_count].

parse_attr(Attr) ->
  [{K,V} || {K,V} <- Attr, (is_binary(V) orelse is_number(V) orelse V == true orelse V == false) andalso lists:member(K,white_keys())].


stream_info(Name, Attrs) ->
  ClientCount = length([true || Client <- flu_session:list(), proplists:get_value(name, Client) == Name]),
  [{client_count,ClientCount}|add_ts_delay(filter_list(Attrs))].

filter_list(Attrs) ->
  [{K,V} || {K,V} <- Attrs, is_atom(K)].

find(Pid) when is_pid(Pid) -> {ok, Pid};
find(Name) when is_list(Name) -> find(list_to_binary(Name));
find(Name) -> gen_tracker:find(flu_streams, Name).
  
add_ts_delay(Attrs) ->
  Now = os:timestamp(),
  Attr1 = case proplists:get_value(last_dts_at, Attrs) of
    undefined -> Attrs;
    LastDTSAt -> [{ts_delay,timer:now_diff(Now, LastDTSAt) div 1000}|Attrs]
  end,
  Attr2 = case proplists:get_value(last_access_at, Attr1) of
    undefined -> Attr1;
    LastAccessAt -> [{client_delay,timer:now_diff(Now, LastAccessAt) div 1000}|Attr1]
  end,
  Attr2.


get(Stream, Key) ->
  get(Stream, Key, 0).

get(_Stream, _Key, Timeout) when Timeout < 0 ->
  undefined;

get(Stream, Key, Timeout) when is_pid(Stream) ->
  case gen_server:call(Stream, {get, Key}) of
    undefined when Timeout > 0 ->
      timer:sleep(1000),
      get(Stream, Key, Timeout - 1000);
    Reply ->
      Reply
  end;

get(Stream, Key, Timeout) when is_list(Stream) ->
  get(list_to_binary(Stream), Key, Timeout);

get(Stream, Key, Timeout) ->
  case flu_stream_data:get(Stream, Key) of
    undefined ->
      case gen_tracker:find(flu_streams, Stream) of
        {ok, Pid} -> get(Pid, Key, Timeout);
        undefined -> undefined
      end;
    Value ->
      % gen_tracker:setattr(flu_streams, Stream, [{last_access_at, os:timestamp()}]),
      Value
  end.



non_static(Stream) ->
  case find(Stream) of
    {ok, Pid} -> Pid ! non_static;
    _ -> false
  end.

static(Stream) ->
  case find(Stream) of
    {ok, Pid} -> Pid ! static;
    _ -> false
  end.

update_options(Stream, Options) ->
  case find(Stream) of
    {ok, Pid} -> gen_server:call(Pid, {update_options, Options});
    _ -> false
  end.

autostart(Stream) ->
  case lookup_in_config(Stream, flu_config:get_config()) of
    undefined -> gen_tracker:find(flu_streams, Stream);
    {ok, Stream1, Options} -> autostart(Stream1, Options)
  end.


lookup_in_config(Path, [{live, Prefix, Options}|Config]) ->
  PrefixLen = size(Prefix),
  case Path of
    <<Prefix:PrefixLen/binary, "/", Stream/binary>> -> {ok, Stream, Options};
    _ -> lookup_in_config(Path, Config)
  end;

lookup_in_config(Path, [{stream, Path, URL, Opts}|_Config]) ->
  {ok, Path, [{url,URL}|Opts]};

lookup_in_config(Path, [_|Config]) ->
  lookup_in_config(Path, Config);

lookup_in_config(_, []) ->
  undefined.


autostart(Stream, Options) ->
  gen_tracker:find_or_open(flu_streams, Stream, fun() -> flussonic_sup:start_flu_stream(Stream,Options) end).

media_info(Stream) ->
  Reply = flu_stream:get(Stream, media_info),
  Reply.

hds_segment(Stream,Segment) ->
  Reply = flu_stream:get(Stream, {hds_segment, 1, Segment}),
  Reply.

hls_segment(Stream, Segment) ->
  Reply = flu_stream:get(Stream, {hls_segment,Segment}),
  Reply.

hls_key(Stream, Number) ->
  Reply = flu_stream:get(Stream, {hls_key, Number}),
  Reply.
  

hds_manifest(Stream) ->
  Reply = flu_stream:get(Stream, hds_manifest, 10000),
  Reply.

hds_manifest(Stream, Token) ->
  case hds_manifest(Stream) of
    {ok, Manifest} -> rewrite_manifest(Manifest, Token);
    Else -> Else
  end.


rewrite_manifest(Manifest, Token) when is_binary(Manifest) andalso is_binary(Token) ->
  Manifest1 = binary:replace(Manifest, <<"url=\"bootstrap\"">>, 
    <<"url=\"bootstrap?token=",Token/binary, "\"">>),
  {ok, Manifest1}.



bootstrap(Stream) ->
  Reply = flu_stream:get(Stream, bootstrap, 10000),
  Reply.

hls_playlist(Stream) ->
  Reply = flu_stream:get(Stream, hls_playlist, 10000),
  Reply.


publish(Stream,Frame) ->
  Stream ! Frame.

subscribe(Stream) when is_binary(Stream) ->
  {ok, {stream, Pid}} = flu_media:find_or_open(Stream),
  subscribe(Pid, []).

subscribe(Stream, Options) when is_binary(Stream) ->
  {ok, Pid} = autostart(Stream, Options),
  subscribe(Pid, Options);

subscribe(Pid, _Options) when is_pid(Pid) ->
  erlang:monitor(process, Pid),
  gen_server:call(Pid, {subscribe, self()}).

set_source(Stream, Source) when is_pid(Stream) andalso is_pid(Source) ->
  gen_server:call(Stream, {set_source, Source}).

set_last_dts(DTS, Now) ->
  erlang:put(last_dts_at, Now),
  erlang:put(last_dts, DTS),
  FirstDTS = case erlang:get(first_dts) of
    undefined -> put(first_dts, DTS), DTS;
    FDTS -> FDTS
  end,
  Lifetime = DTS - FirstDTS,
  gen_tracker:setattr(flu_streams, get(name), [{last_dts, DTS},{last_dts_at,Now},{lifetime,Lifetime}]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name,Options) ->
  gen_server:start_link(?MODULE, [Name,Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name,Options1]) ->
  Options = lists:ukeymerge(1, lists:ukeysort(1,Options1), [{name,Name}]),
  erlang:put(name, Name),
  put(configs, []),
  Source = proplists:get_value(source, Options1),
  if is_pid(Source) -> erlang:monitor(process, Source); true -> ok end,
  CheckTimer = erlang:send_after(3000, self(), check_timeout),
  Stream1 = #stream{last_dts_at=os:timestamp(), last_access_at = os:timestamp(), 
    name = Name, options = Options, source = Source,
    check_timer = CheckTimer},
  % timer:send_interval(1000, next_second),
  
  Stream2 = set_options(Stream1),
  
  ?DBG("Start stream \"~s\" with url ~p and options: ~p", [Name, Stream2#stream.url, Options]),
  self() ! reconnect_source,
  {ok, Stream2}.

set_options(#stream{options = Options, name = Name, url = URL1, source = Source1} = Stream) ->
  {URL, Source} = case proplists:get_value(url, Options) of
    URL1 -> {URL1, Source1};
    URL2 ->
      (catch erlang:exit(Source1, kill)),
      self() ! reconnect_source,
      {URL2, undefined}
  end,
  Stream1 = set_timeouts(Stream),
  Dump = proplists:get_value(dump, Options),
  Stream2 = configure_packetizers(Stream1),
  gen_tracker:setattr(flu_streams, Name, [{url,URL}]),
  Stream2#stream{url = URL, source = Source, dump_frames = Dump}.

set_timeouts(#stream{options = Options} = Stream) ->
  Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
  SourceTimeout = proplists:get_value(source_timeout, Options, ?SOURCE_TIMEOUT),
  ClientsTimeout = proplists:get_value(clients_timeout, Options, Timeout),
  RetryLimit = proplists:get_value(retry_limit, Options, ?RETRY_LIMIT),
  Static = proplists:get_bool(static, Options),
  Stream#stream{retry_limit = RetryLimit, source_timeout = SourceTimeout, clients_timeout = ClientsTimeout, timeout = Timeout, static = Static}.


init_if_required({Module, ModState} = State, Module, Options) ->
  case erlang:function_exported(Module, update_options, 2) of
    true ->
      {ok, ModState1} = Module:update_options(Options, ModState),
      {Module, ModState1};
    false ->
      State
  end;

init_if_required(_, Module, Options) ->
  case code:is_loaded(Module) of
    false -> code:load_file(Module);
    _ -> ok
  end,
  case erlang:function_exported(Module, init, 1) of
    true ->
      % ?D({init,Module,Options}),
      {ok, State} = Module:init(Options),
      {Module, State};
    false ->
      ?D({cant_init,Module,Options}),
      {blank_packetizer, blank}
  end.

shutdown_packetizer(undefined) ->
  ok;
  
shutdown_packetizer({Module,State}) ->
  Module:terminate(normal, State).
  

configure_packetizers(#stream{hls = HLS1, hds = HDS1, udp = UDP1, rtmp = RTMP1, options = Options, media_info = MediaInfo} = Stream) ->
  HLS = case proplists:get_value(hls, Options) of
    false -> shutdown_packetizer(HLS1), {blank_packetizer, undefined};
    _ -> init_if_required(HLS1, hls_dvr_packetizer, Options)
  end,
  HDS = case proplists:get_value(hds, Options) of
    false -> shutdown_packetizer(HDS1), {blank_packetizer, undefined};
    _ -> init_if_required(HDS1, hds_packetizer, Options)
  end,
  % ?D({configuring,Options, proplists:get_value(dvr,Options)}),
  UDP = case proplists:get_value(udp, Options) of
    undefined -> shutdown_packetizer(UDP1), {blank_packetizer, undefined};
    _ -> init_if_required(UDP1, udp_packetizer, Options)
  end,
  RTMP = case proplists:get_value(rtmp, Options) of
    undefined -> shutdown_packetizer(RTMP1), {blank_packetizer, undefined};
    _ -> init_if_required(RTMP1, rtmp_packerizer, Options)
  end,
  Stream1 = pass_message(MediaInfo, Stream#stream{hls = HLS, hds = HDS, udp = UDP, rtmp = RTMP}),
  Stream1.


configs(#stream{media_info = undefined}) ->
  [];
  
configs(#stream{last_dts = DTS, media_info = MediaInfo}) when is_number(DTS) ->
  [F#video_frame{dts = DTS, pts = DTS} || F <- video_frame:config_frames(MediaInfo)];

configs(#stream{last_dts = undefined, media_info = MediaInfo}) ->
  video_frame:config_frames(MediaInfo).



handle_call({update_options, NewOptions}, _From, #stream{name = Name} = Stream) ->
  NewOptions1 = lists:ukeymerge(1, lists:ukeysort(1, NewOptions), [{name,Name}]),
  % ?D({updating_options,NewOptions1}),
  Stream1 = set_options(Stream#stream{options = NewOptions1}),
  {reply, ok, Stream1};

handle_call({subscribe, Pid}, _From, #stream{clients = Clients} = Stream) ->
  Ref = erlang:monitor(process, Pid),
  [Pid ! C#video_frame{stream_id = self()} || C <- configs(Stream)],
  {reply, {ok, Ref}, Stream#stream{clients = [{Pid,Ref}|Clients]}};

handle_call({set_source, Source}, _From, #stream{source = OldSource} = Stream) ->
  OldSource == undefined orelse error({reusing_source,Stream#stream.name,OldSource,Source}),
  erlang:monitor(process, Source),
  {reply, ok, Stream#stream{source = Source}};

handle_call({set, #media_info{} = MediaInfo}, _From, #stream{} = Stream) ->
  {noreply, Stream1} = handle_info(MediaInfo, Stream),
  {reply, ok, Stream1};

handle_call({get, media_info}, _From, #stream{name = Name, media_info = MediaInfo} = Stream) ->
  Now = os:timestamp(),
  gen_tracker:setattr(flu_streams, Name, [{last_access_at, Now}]),
  {reply, MediaInfo, Stream};

handle_call({get, Key}, _From, #stream{name = Name} = Stream) ->
  Reply = case erlang:get(Key) of
    undefined -> undefined;
    Else -> {ok, Else}
  end,
  Now = os:timestamp(),
  gen_tracker:setattr(flu_streams, Name, [{last_access_at, Now}]),
  {reply, Reply, Stream#stream{last_access_at = Now}};

handle_call(#video_frame{} = Frame, _From, #stream{} = Stream) ->
  {noreply, Stream1} = handle_input_frame(Frame, Stream),
  {reply, ok, Stream1};

handle_call(_Call,_From,State) ->
  {stop,{unknown_call, _Call},State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(non_static, #stream{} = Stream) ->
  {noreply, Stream#stream{static = false}};

handle_info(static, #stream{} = Stream) ->
  {noreply, Stream#stream{static = true}};

handle_info(reconnect_source, #stream{url = undefined} = Stream) ->
  {noreply, Stream};

handle_info(reconnect_source, #stream{source = Source} = Stream) when is_pid(Source) ->
  {noreply, Stream};

handle_info(reconnect_source, #stream{retry_count = Count, retry_limit = Limit, static = false} = Stream) when Count*1 >= Limit*1 ->
  ?D({Stream#stream.name, exits_due_retry_limit, Count}),
  {stop, normal, Stream};

handle_info(reconnect_source, #stream{source = undefined, name = Name, url = URL1, retry_count = Count, options = Options} = Stream) ->
  {Proto, URL} = detect_proto(URL1),
  LogError = will_log_error(Count),
  Result = case Proto of
    tshttp -> mpegts:read(URL, [{name,Name}]);
    udp -> mpegts:read(URL, [{name,Name}]);
    udp2 -> mpegts:read(URL, [{name,Name}]);
    rtsp -> flu_rtsp:read2(Name, URL, [{log_error,LogError}|Options]);
    rtsp2 -> flu_rtsp:read2(Name, URL, [{log_error,LogError}|Options]);
    rtsp1 -> flu_rtsp:read(Name, URL, Options);
    hls -> hls:read(URL, Options);
    file -> file_source:read(URL, Options);
    rtmp -> flu_rtmp:play_url(Name, URL, Options);
    playlist -> playlist:read(Name, URL, Options);
    mixer -> flu_mixer:read(Name, URL, Options);
    passive -> {ok, undefined}
  end,
  case Result of
    {ok, Source} -> 
      erlang:monitor(process, Source),
      {noreply, Stream#stream{source = Source}};
    {ok, Source, MediaInfo} -> 
      erlang:monitor(process, Source),
      {noreply, Stream0} = handle_info(MediaInfo, Stream#stream{media_info = undefined}),
      Configs = video_frame:config_frames(MediaInfo),
      Stream1 = Stream0#stream{source = Source},
      Stream2 = lists:foldl(fun(C, Stream_) ->
        {_,Stream1_} = flu_stream_frame:save_config(C, Stream_),
        Stream1_
      end, Stream1, Configs),
      {noreply, Stream2};
    {error, Error} ->
      if LogError ->
      ?ERR("Stream \"~s\" can't open source \"~s\" (~p). Retries: ~B/~B", [Name, URL, Error, Count, Stream#stream.retry_limit]);
      true -> ok end,
      Delay = ((Count rem 30) + 1)*1000,
      erlang:send_after(Delay, self(), reconnect_source),
      gen_tracker:setattr(flu_streams, Name, [{retry_count,Count+1}]),
      {noreply, Stream#stream{retry_count = Count+1}}
  end;

handle_info(#media_info{} = MediaInfo, #stream{media_info = undefined} = Stream) ->
  put(media_info,MediaInfo),
  Stream1 = pass_message(MediaInfo, Stream#stream{media_info = MediaInfo}),
  {noreply, Stream1};

handle_info({'DOWN', _, process, Source, _Reason}, 
  #stream{source = Source, retry_count = Count, name = Name, url = URL, retry_limit = Limit} = Stream) ->
  Delay = ((Count rem 30) + 1)*1000,
  erlang:send_after(Delay, self(), reconnect_source),
  
  LogError = will_log_error(Count),
  if LogError ->  
  ?DBG("stream \"~s\" lost source \"~s\". Retry count ~p/~p", [Name, URL, Count, Limit]);
  true -> ok end,
  gen_tracker:setattr(flu_streams, Name, [{retry_count,Count+1}]),
  {noreply, Stream#stream{source = undefined, ts_delta = undefined, retry_count = Count + 1}};

handle_info(check_timeout, #stream{name = Name, static = Static, check_timer = OldCheckTimer,
  source_timeout = SourceTimeout, url = URL, source = Source, last_dts_at = LastDtsAt, retry_count = Count,
  clients_timeout = ClientsTimeout, last_access_at = LastTouchedAt, clients = Clients} = Stream) ->
  
  erlang:cancel_timer(OldCheckTimer),
  Now = os:timestamp(),
  SourceDelta = timer:now_diff(Now, LastDtsAt) div 1000,
  ClientsDelta = timer:now_diff(Now, LastTouchedAt) div 1000,

  UsingSourceTimeout = lists:max([Count,1])*SourceTimeout,

  % ?D({{source_delta,SourceDelta},{clients_delta,ClientsDelta}}),
  CheckTimer = erlang:send_after(3000, self(), check_timeout),

  if 
  is_number(ClientsTimeout) andalso not Static andalso ClientsDelta >= ClientsTimeout andalso length(Clients) == 0 ->
    ?DBG("Stop stream \"~s\" (url \"~s\"): no clients during timeout: ~p/~p", [Name, URL, ClientsDelta,ClientsTimeout]),
    {stop, normal, Stream};
  is_number(SourceTimeout) andalso SourceDelta >= UsingSourceTimeout andalso is_pid(Source) ->
    erlang:exit(Source, shutdown),
    case will_log_error(Count) of true ->
    ?DBG("stream \"~s\" is killing source \"~s\" because of timeout ~B > ~B", [Name, URL, SourceDelta, Count*SourceTimeout]);
    false -> ok end,
    erlang:exit(Source, kill),
    {noreply, Stream#stream{check_timer = CheckTimer}};
  is_number(SourceTimeout) andalso SourceDelta >= SourceTimeout andalso not Static ->
    ?D({stop_stream,Name,source_timeout,SourceDelta,SourceTimeout}),
    {stop, normal, Stream};
  URL == undefined andalso Source == undefined andalso not Static ->
    ?D({no_url,no_source, Name, stopping}),
    {stop, normal, Stream};  
  true ->  
    {noreply, Stream#stream{check_timer = CheckTimer}}
  end;

handle_info(reload_playlist, #stream{source = Source} = Stream) ->
  Source ! reload_playlist,
  {noreply, Stream};

handle_info({'DOWN', _, process, Pid, _Reason} = Message, #stream{clients = Clients} = Stream) ->
  Stream1 = case lists:keyfind(Pid, 1, Clients) of
    false ->
      pass_message(Message, Stream);
    _ ->
      Clients1 = [Client || {CPid,_Ref} = Client <- Clients, CPid =/= Pid],
      Stream#stream{clients = Clients1}
  end,
  {noreply, Stream1};

handle_info(#video_frame{} = Frame, #stream{} = Stream) ->
  {noreply, Stream1} = handle_input_frame(Frame, Stream),
  {noreply, Stream1};


% handle_info(next_second, #stream{last_dts = DTS, ts_delta = Delta} = Stream) when DTS =/= undefined andalso Delta =/= undefined ->
%   {_, _, Microsecond} = Now = erlang:now(),
%   {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Now),
%   Millisecond = Microsecond div 1000,
%   SD = io_lib:format("~2.. B-~2.. B-~4.. B", [Day, Month, Year]),
%   ST = io_lib:format("~2.. B:~2.. B:~2.. B:~3.. B", [Hour, Minute, Second, Millisecond]),
%   Metadata = #video_frame{dts = DTS - Delta, pts = DTS - Delta, content = metadata, body = [
%     <<"onFI">>, [{sd, iolist_to_binary(SD)}, {st, iolist_to_binary(ST)}]
%   ]},
%   handle_info(Metadata, Stream);

handle_info(Message, #stream{} = Stream) ->
  Stream1 = pass_message(Message, Stream),
  {noreply, Stream1}.











handle_input_frame(#video_frame{} = Frame, #stream{retry_count = Count, name = Name} = Stream) when Count > 0 ->
  gen_tracker:setattr(flu_streams, Name, [{retry_count,0}]),
  handle_input_frame(Frame, Stream#stream{retry_count = 0});
  
handle_input_frame(#video_frame{} = Frame, #stream{name = Name, dump_frames = Dump, clients = Clients} = Stream) ->
  case Dump of
    true -> ?D({frame, Name, Frame#video_frame.codec, Frame#video_frame.flavor, Frame#video_frame.track_id, round(Frame#video_frame.dts), round(Frame#video_frame.pts)});
    _ -> ok
  end,
  {reply, Frame1, Stream2} = flu_stream_frame:handle_frame(Frame, Stream),
  Stream3 = pass_message(Frame1, Stream2),
  Frame2 = Frame1#video_frame{stream_id = self()},
  
  [Pid ! Frame2 || {Pid, _} <- Clients],
  set_last_dts(Stream3#stream.last_dts, Stream3#stream.last_dts_at),
  {noreply, Stream3}.









will_log_error(Count) ->
  Count =< 10 orelse 
  (Count < 500 andalso Count div 10 == 0) orelse
  Count rem 100 == 0.


pass_message(Message, Stream) ->
  try pass_message0(Message, Stream)
  catch
    Class:Error ->
      ?DBG("Failed to pass message ~p: ~p:~p~n~p", [Message, Class, Error, erlang:get_stacktrace()]),
      erlang:raise(Class, Error, erlang:get_stacktrace())
  end.

pass_message0(Message, #stream{hls = {HLSMod, HLS}, hds = {HDSMod, HDS}, udp = {UDPMod, UDP}} = Stream) ->
  {noreply, HLS1} = HLSMod:handle_info(Message, HLS),
  {noreply, HDS1} = HDSMod:handle_info(Message, HDS),
  {noreply, UDP1} = UDPMod:handle_info(Message, UDP),
  Stream#stream{hls = {HLSMod, HLS1}, hds = {HDSMod, HDS1}, udp = {UDPMod, UDP1}}.

detect_proto(<<"file://", Path/binary>>) ->
  {file, Path};

detect_proto(URL) ->
  {Proto, _Auth, _Host, _Port, _Path, _} = http_uri2:parse(URL),
  {Proto, URL}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

  

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
