-module(flu_monotone).

-export([start_link/2]).
-export([subscribe/2, clients_count/1, send_frame/2, send_media_info/2, set_current_dts/2, stop/1]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([delay/2]).
-export([add_client/4]).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").


subscribe(Monotone, Pid) ->
  add_client(Monotone, Pid, raw, undefined).

add_client(Monotone, Pid, Proto, Socket) when
  Proto == rtmp orelse Proto == tcp_mpegts orelse Proto == chunked_mpegts orelse Proto == udp_mpegts orelse Proto == raw ->
  gen_server:call(Monotone, {add_client, Pid, Proto, Socket}).

start_link(Name, Options) ->
  gen_server:start_link(?MODULE, [Name, Options], []).


clients_count(undefined) -> 0;
clients_count(Monotone) ->
  case process_info(Monotone, dictionary) of
    {dictionary, Dict} -> proplists:get_value(clients_count, Dict, 0);
    undefined -> 0
  end.


send_frame(undefined, _) -> ok;
send_frame(Monotone, Frame) ->
  case erlang:process_info(Monotone, message_queue_len) of
    {message_queue_len, Len} when Len > 1000 ->
      {error, busy};
    undefined ->
      {error, noproc};
    {message_queue_len, _} ->
      Monotone ! Frame,
      ok
  end.

set_current_dts(Pid, DTS) -> 
  ok = gen_server:call(Pid, {set_current_dts, DTS}).

send_media_info(undefined, _) -> ok;
send_media_info(_, undefined) -> ok;
send_media_info(Monotone, MediaInfo) -> 
  try gen_server:call(Monotone, MediaInfo, 1000)
  catch
    exit:{timeout, _} ->
      lager:error("stream ~p failed to send nedia info to monotone", [get(name)]),
      % [io:format("~20.. s: ~p~n", [K,V]) || {K,V} <- process_info(Monotone)],
      {error, timeout}
  end.


stop(Monotone) ->
  gen_server:call(Monotone, stop).


-record(monotone, {
  name,
  stream,
  media_info,
  current_dts,
  first_dts,
  start_at,
  timer,
  prepush,
  waiting = [],
  send_mpegts = [],
  mpegts,
  rtmp = [],
  raw = [],
  frames,
  queue_len = 0
}).

-record(client, {
  pid,
  ref,
  proto,
  start_dts,
  socket
}).


init([Name, Options]) ->
  put(name, {flu_monotone,Name}),
  {ok, Stream} = flu_stream:find(Name),
  put(clients_count,0),
  Prepush = case proplists:get_value(prepush, Options) of
    false -> undefined;
    _ -> []
  end,
  {ok, #monotone{name = Name, stream = Stream, prepush = Prepush, frames = queue:new()}}.


handle_call({add_client, Pid, Proto, Socket}, _From, #monotone{waiting = W, prepush = Prepush, 
  raw = Raw, rtmp = RTMP, media_info = MI} = Monotone) ->
  Ref = erlang:monitor(process, Pid),
  Client = #client{pid = Pid, ref = Ref, proto = Proto, socket = Socket},

  DTS = case Prepush of
    undefined -> Monotone#monotone.current_dts;
    [] -> Monotone#monotone.current_dts;
    _ -> (lists:last(Prepush))#video_frame.dts
  end,

  put(clients_count, get(clients_count) + 1),
  case MI of
    [#stream_info{content = audio}] ->
      {ok, Reply, Monotone1} = add_client_to_list(Monotone, Client),
      {reply, Reply, Monotone1};
    _ when Proto == raw andalso length(Prepush) >= 0 ->
      [Pid ! C || C <- configs(Monotone#monotone{current_dts = DTS})],
      [Pid ! Frame || Frame <- lists:reverse(Prepush)],
      {reply, ok, Monotone#monotone{raw = [Client|Raw]}};
    _ when Proto == rtmp andalso length(Prepush) >= 0 ->
      Configs = configs(Monotone#monotone{current_dts = 0}),
      RTMPConfigs = [(flv:rtmp_tag_generator(C))(0, 1) || C <- Configs],
      (catch port_command(Socket, RTMPConfigs, [nosuspend])),
      [begin
        RTMPFrame = flv:rtmp_tag_generator(Frame),
        catch port_command(Socket, RTMPFrame(DTS,1), [nosuspend])
      end || Frame <- lists:reverse(Prepush)],
      {reply, ok, Monotone#monotone{rtmp = [Client#client{start_dts = DTS}|RTMP]}};
    _ ->
      % ?D({add_client,Proto}),
      {reply, ok, Monotone#monotone{waiting = [Client|W]}}
  end;


handle_call(#media_info{} = MI, _From, #monotone{frames = Frames} = M) ->
  {reply, ok, M#monotone{frames = queue:in(MI, Frames), media_info = MI}};

handle_call(#video_frame{} = Frame, From, #monotone{} = M) ->
  gen_server:reply(From, ok),
  {noreply, M1} = handle_info(Frame, M),
  {noreply, M1};

handle_call({set_current_dts, DTS}, _From, #monotone{current_dts = undefined} = M) ->
  {reply, ok, M#monotone{current_dts = DTS}};


handle_call({set_start_at, StartAt}, _From, #monotone{} = M) -> % This call is for tests, because mocking delay is slow
  {reply, ok, M#monotone{start_at = StartAt}};

handle_call(stop, _From, #monotone{} = M) ->
  {stop, normal, ok, M};

handle_call(media_info, _From, #monotone{media_info = MI} = M) ->
  {reply, MI, M};


handle_call(Call, _From, #monotone{} = M) ->
  {reply, {error, {bad_call, Call}}, M}.




handle_info(#video_frame{} = Frame, #monotone{frames = Frames, stream = Stream, queue_len = QueueLen} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  M1 = handle_frame(M#monotone{frames = queue:in(Frame#video_frame{stream_id = Stream}, Frames), queue_len = QueueLen + 1}),
  {noreply, M1};

handle_info({'DOWN', _, process, Pid, _}, #monotone{} = M) ->
  M1 = delete_client(M, Pid),
  put(clients_count, get(clients_count) - 1),
  {noreply, M1};

handle_info(next_frame, #monotone{} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  {noreply, handle_frame(M)};

handle_info({inet_reply, _, _}, #monotone{} = M) ->
  {noreply, M};

handle_info(Msg, #monotone{} = M) ->
  ?D(Msg),
  {noreply, M}.


terminate(_,_) ->
  ok.


delete_client(#monotone{raw = Raw, waiting = Waiting, send_mpegts = Mpeg} = M, Pid) ->
  M#monotone{
    raw = lists:keydelete(Pid, #client.pid, Raw),
    waiting = lists:keydelete(Pid, #client.pid, Waiting),
    send_mpegts = lists:keydelete(Pid, #client.pid, Mpeg)
  }.



add_client_to_list(#monotone{raw = Raw} = M, #client{proto = raw, pid = Pid} = Client) ->
  [Pid ! C || C <- configs(M)],
  {ok, ok, M#monotone{raw = [Client|Raw]}};

add_client_to_list(#monotone{send_mpegts = UDP} = M, #client{proto = P} = Client) when 
  P == udp_mpegts; P == tcp_mpegts; P == chunked_mpegts ->
  {ok, ok, M#monotone{send_mpegts = [Client|UDP]}};

add_client_to_list(#monotone{} = M, #client{} = _Client) ->
  {ok, {error, not_implemented}, M}.


first_dts(Frames) ->
  case queue:get(Frames) of
    #video_frame{dts = DTS} -> DTS;
    #media_info{} -> first_dts(queue:drop(Frames))
  end.

fetch_frame(Frames) -> fetch_frame(Frames, undefined).

fetch_frame(Frames, MI1) ->
  case queue:out(Frames) of
    {{value, #media_info{} = MI2}, Frames1} ->
      fetch_frame(Frames1, MI2);
    {{value, #video_frame{} = Frame}, Frames1} ->
      {MI1, Frame, Frames1}
  end.


handle_frame(#monotone{queue_len = 0} = M) ->
  M;

handle_frame(#monotone{first_dts = undefined, frames = Frames, start_at = OldStart} = M) ->
  DTS = first_dts(Frames),
  StartAt = case OldStart of
    {0,0,0} -> OldStart;
    _ -> os:timestamp()
  end,
  handle_frame(M#monotone{first_dts = DTS, current_dts = DTS, start_at = StartAt});

handle_frame(#monotone{frames = Frames, queue_len = QueueLen, name = Name} = M) when QueueLen > 0 ->
  {MI, F, Frames1} = fetch_frame(Frames),

  Delay = ?MODULE:delay(F, M),

  if Delay < 2 ->
    % ?D({round(F#video_frame.dts - M#monotone.first_dts), Delay, M#monotone.queue_len}),
    M1 = deliver_frame(MI, M),
    M2 = deliver_frame(F, M1#monotone{frames = Frames1, current_dts = F#video_frame.dts, queue_len = QueueLen - 1}),
    #monotone{prepush = Prepush1} = M2,
    Prepush2 = if
      Prepush1 == undefined -> undefined;
      F == undefined -> Prepush1;
      F#video_frame.flavor == keyframe -> [F];
      length(Prepush1) > 500 -> [F];
      true -> [F|Prepush1]
    end,
    handle_frame(M2#monotone{prepush = Prepush2});
  QueueLen > 1000 andalso Delay > 2000 ->
    #video_frame{dts = DTS} = Last = queue:last(Frames),
    lager:warning("monotone stream repeater ~p flushed queue with ~B frames to dts ~p", [Name, QueueLen, round(DTS)]),
    handle_frame(M#monotone{first_dts = DTS, current_dts = DTS, start_at = os:timestamp(), frames = queue:from_list([Last]), queue_len = 1});
  true ->
    schedule_timer(Delay, M)
  end.


delay(#video_frame{dts = DTS}, #monotone{first_dts = FirstDTS, start_at = StartAt}) ->
  round(DTS - FirstDTS) - (timer:now_diff(os:timestamp(), StartAt) div 1000).



deliver_frame(undefined, #monotone{} = M) ->
  M;

deliver_frame(#media_info{} = MI, #monotone{raw = S, mpegts = Mpegts1, send_mpegts = TCP} = M) ->
  [Pid ! MI || #client{pid = Pid} <- S],
  Mpegts2 = send_mpegts(MI, MI, Mpegts1, TCP),
  M#monotone{mpegts = Mpegts2};

deliver_frame(#video_frame{flavor = keyframe, dts = DTS} = Frame, #monotone{waiting = W} = Monotone) when W =/= [] ->
  Monotone1 = start_waiting_clients(Monotone, DTS),
  deliver_frame(Frame, Monotone1);

deliver_frame(#video_frame{} = Frame, #monotone{raw  = S, rtmp = RTMP, send_mpegts = TCP,
  mpegts = Mpegts1, media_info = MI} = M) ->
  % ?debugFmt("deliver ~p ~p to ~p", [M#monotone.name, round(Frame#video_frame.dts), S]),
  [Pid ! Frame || #client{pid = Pid} <- S],

  Mpegts2 = send_mpegts(MI, Frame, Mpegts1, TCP),

  RTMPFrame = flv:rtmp_tag_generator(Frame),
  [(catch port_command(Socket, RTMPFrame(StartDTS,1), [nosuspend])) || #client{socket = Socket, start_dts = StartDTS} <- RTMP],
  M#monotone{mpegts = Mpegts2}.


start_waiting_clients(#monotone{waiting = W} = Monotone, DTS) ->
  Configs = configs(Monotone#monotone{current_dts = 0}),
  RTMPConfigs = [(flv:rtmp_tag_generator(C))(0, 1) || C <- Configs],
  Monotone1 = lists:foldl(fun
    (#client{proto = raw, pid = Pid} = C, #monotone{raw = Raw} = M) ->
      [Pid ! F#video_frame{dts = DTS, pts = DTS} || F <- Configs],
      M#monotone{raw = [C|Raw]};
    (#client{proto = rtmp, socket = Socket} = C, #monotone{rtmp = RTMP} = M) ->
      (catch port_command(Socket, RTMPConfigs, [nosuspend])),
      M#monotone{rtmp = [C#client{start_dts = DTS}|RTMP]};
    (#client{proto = tcp_mpegts} = C, #monotone{send_mpegts = MPEG} = M) ->
      M#monotone{send_mpegts = [C|MPEG]};
    (#client{proto = udp_mpegts} = C, #monotone{send_mpegts = MPEG} = M) ->
      M#monotone{send_mpegts = [C|MPEG]};
    (#client{proto = chunked_mpegts} = C, #monotone{send_mpegts = MPEG} = M) ->
      M#monotone{send_mpegts = [C|MPEG]};
    (#client{proto = Proto} = _C, #monotone{} = M) ->
      lager:info("Proto ~p is not supported yet", [Proto]),
      M
  end, Monotone, W),
  % ?D({start_clients,W}),
  Monotone1#monotone{waiting = []}.



send_mpegts(_MI, _Frame, Mpegts, []) ->
  Mpegts;

send_mpegts(MI, Frame, undefined, TCP) ->
  Mpegts1_ = mpegts:init([{resync_on_keyframe,true}]),
  Mpegts2_ = send_mpegts(MI, MI, Mpegts1_, TCP),
  send_mpegts(MI, Frame, Mpegts2_, TCP);

send_mpegts(_MI, Frame, Mpegts1, TCP) ->
  {Mpegts3_, Data2} = mpegts:encode(Mpegts1, Frame),
  case iolist_size(Data2) > 0 of
    true ->
    [(catch port_command(Socket, Data2, [nosuspend])) || #client{socket = Socket, proto = tcp_mpegts} <- TCP],
    Chunk = [io_lib:format("~.16. B\r\n", [iolist_size(Data2)]),Data2,"\r\n"],
    [(catch port_command(Socket, Chunk, [nosuspend])) || #client{socket = Socket, proto = chunked_mpegts} <- TCP],
    ok;
  false -> ok end,
  Mpegts3_.





schedule_timer(Delay, #monotone{} = M) ->
  M1 = cancel_timer(M),
  M1#monotone{timer = erlang:send_after(Delay, self(), next_frame)}.


cancel_timer(#monotone{timer = undefined} = M) -> M;
cancel_timer(#monotone{timer = T} = M) ->
  erlang:cancel_timer(T),
  flush_tick(),
  M#monotone{timer = undefined}.



flush_tick() ->
  receive
    next_frame -> flush_tick()
  after
    0 -> ok
  end.


configs(#monotone{media_info = undefined}) ->
  [];
  
configs(#monotone{current_dts = DTS, media_info = MediaInfo, stream = Stream}) when is_number(DTS) ->
  [F#video_frame{dts = DTS, pts = DTS, stream_id = Stream} || F <- video_frame:config_frames(MediaInfo)];

configs(#monotone{current_dts = undefined, media_info = MediaInfo, stream = Stream}) ->
  [F#video_frame{stream_id = Stream} || F <- video_frame:config_frames(MediaInfo)].










