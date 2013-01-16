-module(flu_monotone).

-export([start_link/1]).
-export([subscribe/2, clients_count/1, send_frame/2, send_media_info/2, set_current_dts/2, stop/1]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([delay/2]).
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").



start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

subscribe(Monotone, Pid) ->
  gen_server:call(Monotone, {subscribe, Pid}).

clients_count(undefined) -> 0;
clients_count(Monotone) ->
  case process_info(Monotone, dictionary) of
    {dictionary, Dict} -> proplists:get_value(clients_count, Dict, 0);
    undefined -> 0
  end.


send_frame(undefined, _) -> ok;
send_frame(Monotone, Frame) -> 
  try gen_server:call(Monotone, Frame, 1000)
  catch
    exit:{timeout, _} ->
      lager:error("stream ~p failed to send frame to monotone", [get(name)]),
      % [io:format("~20.. s: ~p~n", [K,V]) || {K,V} <- process_info(Monotone)],
      {error, timeout}
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
  subscribers = [],
  waiting = [],
  frames,
  queue_len = 0
}).

init([Name]) ->
  put(name, {flu_monotone,Name}),
  {ok, Stream} = flu_stream:find(Name),
  {ok, #monotone{name = Name, stream = Stream, frames = queue:new()}}.


handle_call({subscribe, Pid}, _From, #monotone{subscribers = S, stream = Stream, waiting = W, media_info = MI} = M) ->
  erlang:monitor(process, Pid),
  M1 = case MI of
    [#stream_info{content = audio}] ->
      [Pid ! C#video_frame{stream_id = Stream} || C <- configs(M)],
      M#monotone{subscribers = [Pid|S]};
    _ ->
      M#monotone{waiting = [Pid|W]}
  end,
  put(clients_count, length(S) + 1),
  {reply, ok, M1};

handle_call(clients_count, _From, #monotone{subscribers = S} = M) ->
  {reply, length(S), M};

handle_call(#media_info{} = MI, _From, #monotone{frames = Frames} = M) ->
  {reply, ok, M#monotone{frames = queue:in(MI, Frames), media_info = MI}};

handle_call(#video_frame{} = Frame, From, #monotone{frames = Frames, queue_len = QueueLen} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  gen_server:reply(From, ok),
  {noreply, handle_frame(M#monotone{frames = queue:in(Frame, Frames), queue_len = QueueLen + 1})};

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




handle_info(#video_frame{} = Frame, #monotone{frames = Frames, queue_len = QueueLen} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  M1 = handle_frame(M#monotone{frames = queue:in(Frame, Frames), queue_len = QueueLen + 1}),
  {noreply, M1};

handle_info({'DOWN', _, process, Pid, _}, #monotone{subscribers = S} = M) ->
  put(clients_count, length(S) - 1),
  {noreply, M#monotone{subscribers = lists:delete(Pid, S)}};

handle_info(next_frame, #monotone{} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  {noreply, handle_frame(M)};

handle_info(Msg, #monotone{} = M) ->
  ?D(Msg),
  {noreply, M}.


terminate(_,_) ->
  ok.



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
    handle_frame(M2);
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

deliver_frame(#media_info{} = MI, #monotone{subscribers = S} = M) ->
  [Pid ! MI || Pid <- S],
  M;

deliver_frame(#video_frame{flavor = keyframe, dts = DTS} = Frame, #monotone{waiting = W, stream = Stream, subscribers = S} = M) when W =/= [] ->
  [Pid ! C#video_frame{stream_id = Stream} || C <- configs(M#monotone{current_dts = DTS}), Pid <- W],
  deliver_frame(Frame, M#monotone{waiting = [], subscribers = W ++ S});

deliver_frame(#video_frame{} = Frame, #monotone{subscribers = S, stream = Stream} = M) ->
  % ?debugFmt("deliver ~p ~p to ~p", [M#monotone.name, round(Frame#video_frame.dts), S]),
  Frame2 = Frame#video_frame{stream_id = Stream},
  [Pid ! Frame2 || Pid <- S],
  M.


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
  
configs(#monotone{current_dts = DTS, media_info = MediaInfo}) when is_number(DTS) ->
  [F#video_frame{dts = DTS, pts = DTS} || F <- video_frame:config_frames(MediaInfo)];

configs(#monotone{current_dts = undefined, media_info = MediaInfo}) ->
  video_frame:config_frames(MediaInfo).










