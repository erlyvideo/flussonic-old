-module(flu_monotone).

-export([start_link/1]).
-export([subscribe/2, clients_count/1, send_frame/2, send_media_info/2, stop/1]).

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
      ?DBG("stream ~p failed to send frame to monotone", [get(name)]),
      % [io:format("~20.. s: ~p~n", [K,V]) || {K,V} <- process_info(Monotone)],
      {error, timeout}
  end.

send_media_info(undefined, _) -> ok;
send_media_info(Monotone, MediaInfo) -> 
  try gen_server:call(Monotone, MediaInfo, 1000)
  catch
    exit:{timeout, _} ->
      ?DBG("stream ~p failed to send nedia info to monotone", [get(name)]),
      % [io:format("~20.. s: ~p~n", [K,V]) || {K,V} <- process_info(Monotone)],
      {error, timeout}
  end.


stop(Monotone) ->
  gen_server:call(Monotone, stop).


-record(monotone, {
  name,
  stream,
  first_dts,
  start_at,
  timer,
  subscribers = [],
  frames,
  queue_len = 0
}).

init([Name]) ->
  put(name, {flu_monotone,Name}),
  {ok, Stream} = flu_stream:find(Name),
  {ok, #monotone{name = Name, stream = Stream, frames = queue:new()}}.


handle_call({subscribe, Pid}, _From, #monotone{subscribers = S} = M) ->
  erlang:monitor(process, Pid),
  put(clients_count, length(S) + 1),
  {reply, ok, M#monotone{subscribers = [Pid|S]}};

handle_call(clients_count, _From, #monotone{subscribers = S} = M) ->
  {reply, length(S), M};

handle_call(#media_info{} = MI, _From, #monotone{frames = Frames} = M) ->
  {reply, ok, M#monotone{frames = queue:in(MI, Frames)}};

handle_call(#video_frame{} = Frame, From, #monotone{frames = Frames, queue_len = QueueLen} = M) ->
  % M#monotone.queue_len == queue:len(M#monotone.frames) orelse error({broken_queue_len, M#monotone.queue_len, queue:len(M#monotone.frames)}),
  gen_server:reply(From, ok),
  {noreply, handle_frame(M#monotone{frames = queue:in(Frame, Frames), queue_len = QueueLen + 1})};

handle_call(stop, _From, #monotone{} = M) ->
  {stop, normal, ok, M};

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


handle_frame(#monotone{queue_len = 0} = M) ->
  M;

handle_frame(#monotone{first_dts = undefined, frames = Frames} = M) ->
  DTS = case queue:get(Frames) of
    #video_frame{dts = DTS_} -> DTS_;
    #media_info{} -> (queue:get(queue:drop(Frames)))#video_frame.dts
  end,
  handle_frame(M#monotone{first_dts = DTS, start_at = os:timestamp()});

handle_frame(#monotone{frames = Frames, queue_len = QueueLen, name = Name} = M) when QueueLen > 0 ->
  {MI, F, Frames1} = case queue:out(Frames) of
    {{value, #media_info{} = MI_}, Frames1_} ->
      {{value, #video_frame{} = F_}, Frames2_} = queue:out(Frames1_),
      {MI_, F_, Frames2_};
    {{value, #video_frame{} = F_}, Frames1_} ->
      {undefined, F_, Frames1_}
  end,

  Delay = ?MODULE:delay(F, M),

  if Delay < 2 ->
    % ?D({round(F#video_frame.dts - M#monotone.first_dts), Delay, M#monotone.queue_len}),
    M1 = deliver_frame(MI, M),
    M2 = deliver_frame(F, M1#monotone{frames = Frames1, queue_len = QueueLen - 1}),
    handle_frame(M2);
  QueueLen > 1000 andalso Delay > 2000 ->
    #video_frame{dts = DTS} = Last = queue:last(Frames),
    ?DBG("monotone stream repeater ~p flushed queue with ~B frames to dts ~p", [Name, QueueLen, round(DTS)]),
    handle_frame(M#monotone{first_dts = DTS, start_at = os:timestamp(), frames = queue:from_list([Last]), queue_len = 1});
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

deliver_frame(#video_frame{} = Frame, #monotone{subscribers = S, stream = Stream} = M) ->
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



