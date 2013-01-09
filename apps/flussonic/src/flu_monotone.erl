-module(flu_monotone).

-export([start_link/1]).
-export([subscribe/2, clients_count/1, send_frame/2]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([delay/2]).
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").



start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

subscribe(Monotone, Pid) ->
  gen_server:call(Monotone, {subscribe, Pid}).

clients_count(undefined) -> 0;
clients_count(Monotone) -> gen_server:call(Monotone, clients_count).


send_frame(undefined, _) -> ok;
send_frame(Monotone, Frame) -> gen_server:call(Monotone, Frame).

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
  {reply, ok, M#monotone{subscribers = [Pid|S]}};

handle_call(clients_count, _From, #monotone{subscribers = S} = M) ->
  {reply, length(S), M};

handle_call(#video_frame{} = Frame, From, #monotone{frames = Frames, queue_len = QueueLen} = M) ->
  gen_server:reply(From, ok),
  {noreply, handle_frame(M#monotone{frames = queue:in(Frame, Frames), queue_len = QueueLen + 1})};

handle_call(Call, _From, #monotone{} = M) ->
  {reply, {error, {bad_call, Call}}, M}.

handle_info(#video_frame{} = Frame, #monotone{frames = Frames, queue_len = QueueLen} = M) ->
  M1 = handle_frame(M#monotone{frames = queue:in(Frame, Frames), queue_len = QueueLen + 1}),
  {noreply, M1};

handle_info({'DOWN', _, process, Pid, _}, #monotone{subscribers = S} = M) ->
  {noreply, M#monotone{subscribers = lists:delete(Pid, S)}};

handle_info(next_frame, #monotone{} = M) ->
  {noreply, handle_frame(M)};

handle_info(Msg, #monotone{} = M) ->
  ?D(Msg),
  {noreply, M}.


terminate(_,_) ->
  ok.


handle_frame(#monotone{queue_len = 0} = M) ->
  M;

handle_frame(#monotone{first_dts = undefined, frames = Frames} = M) ->
  #video_frame{dts = DTS} = queue:get(Frames),
  handle_frame(M#monotone{first_dts = DTS, start_at = os:timestamp()});

handle_frame(#monotone{queue_len = BigQueueLen, frames = Frames} = M) when BigQueueLen > 100 ->
  #video_frame{dts = DTS} = queue:get(Frames),
  handle_frame(M#monotone{first_dts = DTS, start_at = os:timestamp()});

handle_frame(#monotone{frames = Frames, queue_len = QueueLen} = M) ->
  {{value, #video_frame{} = F}, Frames1} = queue:out(Frames),
  Delay = ?MODULE:delay(F, M),

  if Delay < 2 ->
    % ?D({round(F#video_frame.dts - M#monotone.first_dts), Delay, M#monotone.queue_len}),
    M1 = deliver_frame(F, M#monotone{frames = Frames1, queue_len = QueueLen - 1}),
    handle_frame(M1);
  true ->
    schedule_timer(Delay, M)
  end.


delay(#video_frame{dts = DTS}, #monotone{first_dts = FirstDTS, start_at = StartAt}) ->
  round(DTS - FirstDTS) - (timer:now_diff(os:timestamp(), StartAt) div 1000).



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



