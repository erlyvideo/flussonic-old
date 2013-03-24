-module(monotone_ticker).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("log.hrl").


-export([start_link/2]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-export([pause/1, resume/1, stop/1]).



start_link(Consumer, {_M,_F,_A} = Reader) ->
  gen_server:start_link(?MODULE, [Consumer, Reader], []).


pause(Ticker) ->
  gen_server:call(Ticker, pause).

resume(Ticker) ->
  gen_server:call(Ticker, resume).

stop(Ticker) ->
  gen_server:call(Ticker, stop).



-record(ticker, {
  consumer,
  reader,
  paused = false,
  start_at,
  first_dts,
  next_id = 1,
  frame,
  frames = []
}).

init([Consumer, {_M,_F,Pid} = Reader]) ->
  erlang:monitor(process, Consumer),
  erlang:monitor(process, Pid),
  put(name, monotone_ticker),
  self() ! tick,
  {ok, #ticker{reader = Reader, consumer = Consumer, start_at = os:timestamp()}}.


handle_call(pause, _From, #ticker{} = Ticker) ->
  {reply, ok, Ticker#ticker{paused = true}};

handle_call(resume, _From, #ticker{} = Ticker) ->
  self() ! tick,
  {reply, ok, Ticker#ticker{paused = false}};

handle_call(Call, _From, #ticker{} = Ticker) ->
  {reply, {error, Call}, Ticker}.



handle_info({'DOWN', _, _, _, _}, #ticker{} = Ticker) ->
  {stop, normal, Ticker};

handle_info(tick, #ticker{paused = true} = Ticker) ->
  {noreply, Ticker};

handle_info(tick, #ticker{first_dts = undefined, consumer = Consumer, start_at = StartAt} = Ticker) ->
  Ticker1 = fetch_frame(Ticker),
  case Ticker1 of
    #ticker{frame = undefined} ->
      {stop, normal, Ticker1};
    #ticker{frame = #video_frame{dts = DTS} = Frame} ->
      flu_stream:send_frame(Consumer, Frame),
      Ticker2 = #ticker{frame = #video_frame{dts = NextDTS}} = fetch_frame(Ticker1),
      Delay = delay(NextDTS, DTS, os:timestamp(), StartAt),
      erlang:send_after(Delay, self(), tick),
      {noreply, Ticker2#ticker{first_dts = DTS}}
  end;

handle_info(tick, #ticker{consumer = Consumer, frame = #video_frame{} = Frame, 
  first_dts = FirstDTS, start_at = StartAt} = Ticker) ->
  flu_stream:send_frame(Consumer, Frame),
  Ticker2 = #ticker{frame = #video_frame{dts = NextDTS}} = fetch_frame(Ticker),
  Delay = delay(NextDTS, FirstDTS, os:timestamp(), StartAt),
  erlang:send_after(Delay, self(), tick),
  {noreply, Ticker2}.


terminate(_,_) ->
  ok.



fetch_frame(#ticker{frames = [#video_frame{next_id = Next} = Frame|Frames]} = Ticker) ->
  Ticker#ticker{frame = Frame, frames = Frames, next_id = Next};

fetch_frame(#ticker{reader = {M,F,A}, next_id = Key} = Ticker) ->
  case M:F(A, Key) of
    #video_frame{next_id = Next} = Frame ->
      Ticker#ticker{frame = Frame, next_id = Next};
    [#video_frame{}|_] = Frames ->
      fetch_frame(Ticker#ticker{frames = Frames});
    {ok, [#video_frame{}|_] = Frames} ->
      fetch_frame(Ticker#ticker{frames = [F_#video_frame{next_id = Key + 1} || F_ <- Frames]});
    [] ->
      throw({stop, normal, Ticker});
    {error, no_segment} ->
      throw({stop, normal, Ticker});
    eof ->
      throw({stop, normal, Ticker})
  end.


delay(DTS, FirstDTS, Now, StartAt) ->
  RealDelay = timer:now_diff(Now, StartAt) div 1000,
  StreamDelay = DTS - FirstDTS,
  Delay = round(StreamDelay - RealDelay),
  if Delay =< 0 -> 0;
    true -> Delay
  end.











