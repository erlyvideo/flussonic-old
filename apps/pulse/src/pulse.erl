-module(pulse).
-include_lib("erlmedia/include/video_frame.hrl").
-include("log.hrl").

-export([json_list/1]).

-export([disk_io/2, segment_io/1, hls_read/3, network_traffic/4]).

json_list(traffic) ->
  Traffic = traffic_collector:stats(),
  [{event, 'pulse.traffic'},{traffic,Traffic}].



hls_read(_Time, _Duration, _Size) ->
  ok.


network_traffic(Time, Iface, Ibytes, Obytes) ->
  ets:insert(pulse_traffic, {{Iface, Time}, Ibytes, Obytes}),
  ok.


disk_io(_Path, Fun) ->
  T1 = os:timestamp(),
  case Fun() of
    % {ok, [#video_frame{}|_] = Frames} ->
    %   {Size,Duration} = frames_metrics(Frames),
    {ok, Frames} ->
      _Size = erlang:external_size(Frames),
      T3 = os:timestamp(),
      _ReadTime = timer:now_diff(T3,T1),

      % lager:info("Read ~B bytes in ~B us", [Size, ReadTime]),
      {ok, Frames};
    Else ->
      Else
  end.

% frames_metrics([#video_frame{dts = DTS}|_] = Frames) ->
%   frames_metrics(Frames, DTS, DTS, 0).


% frames_metrics([#video_frame{body = Body, dts = DTS}|Frames], StartDTS, _, Size) ->
%   frames_metrics(Frames, StartDTS, DTS, Size + iolist_size(Body));

% frames_metrics([], StartDTS, LastDTS, Size) ->
%   {Size, round((LastDTS - StartDTS)*1000)}.


segment_io(Fun) ->
  T1 = os:timestamp(),
  case Fun() of
    {ok, IO} ->
      T2 = os:timestamp(),
      _Size = iolist_size(IO),
      _Time = timer:now_diff(T2,T1),
      % lager:info("Get ~B bytes in ~B us", [Size, Time]),
      {ok, IO};
    Else ->
      Else
  end.
