-module(pulse).
-include_lib("erlmedia/include/video_frame.hrl").
-include("log.hrl").

-export([json_list/0, segment_info/0]).

-export([read_segment/4, hls_read/3, network_traffic/4]).
-export([disk_read/2]).

json_list() ->
  Traffic = traffic_collector:stats(),
  [{event, 'pulse.traffic'},{interfaces,Traffic},{file,segment_info()}].



hls_read(_Time, _Duration, _Size) ->
  ok.


network_traffic(Time, Iface, Ibytes, Obytes) ->
  ets:insert(pulse_traffic_min, {{Iface, Time}, Ibytes, Obytes}),
  ok.



current_time() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega*1000000 + Sec.

read_segment(Size, Duration, ReadTime, SegmentTime) ->
  catch read_segment0(Size, Duration, ReadTime, SegmentTime).


read_segment0(Size, Duration, ReadTime, SegmentTime) ->
  T = current_time(),

  ets:insert_new(pulse_file_min, {T, 0, 0, 0, 0}),
  ets:update_counter(pulse_file_min, T, [{2,ReadTime},{3,SegmentTime},{4,Size},{5,Duration}]),

  % It is 30%, because Duration in ms and SegmentTime is us
  if SegmentTime > 300*Duration ->
    ets:insert_new(pulse_file_timeouts, {T, 0}),
    ets:update_counter(pulse_file_timeouts, T, 1);
  true -> ok end,

  ok.

segment_info() ->
  [{minute,segment_info(pulse_file_min)},{hour,segment_info(pulse_file_hour)}].

div_(_,0) -> 0;
div_(Bytes, Time) -> Bytes div Time.

segment_info(Table) ->
  [ [{time,T},{disk,div_(Disk,Duration*10)},{segment,div_(Segment,Duration*10)},{size,Size}] 
  || {T,Disk,Segment,Size,Duration} <- lists:sort(ets:tab2list(Table))].


disk_read(_Path, Fun) ->
  Fun().
