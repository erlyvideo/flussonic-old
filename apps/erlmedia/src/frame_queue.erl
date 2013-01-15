-module(frame_queue).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/video_frame.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/1, push/2]).


-record(frame_queue, {
  length,
  capacity,
  first_dts,
  frames = []
}).

init(Capacity) ->
  #frame_queue{length = 0, capacity = Capacity,frames = []}.


push(#video_frame{dts = DTS} = F, #frame_queue{length = 0, frames = []} = Q) ->
  {undefined, Q#frame_queue{length = 1, first_dts = DTS, frames = [F]}};

push(#video_frame{dts = DTS1} = F, #frame_queue{length = L, capacity = C, frames = Frames, first_dts = DTS2} = Q) when L < C ->
  {undefined, Q#frame_queue{length = L+1, first_dts = lists:min([DTS1,DTS2]), frames = video_frame:sort([F|Frames])}};

push(#video_frame{dts = DTS} = F, #frame_queue{length = C, capacity = C, first_dts = FDTS} = Q) when DTS < FDTS ->
  {F, Q};

push(#video_frame{} = F, #frame_queue{length = C, capacity = C, frames = Frames} = Q) ->
  [Out|[#video_frame{dts = DTS}|_] = Frames1] = video_frame:sort([F|Frames]),
  {Out, Q#frame_queue{frames = Frames1, first_dts = DTS}}.

