-module(frame_queue_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").


ordered_frames_test() ->
  Q0 = frame_queue:init(3),
  {undefined, Q1} = frame_queue:push(#video_frame{dts = 1}, Q0),
  {undefined, Q2} = frame_queue:push(#video_frame{dts = 2}, Q1),
  {undefined, Q3} = frame_queue:push(#video_frame{dts = 3}, Q2),
  {#video_frame{dts = 1}, _Q4} = frame_queue:push(#video_frame{dts = 4}, Q3),
  ok.


delayed_frame_test() ->
  Q0 = frame_queue:init(3),
  {undefined, Q1} = frame_queue:push(#video_frame{dts = 2}, Q0),
  {undefined, Q2} = frame_queue:push(#video_frame{dts = 3}, Q1),
  {undefined, Q3} = frame_queue:push(#video_frame{dts = 4}, Q2),
  {#video_frame{dts = 1}, Q3} = frame_queue:push(#video_frame{dts = 1}, Q3),
  ok.


unordered_frames_test() ->
  Q0 = frame_queue:init(3),
  Input = [#video_frame{dts = I} || I <- [3,2,1,6,5,4,9,8,7]],
  {Output, _} = lists:mapfoldl(fun frame_queue:push/2, Q0, Input),
  ?assertMatch([undefined,undefined,undefined,
    #video_frame{dts=1},#video_frame{dts=2},#video_frame{dts=3},
    #video_frame{dts=4},#video_frame{dts=5},#video_frame{dts=6}], Output),
  ok.
