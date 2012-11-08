-module(ffmpeg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).

launch() ->
  % ffmpeg:start_worker(filename:join("../priv", "ffmpeg")).
  ffmpeg:start_worker("127.0.0.1:9000").


non_tuple_command_test() ->
  Port = launch(),
  ffmpeg:send(Port, [hi]),
  ?assertEqual({error, "must pass tuple"}, ffmpeg:fetch(Port)),
  ffmpeg:close(Port),
  ok.


ffmpeg_ping_pong_test() ->
  Port = launch(),
  ffmpeg:send(Port, {ping}),
  ?assertEqual(pong, ffmpeg:fetch(Port)),
  ffmpeg:close(Port),
  ok.

ffmpeg_unknow_command_test() ->
  Port = launch(),
  ffmpeg:send(Port, {some_command, args}),
  ?assertMatch({error, "Unknown command" ++_}, ffmpeg:fetch(Port)),
  ffmpeg:close(Port),
  ok.

ffmpeg_exit_test() ->
  Port = launch(),
  ffmpeg:send(Port, {exit}),
  ?assertEqual(closed, ffmpeg:fetch(Port)),
  ffmpeg:close(Port),
  ok.


init_decoder_test() ->
  Port = launch(),
  MediaInfo = media_info(),
  ?assertEqual(ok, ffmpeg:init_decoder(Port, MediaInfo)),
  ffmpeg:close(Port),
  ok.


transcode_audio_test() ->
  Port = launch(),
  ffmpeg:init_decoder(Port, media_info()),
  Frames = frames(),
  FirstFrames = lists:sublist([F || #video_frame{flavor = frame, codec = aac} = F <- Frames], 1, 5),
  [ffmpeg:send(Port, Frame) || Frame <- FirstFrames],
  ?assertEqual(ok, ffmpeg:fetch(Port)),
  % ?assertMatch(#video_frame{}, Reply),
  ffmpeg:close(Port),
  ok.



transcode_video_test() ->
  Port = launch(),
  ffmpeg:init_decoder(Port, media_info()),
  Frames = frames(),
  FirstFrames = lists:sublist(
    [F || #video_frame{flavor = Flavor, codec = h264} = F <- Frames, Flavor == keyframe orelse Flavor == frame], 1, 5),
  [ffmpeg:send(Port, Frame) || Frame <- FirstFrames],
  ?assertEqual(ok, ffmpeg:fetch(Port)),
  % ?assertMatch(#video_frame{}, ffmpeg:fetch(Port)),
  ffmpeg:close(Port),
  ok.


media_info() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  mp4_reader:media_info(R).  

frames() ->
  {ok, F} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,F},[]),
  Frames = read_frames(R, undefined),
  Frames.

read_frames(R, Key) ->
  case mp4_reader:read_frame(R, Key) of
    #video_frame{next_id = Next} = F ->
      [F|read_frames(R, Next)];
    eof ->
      []
  end.





test_init(Port) ->
  erlang:port_command(Port, term_to_binary({video_frame, h264, keyframe, 40.0, 40.0, <<"hi">>})),
  receive
    H -> ?debugFmt("msg: ~p", [H])
  after
    1000 -> error(init_timeout)
  end,
  ok.
