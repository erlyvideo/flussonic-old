-module(ffmpeg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).

launch() ->
  % ffmpeg:start_worker(filename:join("../priv", "flussonic_ffmpeg")).
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

init_encoder_test5() ->
  Port = launch(),
  MediaInfo = #media_info{streams = [
    #stream_info{track_id = 1, content = video, codec = h264, bitrate = 900000, params = #video_params{width = 240, height = 160}, options = []}
    ,#stream_info{track_id = 2, content = audio, codec = aac, options = []}
  ]},
  ?assertEqual(ok, ffmpeg:init_encoder(Port, MediaInfo)),
  ffmpeg:close(Port),
  ok.

% transcode_audio_test() ->
%   Port = launch(),
%   ffmpeg:init_decoder(Port, media_info()),
%   Frames = frames(),
%   FirstFrames = lists:sublist([F || #video_frame{flavor = frame, codec = aac} = F <- Frames], 1, 5),
%   [ffmpeg:send(Port, Frame) || Frame <- FirstFrames],
%   ?assertEqual(ok, ffmpeg:fetch(Port)),
%   % ?assertMatch(#video_frame{}, Reply),
%   ffmpeg:close(Port),
%   ok.


valid_h264_config_test() ->
  Port = launch(),
  ffmpeg:init_decoder(Port, media_info()),
  Frames = 
    lists:sublist(
    [F || #video_frame{flavor = Flavor, codec = h264} = F <- frames(), Flavor == keyframe orelse Flavor == frame],
    1,70),
  Replies = [R || #video_frame{} = R <- [ffmpeg:send_frame(Port, Frame) || Frame <- Frames]],
  ffmpeg:close(Port),
  ?assertMatch([#video_frame{codec = h264, flavor = config}, #video_frame{codec = h264, flavor = keyframe}|_], Replies),
  #video_frame{body = Config, flavor = config} = hd(Replies),
  ?assertMatch({4, ConfigNals} when is_list(ConfigNals), h264:unpack_config(Config)),

  ok.



transcode_video_test() ->
  Port = launch(),
  ffmpeg:init_decoder(Port, media_info()),
  Frames = 
    lists:sublist(
    [F || #video_frame{flavor = Flavor, codec = h264} = F <- frames(), Flavor == keyframe orelse Flavor == frame],
    1,70),
  Replies = [ffmpeg:send_frame(Port, Frame) || Frame <- Frames],
  [?assertMatch(R when R == ok orelse is_record(R,video_frame), R) || R <- Replies],
  % ?assertMatch(#video_frame{}, ffmpeg:fetch(Port)),
  % [?debugFmt("frame: ~.2f", [PTS*1.0]) || #video_frame{pts = PTS} <- Replies],
  ffmpeg:close(Port),

  % file:write_file("../a.flv", [flv:header(), [flv_video_frame:to_tag(F) || #video_frame{} = F <- Replies]]),
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
