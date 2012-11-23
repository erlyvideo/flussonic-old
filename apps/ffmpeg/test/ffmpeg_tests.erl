-module(ffmpeg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-compile(export_all).

launch() ->
  case gen_tcp:connect("127.0.0.1", 9000, [binary]) of
    {error, _} -> ffmpeg:start_worker(filename:join("../priv", "flussonic_ffmpeg"));
    {ok, S} ->
      gen_tcp:close(S),
      ffmpeg:start_worker("127.0.0.1:9000")
  end.


ffmpeg_test_() ->
  TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
  lists:prefix("test_", atom_to_list(F))],
  [{atom_to_list(F), {spawn, [fun ?MODULE:F/0]}} || F <- TestFunctions].


test_non_tuple_command() ->
  Port = launch(),
  ffmpeg:send(Port, [hi]),
  ?assertEqual({error, "must pass tuple"}, ffmpeg:fetch(Port)),
  ok.


test_ffmpeg_ping_pong() ->
  Port = launch(),
  ffmpeg:send(Port, {ping}),
  ?assertEqual(pong, ffmpeg:fetch(Port)),
  ok.

test_ffmpeg_unknow_command() ->
  Port = launch(),
  ffmpeg:send(Port, {some_command, args}),
  ?assertMatch({error, "Unknown command" ++_}, ffmpeg:fetch(Port)),
  ok.

test_ffmpeg_exit() ->
  Port = launch(),
  ffmpeg:send(Port, {exit}),
  ?assertEqual(closed, ffmpeg:fetch(Port)),
  ok.


test_init_decoder() ->
  Port = launch(),
  MediaInfo = media_info(),
  ?assertEqual(ok, ffmpeg:init_decoder(Port, MediaInfo)),
  ok.

test_init_encoder() ->
  Port = launch(),
  MediaInfo = #media_info{streams = [
    #stream_info{track_id = 1, content = video, codec = h264, bitrate = 900000, params = #video_params{width = 240, height = 160}, options = []}
    ,#stream_info{track_id = 2, content = audio, codec = aac, bitrate = 64000, params = #audio_params{sample_rate = 44100, channels = 2}}
  ]},
  ffmpeg:init_decoder(Port, media_info()),
  Result = ffmpeg:init_encoder(Port, MediaInfo),
  ?assertMatch({ok, Configs} when length(Configs) == 2, Result),
  
  ?assertMatch({ok, [#video_frame{codec = h264, flavor = config}, #video_frame{codec = aac, flavor = config}|_]}, Result),
  {ok, Configs} = Result,
  #video_frame{body = Config, codec = h264, flavor = config} = hd(Configs),
  ?assertMatch({4, ConfigNals} when is_list(ConfigNals), h264:unpack_config(Config)),
  ok.

test_transcode_audio() ->
  Port = launch(),
  ffmpeg:init_decoder(Port, media_info()),
  Frames = frames(),
  FirstFrames = lists:sublist([F || #video_frame{flavor = frame, codec = aac} = F <- Frames], 1, 5),
  [ffmpeg:send(Port, Frame) || Frame <- FirstFrames],
  ?assertEqual(ok, ffmpeg:fetch(Port)),
  % ?assertMatch(#video_frame{}, Reply),
  ok.




test_transcode_video() ->
  Port = launch(),
  MI = media_info(),
  ffmpeg:init_decoder(Port, MI),
  ffmpeg:init_encoder(Port, MI),
  Frames = 
    lists:sublist(
    [F || #video_frame{flavor = Flavor, codec = h264} = F <- frames(), Flavor == keyframe orelse Flavor == frame],
    1,70),
  Replies = [ffmpeg:send_frame(Port, Frame) || Frame <- Frames],
  [?assertMatch(R when R == ok orelse is_record(R,video_frame), R) || R <- Replies],
  % ?assertMatch(#video_frame{}, ffmpeg:fetch(Port)),
  % [?debugFmt("frame: ~.2f", [PTS*1.0]) || #video_frame{pts = PTS} <- Replies],
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
