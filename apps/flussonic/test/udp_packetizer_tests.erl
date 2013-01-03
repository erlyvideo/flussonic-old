-module(udp_packetizer_tests).
-compile(export_all).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("eunit/include/eunit.hrl").



udp_packetizer_test_() ->
  {foreach,
  fun() ->
    ok = application:start(ranch),
    ok = application:start(gen_tracker),
    ok = application:start(flussonic),
    gen_tracker_sup:start_tracker(flu_streams)
  end,
  fun(_) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(ranch),
    application:stop(gen_tracker),
    application:stop(flussonic),
    application:stop(inets),
    error_logger:add_report_handler(error_logger_tty_h),
    ok
  end, [
    {"test_packetizer", fun test_packetizer/0}
  ]}.


frames() ->
  {ok, File} = file:open("../../../priv/bunny.mp4", [binary,read,raw]),
  {ok, R} = mp4_reader:init({file,File},[]),
  MI = mp4_reader:media_info(R),
  Configs = video_frame:config_frames(MI),
  Frames1 = read_frames(R, 1),
  Frames2 = lists:flatmap(fun
    (#video_frame{flavor = keyframe} = F) -> [MI]++Configs++[F];
    (#video_frame{} = F) -> [F]
  end, Frames1),
  file:close(File),
  {ok, MI, Frames2}.

read_frames(R, Key) ->
  case mp4_reader:read_gop(R, Key) of
    {ok, Gop} -> Gop ++ read_frames(R, Key + 1);
    {error, _} -> []
  end.

listen_multicast(Port) ->
  {ok,Addr} = inet_parse:address("239.0.0.1"),
  {ok,Sock} = gen_udp:open(Port, [binary,{add_membership,{Addr,{0,0,0,0}}},
    {reuseaddr,true},{multicast_ttl,4},{multicast_loop,true},{active,true},{recbuf,10*1024*1024}]),
  {ok, Sock}.  

test_packetizer() ->
  {ok, _MI, Frames} = frames(),
  Port = crypto:rand_uniform(5000, 10000),
  {ok, Sock} = listen_multicast(Port),
  {ok, S} = flu_stream:autostart(<<"livestream">>, [{udp, "udp://239.0.0.1:"++integer_to_list(Port) },{hds,false},{hls,false}]),
  link(S),
  [S ! F || F <- Frames],

  {ok, _, Frames1} = read_from_udp(Sock),
  Length = length(Frames1),
  ?assertMatch(_ when Length > 200, Length),

  ok.

read_from_udp(Sock) ->
  Packets = fetch_udp(Sock),
  Data = iolist_to_binary(Packets),
  {ok, Frames1} = mpegts_decoder:decode_file(Data),
  gen_udp:close(Sock),
  {ok, Packets, Frames1}.

fetch_udp(Sock) ->
  receive
    {udp, Sock, _, _, Bin} -> [Bin|fetch_udp(Sock)]
  after
    100 -> []
  end.

pids(Packets) ->
  Pids = lists:flatmap(fun(Packet) ->
    [Pid || <<16#47, _:3, Pid:13, _:185/binary>> <= Packet]
  end, Packets),
  lists:usort(Pids).
  


udp_raw_packetizer_test() ->
  {ok, _MI, Frames} = frames(),
  Port = crypto:rand_uniform(5000, 10000),
  {ok, Sock} = listen_multicast(Port),
  {ok, UDP1} = udp_packetizer:init([{name,<<"livestream">>},{udp, "udp://239.0.0.1:"++integer_to_list(Port) }]),
  UDP2 = lists:foldl(fun(F, UDP1_) ->
    {noreply, UDP2_} = udp_packetizer:handle_info(F, UDP1_),
    UDP2_
  end, UDP1, Frames),
  udp_packetizer:terminate(normal, UDP2),
  {ok, Packets, Frames1} = read_from_udp(Sock),
  Length = length(Frames1),
  Pids = pids(Packets),
  ?assert(lists:member(0, Pids)),
  ?assert(lists:member(4095, Pids)),
  ?assertMatch(_ when Length > 200, Length),
  ok.


udp_raw_packetizer_after_first_keyframe_test() ->
  {ok, _MI, Frames} = frames(),
  Port = crypto:rand_uniform(5000, 10000),
  {ok, UDP1} = udp_packetizer:init([{name,<<"livestream">>},{udp, "udp://239.0.0.1:"++integer_to_list(Port) }]),
  UDP2 = lists:foldl(fun(F, UDP1_) ->
    {noreply, UDP2_} = udp_packetizer:handle_info(F, UDP1_),
    UDP2_
  end, UDP1, Frames),

  {ok, Sock} = listen_multicast(Port),
  UDP3 = lists:foldl(fun(F, UDP1_) ->
    {noreply, UDP2_} = udp_packetizer:handle_info(F, UDP1_),
    UDP2_
  end, UDP2, Frames),

  udp_packetizer:terminate(normal, UDP3),
  {ok, Packets, Frames1} = read_from_udp(Sock),
  Length = length(Frames1),
  Pids = pids(Packets),
  ?assert(lists:member(0, Pids)),
  ?assert(lists:member(4095, Pids)),
  ?assertMatch(_ when Length > 200, Length),
  ok.


udp_raw_packetizer_from_middle_of_stream_test() ->
  {ok, _MI, Frames1} = frames(),
  Frames = lists:nthtail(20,Frames1),
  Port = crypto:rand_uniform(5000, 10000),
  {ok, UDP1} = udp_packetizer:init([{name,<<"livestream">>},{udp, "udp://239.0.0.1:"++integer_to_list(Port) }]),
  UDP2 = lists:foldl(fun(F, UDP1_) ->
    {noreply, UDP2_} = udp_packetizer:handle_info(F, UDP1_),
    UDP2_
  end, UDP1, Frames),
  udp_packetizer:terminate(normal, UDP2),
  ok.










