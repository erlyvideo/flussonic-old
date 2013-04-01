-module(hds_packetizer_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).



all() ->
  [{group,hds}].


groups() ->
  [{hds,[parallel],[
    full_cycle
  ]}].

init_per_suite(Config) ->
  R = flu_test:setup(fun() ->
    flu_test:set_config([
      {stream,"stream1","passive://",[{sessions, "http://127.0.0.1:5671/auth/token_unique_number"}]}
    ]),
    ok
  end),
  [{r,R}|Config].

end_per_suite(Config) ->
  {value,{r,R},Config1} = lists:keytake(r,1,Config),
  flu_test:teardown(R),
  Config1.



test_terminate() ->
  {ok, HDS} = hds_packetizer:init([{name,<<"stream1">>}]),
  ok = hds_packetizer:terminate(normal, HDS),
  ?assertEqual({ok, false}, gen_tracker:getattr(flu_streams, <<"stream1">>, hds)),
  ok.


% test_gop_with_empty_media_info() ->
%   {ok, HDS1} = hds_packetizer:init([{name,<<"stream1">>}]),
%   {noreply, _HDS2} = hds_packetizer:handle_info({gop, gop(2)}, HDS1),
%   ?assertMatch(undefined, gen_tracker:getattr(flu_streams,<<"stream1">>, hds_manifest)),
%   ?assertMatch(undefined, gen_tracker:getattr(flu_streams,<<"stream1">>, {hds_fragment, 1, 1})),
%   ?assertMatch(undefined, gen_tracker:getattr(flu_streams,<<"stream1">>, bootstrap)),
%   ok.


wait(_,_,0) -> undefined;

wait(K,V,Count) ->
  case gen_tracker:getattr(flu_streams, K, V) of
    undefined ->
      timer:sleep(50),
      wait(K,V,Count-1);
    Else ->
      Else
  end.


full_cycle(_Config) ->
  {ok, S} = flu_stream:autostart(<<"stream1">>, [{url,<<"passive://ok">>}]),
  S ! flu_test:media_info(),

  ?assertEqual({ok, true}, wait(<<"stream1">>, hds, 10)),

  ?assertEqual(undefined, gen_tracker:getattr(flu_streams,<<"stream1">>, hds_manifest)),

  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(1)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(2)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(3)],



  ?assertMatch({ok, Fragment} when is_binary(Fragment),
    gen_tracker:getattr(flu_streams,<<"stream1">>, {hds_fragment, 1, 1})),
  ?assertMatch({ok, Manifest} when is_binary(Manifest),
    gen_tracker:getattr(flu_streams,<<"stream1">>, hds_manifest)),
  ?assertMatch({ok, Bootstrap} when is_binary(Bootstrap),
    gen_tracker:getattr(flu_streams,<<"stream1">>, bootstrap)),

  % Now lets check full HTTP cycle

  {ok, {{403,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/stream1/manifest.f4m", get, [], 1000),
  {ok, {{200,_}, _, _Manifest}} = flu_test:request("http://127.0.0.1:5670/stream1/manifest.f4m?token=123", get, [], 1000),
  {ok, {{403,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/stream1/bootstrap", get, [], 1000),
  {ok, {{200,_}, _, _Bootstrap}} = flu_test:request("http://127.0.0.1:5670/stream1/bootstrap?token=123", get, [], 1000),
  {ok, {{200,_}, _, _}} = flu_test:request("http://127.0.0.1:5670/stream1/hds/0/Seg1-Frag1?token=123", get, [], 1000),





  {ok, <<_:32, "mdat", FLV/binary>>} = gen_tracker:getattr(flu_streams,<<"stream1">>, {hds_fragment, 1, 1}),
  [#video_frame{content = metadata}|Frames] = flv:read_all_frames(FLV),
  % ?debugFmt("gop: ",[]),
  % [?debugFmt("~4s ~8s ~B", [Codec, Flavor, round(DTS)]) || #video_frame{codec = Codec, flavor = Flavor, dts = DTS} <- Gop],

  % ?debugFmt("~nframes: ",[]),
  % [?debugFmt("~4s ~8s ~B", [Codec, Flavor, round(DTS)]) || #video_frame{codec = Codec, flavor = Flavor, dts = DTS} <- Frames],
  GopLen = length(flu_test:gop(1)),
  ?assertEqual(GopLen, length(Frames)-2),

  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(4)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(5)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(6)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(7)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(8)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(9)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(10)],
  [flu_stream:send_frame(S, Frame) || Frame <- flu_test:gop(11)],



  {ok, Bootstrap} = gen_tracker:getattr(flu_streams,<<"stream1">>, bootstrap),
  Atom = mp4:parse_atom(Bootstrap, state),
  {'Bootstrap', Options, <<>>} = Atom,
  ?assertEqual(1, proplists:get_value(live, Options)),
  ?assertEqual(1000, proplists:get_value(timescale, Options)),
  ?assertEqual(35042, proplists:get_value(current_time, Options)),

  ?assertEqual({'SegmentRunTable',[{update,0}],[{1,6}]}, proplists:get_value(segments, Options)),
  ?assertMatch({'FragmentRunEntry', _, _}, proplists:get_value(fragments, Options)),

  {'FragmentRunEntry', AFRTOptions, AFRTData} = proplists:get_value(fragments, Options),

  ?assertEqual(0, proplists:get_value(update, AFRTOptions)),
  ?assertEqual(1000, proplists:get_value(timescale, AFRTOptions)),


  ?assertEqual([5,6,7,8,9,10], [N || {N,_,_} <- AFRTData]),

  [?assertMatch(_ when Start > 0 andalso End > 0, {N,Start,End}) || {N,Start,End} <- AFRTData],

  ok.















