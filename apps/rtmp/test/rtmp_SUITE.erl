-module(rtmp_SUITE).

-compile(export_all).
-include("../include/rtmp.hrl").

-include_lib("eunit/include/eunit.hrl").



all() ->
  [{group,rtmp}].


groups() ->
  [{rtmp,[parallel], [
    connect_ok,
    connect_failed,
    publish,
    play_ok_stream,
    play_ok_file,
    play_forbidden_app,
    play_failed,
    play_rejected
  ]}].

init_per_suite(Config) ->
  application:start(crypto),
  application:start(ranch),
  application:start(rtmp),
  rtmp_socket:start_server(1945, test_rtmp, rtmp_test_client, [[]]),
  Config.


end_per_suite(Config) ->
  application:stop(rtmp),
  application:stop(ranch),
  application:stop(crypto),
  Config.




connect_ok(_Config) ->
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1945/rtmpapp"),
  rtmp_socket:close(RTMP),
  ok.

connect_failed(_Config) ->
  {error, _} = rtmp_lib:connect("rtmp://localhost:1945/app").



publish(_Config) ->
  {ok, RTMP} = rtmp_lib:connect("rtmp://localhost:1945/rtmpapp"),
  Stream = rtmp_lib:createStream(RTMP),
  ok = rtmp_lib:publish(RTMP, Stream, <<"stream0">>),
  {ok, RTMP}.


play_ok_stream(_Config) ->
  {ok, RTMP, _StreamId} = rtmp_lib:play("rtmp://localhost:1945/rtmpapp/stream"),
  {ok, RTMP}.

play_ok_file(_Config) ->
  {ok, RTMP, _StreamId} = rtmp_lib:play("rtmp://localhost:1945/rtmpapp/file.mp4"),
  {ok, RTMP}.

play_forbidden_app(_Config) ->
  ?assertMatch({error, _}, rtmp_lib:play("rtmp://localhost:1945/app/file.mp4")),
  ok.

play_failed(_Config) ->
  ?assertMatch({error, _}, rtmp_lib:play("rtmp://localhost:1945/rtmpapp/cam0")),
  ok.

play_rejected(_Config) ->
  ?assertMatch({error, _}, rtmp_lib:play("rtmp://localhost:1945/rtmpapp/rejected")),
  ok.

