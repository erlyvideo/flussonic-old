-module(flu_file_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).





flu_file_test_() ->
  {foreach,
  fun setup_flu_file/0,
  fun teardown_flu_file/1,
  [{with, [
    fun test_hls_playlist/1,
    fun test_hls_segment/1,
    fun test_hds_manifest/1,
    fun test_hds_segment/1
  ]}]}.

setup_flu_file() ->
  Modules = [],
  meck:new(Modules, [{passthrough, true}]),
  {ok, File} = flu_file:start_link("bunny.mp4", [{root, "priv"}]),
  unlink(File),
  lager:set_loglevel(lager_console_backend, notice),
  {Modules, File}.


teardown_flu_file({Modules, File}) ->
  meck:unload(Modules),
  erlang:exit(File, shutdown),
  lager:set_loglevel(lager_console_backend, info),
  ok.



test_hls_playlist({_,File}) ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hls_playlist(File)).

test_hls_segment({_,File}) ->
  ?assertMatch({ok, Bin} when is_list(Bin), flu_file:hls_segment(File, 2)).

test_hds_manifest({_,File}) ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hds_manifest(File)).

test_hds_segment({_,File}) ->
  ?assertMatch({ok, Bin} when is_binary(Bin), flu_file:hds_segment(File, 2)).
