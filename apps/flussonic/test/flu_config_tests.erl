-module(flu_config_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").




expand_entry_test_() ->
  [?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,false}]}]},
      flu_config:parse_config([{rewrite, "stream1", "fake://stream1"}], undefined)),
  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{dvr,"root"},{static,false}]}]},
      flu_config:parse_config([{rewrite, "stream1", "fake://stream1", [{dvr,"root"}]}], undefined)),

  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{static,true}]}]},
      flu_config:parse_config([{stream, "stream1", "fake://stream1"}], undefined)),
  ?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{dvr,"root"},{static,true}]}]},
      flu_config:parse_config([{stream, "stream1", "fake://stream1", [{dvr,"root"}]}], undefined)),

  ?_assertEqual({ok, [{mpegts, <<"stream">>, [{clients_timeout,false}]}]},
      flu_config:parse_config([{mpegts, "stream"}], undefined)),
  ?_assertEqual({ok, [{mpegts, <<"stream">>, [{clients_timeout,false},{sessions, "http://host"}]}]},
      flu_config:parse_config([{mpegts, "stream", [{sessions, "http://host"}]}], undefined)),

  ?_assertEqual({ok, [{live, <<"live">>, []}]},
      flu_config:parse_config([{live, "live"}], undefined)),
  ?_assertEqual({ok, [{live, <<"live">>, [{sessions, "http://host"}]}]},
      flu_config:parse_config([{live, "live", [{sessions, "http://host"}]}], undefined)),

  ?_assertEqual({ok, [{file, <<"vod">>, <<"/movies">>, []}]}, 
      flu_config:parse_config([{file, "vod", "/movies"}], undefined)),
  ?_assertEqual({ok, [{file, <<"vod">>, <<"/movies">>, [{sessions, "http://ya.ru/"}]}]}, 
      flu_config:parse_config([{file, "vod", "/movies", [{sessions, "http://ya.ru/"}]}], undefined)),

  ?_assertEqual({ok, [{api, []}]}, 
    flu_config:parse_config([api], undefined)),
  ?_assertEqual({ok, [{api, [{pass,"admin","passw"}]}]}, 
    flu_config:parse_config([{api,[{pass,"admin","passw"}]}], undefined)),

  ?_assertEqual({ok, [{api, [{http_auth,"user", "zzz"}]},{http_auth,"user", "zzz"}]},
    flu_config:parse_config([api,{http_auth,"user", "zzz"}], undefined)),
  ?_assertEqual({ok, [{api, [{http_auth,"user", "zzz"},{pass,"admin","passw"}]},{http_auth,"user", "zzz"}]}, 
    flu_config:parse_config([{api,[{pass,"admin","passw"}]},{http_auth,"user", "zzz"}], undefined)),


  ?_assertEqual({ok, [{plugin, iptv, []}]}, 
    flu_config:parse_config([{plugin, iptv}], undefined)),
  ?_assertEqual({ok, [{plugin, iptv, [{cas,none}]}]}, 
    flu_config:parse_config([{plugin, iptv, [{cas,none}]}], undefined))
  ].

global_sessions_test_() ->
  [?_assertEqual({ok, [{stream, <<"stream1">>, <<"fake://stream1">>, [{sessions,"http://ya.ru"},{static,true}]},{sessions,"http://ya.ru"}]},
      flu_config:parse_config([{stream, "stream1", "fake://stream1"},{sessions, "http://ya.ru"}], undefined)),
  ?_assertEqual({ok, [{live, <<"live">>, [{sessions, "http://ya.ru"}]}, {sessions,"http://ya.ru"}]},
      flu_config:parse_config([{live, "live"}, {sessions, "http://ya.ru"}], undefined)),
  ?_assertEqual({ok, [{file, <<"vod">>, <<"/movies">>, [{sessions, "http://ya.ru/"}]}, {sessions,"http://ya.ru/"}]}, 
      flu_config:parse_config([{file, "vod", "/movies"}, {sessions, "http://ya.ru/"}], undefined))
  ].


parse_route_test_() ->
  [
    ?_assertMatch([{<<"/live/injest/[...]">>, media_handler, [{prefix, <<"live/injest">>}|_]}], 
      flu_config:parse_routes([{live, <<"live/injest">>, []}])),
    ?_assertMatch([{<<"/vod/mp4/[...]">>, media_handler, [{file_prefix,<<"vod/mp4">>},{module,flu_file},{root,<<"/movies">>}]}],
      flu_config:parse_routes([{file, <<"vod/mp4">>, <<"/movies">>, []}]))
  ].


plugin_route_test_() ->
  {setup,
  fun() ->
    meck:new(fake_plugin),
    meck:expect(fake_plugin, routes, fun(_) -> 
      [{<<"/plugin/api/[...]">>, plugin_api, []}]
    end),
    ok
  end,
  fun(_) ->
      meck:unload(fake_plugin)
  end,
  [
  fun() ->
    ?assertMatch([{<<"/plugin/api/[...]">>, plugin_api, []}], 
      flu_config:parse_routes([{plugin, fake_plugin, []}]))
  end
  ]
  }.

