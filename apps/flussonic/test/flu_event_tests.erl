-module(flu_event_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("../src/flu_event.hrl").


disconnect_to_json_test() ->
  ?assertMatch(<<"{\"event\":\"user.disconnected\"",_/binary>>, flu_event:to_json(disconnect_event())).

disconnect_to_xml_test() ->
  ?assertMatch(<<"<",_/binary>>, flu_event:to_xml(disconnect_event())).


disconnect_event() ->
  #flu_event{
    event = 'user.disconnected',
    user_id = 123456,
    session_id = <<"98aea28ed2a9149e1f6be2a71073e6cbf99ecc33">>,
    stream = <<"live/mini">>,
    options = [
      {session_id,<<"98aea28ed2a9149e1f6be2a71073e6cbf99ecc33">>},
      {token,<<"123456">>},
      {ip,<<"213.141.129.72">>},
      {name,<<"live/mini">>},
      {user_id,123456},
      {access,granted},
      {type,<<"rtmp">>},
      {created_at,1358267356425},
      {auth_time,10000},
      {delete_time,10000},
      {last_access_time,1358267356425},
      {bytes_sent,0},{pid,erlang:list_to_pid("<0.5379.0>")},{ref,make_ref()},{options,[]}]
  }.


configuration_test_() ->
  {foreach, fun() ->
    application:load(flussonic),
    gen_event:start_link({local, flu_event})
  end, fun(_) ->
    gen_event:stop(flu_event),
    ok
  end, 
    [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports),
    lists:prefix("test_", atom_to_list(F))]  
  }.


test_initialize() ->
  Self = self(),
  Conf1 = [Self, 1, 2],
  flu_config:set_config([{flu_event, fake_event_handler, Conf1}]),
  ?assertEqual([], gen_event:which_handlers(flu_event)),
  flu_event:start_handlers(),
  ?assertEqual([fake_event_handler], gen_event:which_handlers(flu_event)),
  ?assertEqual(Conf1, gen_event:call(flu_event, fake_event_handler, options)),
  ok.


test_update_conf() ->
  Self = self(),

  Conf1 = [Self, 1, 2],
  flu_config:set_config([{flu_event, fake_event_handler, Conf1}]),
  flu_event:start_handlers(),


  Conf2 = [Self, 3, 4],
  flu_config:set_config([{flu_event, fake_event_handler, Conf2}]),
  flu_event:start_handlers(),

  ?assertEqual(Conf2, gen_event:call(flu_event, fake_event_handler, options)),
  ok.


test_install_new_conf() ->
  flu_config:set_config([{flu_event, fake_event_handler, [1,2]}]),
  flu_event:start_handlers(),


  flu_config:set_config([{flu_event, fake_event_handler, [3,4]}, {flu_event, flu_event_consumer, [self()]}]),
  flu_event:start_handlers(),

  ?assertEqual([3,4], gen_event:call(flu_event, fake_event_handler, options)),
  gen_event:notify(flu_event, my_test_event),

  receive
    my_test_event -> ok
  after
    100 -> error({flu_event_consumer,not_installed})
  end.









