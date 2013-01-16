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