-module(flu_session_log_tests).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-define(STATS, [{session_id,<<"34626121de81cea59069efddfd68a391880b1ddd">>},
                {token,<<"1349348102172">>},
                {ip,<<"127.0.0.1">>},
                {name,<<"ort1">>},
                {user_id,undefined},
                {flag,granted},
                {type,<<"hds">>},
                {created_at,1349348103184},
                {expire_time,120000},
                {last_access_time,1349348103184},
                {bytes_sent,0},
                {pid,undefined},
                {ref,undefined},
                {options,[]}]).

-define(TEST_FORMAT(In, Out), ?_assertEqual(Out, lists:flatten(flu_session_log:format(In, ?STATS)))).

flu_session_log_test_() ->
  [?TEST_FORMAT("", ""),
   ?TEST_FORMAT("%%\%%n%%b", "%%%ort1%0"),
   ?TEST_FORMAT("%s %t %i %n %u %f %y %c %e %l %b",
                "34626121de81cea59069efddfd68a391880b1ddd "
                "1349348102172 "
                "127.0.0.1 "
                "ort1 "
                "undefined "
                "granted "
                "hds "
                "1349348103184 "
                "120000 "
                "1349348103184 "
                "0")].
