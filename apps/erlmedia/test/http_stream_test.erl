-module(http_stream_test).
-compile(export_all).



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl").

do(Mod) ->
  case handle_test_req(Mod) of
    false ->
      {proceed, Mod#mod.data};
    Else ->
      Else
  end.

handle_test_req(#mod{absolute_uri="127.0.0.1:9090/redirect"}) ->
  {proceed, [{response,{response,[{code,301},{location,"http://127.0.0.1:9090/123.txt"}],nobody}}]};

handle_test_req(#mod{absolute_uri="127.0.0.1:9090/chunk"}) ->
  Body = "25\nThis is the data in the first chunk\r\n3\nsec\r\n0",
  {proceed, [{response,{response,[{code,200},{transfer_encoding,"chunked"}],Body}}]};

handle_test_req(_Mod) ->
  false.



calculate_redirected_url_test_() ->
  [
  ?_assertEqual(<<"http://ya.ru/145">>, http_stream:calculate_redirected_url("http://ya.ru/", "/145")),
  ?_assertEqual(<<"http://ya.ru/145">>, http_stream:calculate_redirected_url("http://ya.ru/", "http://ya.ru/145")),
  ?_assertEqual(<<"http://yahoo.com/145">>, http_stream:calculate_redirected_url("http://ya.ru/", "http://yahoo.com/145"))
  ].


request_body_test_() ->
  {setup, 
   fun() ->
       inets:start(),
       inets:start(httpd,[
        {server_root,"../test"},
        {port,9090},
        {server_name,"test_server"},
        {document_root,"../test/files"},
        {modules,[mod_alias,mod_range, ?MODULE, mod_auth, mod_actions, mod_dir, mod_get, mod_head]}
      ])
   end, 
   fun({ok,Pid}) ->
      error_logger:delete_report_handler(error_logger_tty_h),
      inets:stop(httpd,Pid)
   end,
   [
   %%%  Simple body request test
   ?_assertMatch({ok,{_Socket,200,_Headers,<<"123\n">>}},http_stream:request_body("http://127.0.0.1:9090/123.txt",[])),
   %%% Case when connection is enstablished
   ?_assertEqual({error,econnrefused},http_stream:request_body("http://127.0.0.1:9091/",[])),
   %%% Case when page not found 
   ?_assertEqual({error,{http_code,404}},http_stream:request_body("http://127.0.0.1:9090/wrong.file",[])),
    %%% Range request test
   ?_assertMatch({ok,{_Socket,206,_Headers,<<"1">>}},http_stream:request_body("http://127.0.0.1:9090/123.txt",[{range,{0,1}}])),
    %%% Range out test 
   ?_assertEqual({error,{http_code,416}}, http_stream:request_body("http://127.0.0.1:9090/123.txt",[{range,{5,10}}])),

%%% Redirect test
   ?_assertMatch({ok,{_Socket,200,
       [{'Server',_ServerName},
        {'Date',_Date},
        {'Content-Type',<<"text/plain">>},
        {'Etag',_ETag},
        {'Content-Length',<<"4">>},
        {'Last-Modified',_LastMod},
        {redirected_url,<<"http://127.0.0.1:9090/123.txt">>}]}},
        http_stream:request("http://127.0.0.1:9090/redirect",[])),

%%% Noredirect option test
   ?_assertMatch({ok,{_Socket,301,
       [{'Server', _ServerName},
        {'Content-Type',<<"text/html">>},
        {'Date', _Date},
        {'Location',<<"http://127.0.0.1:9090/123.txt">>}]}},
        http_stream:request("http://127.0.0.1:9090/redirect",[{noredirect, true}])),
%%% Chunked body test
   ?_assertMatch({ok,{_Socket, 200,
       [{'Server', _ServerName},
        {'Content-Type',<<"text/html">>},
        {'Date', _Date},
        {'Transfer-Encoding',<<"chunked">>},
        {redirected_url,"http://127.0.0.1:9090/chunk"}],
       <<"This is the data in the first chunk\r\nsec">>}},
        http_stream:request_body("http://127.0.0.1:9090/chunk",[]))
   ]
   }.
