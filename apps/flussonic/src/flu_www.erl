-module(flu_www).

-behaviour(cowboy_middleware).

-export([execute/2]).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").




execute(Req, Env) ->
  case lists:keyfind(routing, 1, Env) of
    {routing, {M,F,A,Opts}} ->
      handle_flu_action(M,F,A,Opts,Req);
   false ->
      {ok, Req, Env}
  end.


handle_flu_action(M,F,A1,Opts,Req) ->
  A = case A1 of
    [req|A_] -> [Req|A_];
    _ -> A1
  end,

  T1 = os:timestamp(),
  Reply1 = try run_action_with_auth(M,F,A, Opts, Req)
  catch
    throw:R -> R;
    Class:Reason ->
      lager:error(
        "** Flu handler ~p:~p~p failed:\n"
        "   for the reason ~p:~p\n"
        "** Request was: ~p\n"
        "** Stacktrace: ~p\n",
        [M,F,A, Class, Reason, cowboy_req:to_list(Req), erlang:get_stacktrace()] ),
      {ok, {500, [], <<"Server error\n">>}}
  end,
  T2 = os:timestamp(),

  Reply2 = convert_old_replies(Reply1),
  Reply3 = normalize_short_replies(Reply2, Opts, Req),

  % Calculate stats
  log_request(Req, Reply3, timer:now_diff(T2,T1) div 1000),

  case Reply3 of
    {ok, {Code, Headers, Body}} when is_number(Code) ->
      {ok, Req1} = cowboy_req:reply(Code, Headers, Body, Req),
      return_req(Code, Req1);
    {done, Req1} ->
      {halt, cowboy_req:set([{connection,close},{resp_state,done}], Req1)}
  end.


run_action_with_auth(M,F,A,_Opts,_Req) when M == api_handler ->
  erlang:apply(M,F,A);

run_action_with_auth(M,F,A2,Opts,Req) ->
  {name,Name} = lists:keyfind(name,1,Opts),
  Type = proplists:get_value(type,Opts,<<"media">>),
  {ok, Token} = media_handler:check_sessions(Req, Name, [{type, Type} | Opts]),

  A = if
    M == flu_stream andalso F == hds_manifest andalso Token =/= undefined -> A2 ++ [Token];
    M == dvr_session andalso F == hds_manifest andalso Token =/= undefined -> A2 ++ [Token];
    true -> A2
  end,
  erlang:apply(M,F,A).


convert_old_replies({error, busy}) -> {ok, {503, [], <<"busy\n">>}};
convert_old_replies({error, no_segment}) -> {ok, {404, [], <<"not found\n">>}};
convert_old_replies({error, enoent}) -> {ok, {404, [], <<"not found\n">>}};
convert_old_replies({error, {return, Code, Message}}) -> {ok, {Code, [], [Message,"\n"]}};
convert_old_replies({error, Error}) -> {ok, {500, [], io_lib:format("~p~n", [Error])}};
convert_old_replies({return, Code_, Message}) -> {ok, {Code_, [], [Message,"\n"]}};
convert_old_replies(undefined) -> {ok, {404, [], <<"not found\n">>}};
convert_old_replies(Reply) -> Reply.


normalize_short_replies({ok, {Code, Body}}, Opts, _) when is_number(Code) ->
  {ok, {Code, headers(proplists:get_value(tag, Opts)), Body}};
normalize_short_replies({ok, Body}, Opts, _) when is_binary(Body) orelse is_list(Body) ->
  {ok, {200, headers(proplists:get_value(tag, Opts)), Body}};
normalize_short_replies({json, JSON},_,_) ->
  {ok, {200, headers(json), [mochijson2:encode(JSON), "\n"]}};
normalize_short_replies(done, _, Req) ->
  {done, Req};
normalize_short_replies({done, _} = Reply, _,_) ->
  Reply;
normalize_short_replies({ok,{_,_,_}} = Reply, _,_) ->
  Reply.


log_request(Req, Reply, Time) ->
  {Path, _} = cowboy_req:path(Req),
  {Code, Size} = case Reply of
    {ok, {Code_, _, Body_}} -> {Code_, iolist_size(Body_)};
    _ -> {0, 0}
  end,
  lager:debug([{request,web},{duration,Time}],"~3..0B ~5.. B ~B ~s", [Code,Time, Size, Path]),
  ok.




return_req(500, Req) ->
  {halt, cowboy_req:set([{connection, close}, {resp_state, done}], Req)};
return_req(_, Req) ->
  {halt, Req}.


headers(html) -> [{<<"Content-Type">>, <<"text/html">>}];
headers(hds) -> [{<<"Content-Type">>, <<"text/xml">>}|no_cache()];
headers(hds_d) -> [{<<"Content-Type">>, <<"video/f4f">>}];
headers(hls) -> [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()];
headers(hls_d) -> [{<<"Content-Type">>, <<"video/MP2T">>}];
headers(json) -> [{<<"Content-Type">>,<<"application/json">>}];
headers(_) -> [].


no_cache() ->
  [{<<"Cache-Control">>, <<"no-cache">>},{<<"Pragma">>, <<"no-cache">>}].
