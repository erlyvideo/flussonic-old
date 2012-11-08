-module(flu_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-define(TIMEOUT, 120000).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([find_session/1, new_or_update/2, update_session/1, url/1, ref/1]).
-export([info/1]).
-export([table/0]).
-export([stats/0]).
-export([list/0, clients/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("flu_session.hrl").

-export([verify/3]).


-export([backend_request/3, timeout/0]).

% -record(session, {
%   id,
%   expire,
%   path,
%   name,
%   options = []
% }).




merge(List1, List2) ->
  lists:ukeymerge(1, lists:ukeysort(1,List1), lists:ukeysort(1, List2) ).

verify(URL, Identity, Options) ->
  Now = flu:now_ms(),

  {Session, ErrorMessage} = case find_session(Identity) of
    Sess when Sess == undefined orelse Sess#session.last_access_time + Sess#session.expire_time < Now ->
      case backend_request(URL, Identity, Options) of
        {error,  {_Code,ErrMsg}, Opts1} -> {new_or_update(Identity, Opts1 ++ Options), ErrMsg};
        {ok, Name1, Opts1} -> {new_or_update(Identity, merge([{name,Name1}], Opts1 ++ Options)), "cached_positive"}
      end;
    R -> {R, "cached_negative"}
  end,
  case update_session(Session) of
    denied -> {error, 403, ErrorMessage};
    granted -> {ok, url(Session)}
  end.


timeout() ->
  3000.

backend_request(URL, Identity, Options) ->
  Query = [io_lib:format("~s=~s&", [K,V]) || {K,V} <- Identity ++ Options, is_binary(V) orelse is_list(V)],
  RequestURL = lists:flatten([URL, "?", Query]),
  case httpc:request(get, {RequestURL, []}, [{connect_timeout, flu_session:timeout()},{timeout, flu_session:timeout()},{autoredirect,false}],
    [], auth) of
    {ok, {{_,Code,_}, Headers, _Body}} ->
      ?DBG("Backend auth request \"~s\": ~B code", [RequestURL, Code]),
      Opts0_ = [{expire,to_i(proplists:get_value("x-authduration", Headers, 30))*1000},
        {user_id,to_i(proplists:get_value("x-userid", Headers))}],
      Opts0 = merge([{K,V} || {K,V} <- Opts0_, V =/= undefined], Options),
      % Name = to_b(proplists:get_value("x-name", Headers, proplists:get_value(name, Identity))),
      Name = proplists:get_value(name, Identity),
      case Code of
        200 -> {ok,    Name, merge([{access, granted}],Opts0)};
        302 -> {ok,    Name, merge([{access, granted}],Opts0)};
        403 -> {error, {403, "backend_denied"},  merge([{access, denied}], Opts0)};
        _ ->   {error, {403, io_lib:format("backend_code: ~B", [Code])},  merge([{access, denied}], Opts0)}
      end;
    {error, _Error} ->
      ?DBG("Backend auth request \"~s\": failed: ~p", [RequestURL, _Error]),
      {error, {404, "backend_http_error"}, merge([{access, denied}], Options)}
  end.


to_i(undefined) -> undefined;
to_i(List) when is_list(List) -> list_to_integer(List);
to_i(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin));
to_i(Int) when is_integer(Int) -> Int.


% to_b(undefined) -> undefined;
% to_b(List) when is_list(List) -> list_to_binary(List);
% to_b(Bin) when is_binary(Bin) -> Bin.

clients() ->
  Now = flu:now_ms(),
  Sessions = ets:select(flu_session:table(), ets:fun2ms(fun(#session{flag = granted} = E) -> E end)),
  [[{ip,IP},{name,Name},{start_at,StartAt},{duration,Now - StartAt},{type,Type}] || #session{ip = IP, name = Name, created_at = StartAt, type = Type} <- Sessions].

list() ->
  flu_rtmp:clients() ++ clients().

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stats() ->
  Streams = ets:foldl(fun(#session{name = Name}, Acc) ->
      dict:update_counter(Name, 1, Acc)
    end, dict:new(), flu_session:table()),
  dict:to_list(Streams).

% cookie_name() ->
%   <<"flu_cookie_">>.

url(#session{name = Name}) -> Name.
ref(#session{ref = Ref}) -> Ref.


info(#session{} = Session) -> lists:zip(record_info(fields, session), tl(tuple_to_list(Session)));
info(undefined) -> undefined;
info(Identity) -> info(find_session(Identity)).
  


hex(Binary) when is_binary(Binary) ->
  iolist_to_binary([string:to_lower(lists:flatten(io_lib:format("~2.16.0B", [H]))) || <<H>> <= Binary]).

session_id(Identity) -> hex(crypto:sha([V || {_K,V} <- lists:sort(Identity), is_list(V) orelse is_binary(V)])).

find_session(Identity) ->
  SessionId = session_id(Identity),
  case ets:lookup(flu_session:table(), SessionId) of
    [] -> undefined;
    [#session{} = Session] -> Session
  end.

new_or_update(Identity, Opts) ->
  Flag    = proplists:get_value(access, Opts, denied),
  Expire  = proplists:get_value(expire, Opts, ?TIMEOUT),
  Pid     = proplists:get_value(pid, Opts),

  Now = flu:now_ms(),

  SessionId = session_id(Identity),
  Token = proplists:get_value(token,Identity),
  Ip = proplists:get_value(ip, Identity),
  UserId = proplists:get_value(user_id, Opts),
  Name = proplists:get_value(name, Opts, proplists:get_value(name, Identity)),
  {ok, Ref} = if is_pid(Pid) -> gen_server:call(?MODULE, {register, Pid});
    Pid == undefined -> {ok, undefined}
  end,
  {OldSession, New} = case ets:lookup(flu_session:table(), SessionId) of
    [#session{} = Old_] -> 
      {Old_, false};
    [] -> 
      {#session{session_id = SessionId, token = Token, ip = Ip, name = Name, created_at = Now,
      bytes_sent = 0, pid = Pid, ref = Ref, user_id = UserId, type = proplists:get_value(type, Opts, <<"http">>)}, true}
  end,
  Session = OldSession#session{expire_time = Expire, last_access_time = Now, flag = Flag},
  ets:insert(flu_session:table(), Session),
  erlang:put(<<"session_cookie">>, Token),

  case New of
    true -> flu_event:user_connected(Name, info(Session));
    false -> ok
  end,
  Session.

update_session(#session{session_id = SessionId, flag = Flag}) ->
  ets:update_element(flu_session:table(), SessionId, {#session.last_access_time, flu:now_ms()}),
  Flag.

delete_session(Session) ->
    ets:delete_object(flu_session:table(), Session),
    flu_event:user_disconnected(Session#session.name, info(Session)).


table() ->
  ?MODULE.




init([]) ->
  ets:new(flu_session:table(), [public, named_table, {keypos, #session.session_id}]),
  timer:send_interval(5000, clean),
  {ok, state}.

handle_call({register, Pid}, _From, State) ->
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Ref}, State};

handle_call({unregister, Ref}, _From, State) ->
  erlang:demonitor(Ref, [flush]),
  {reply, ok, State};

handle_call(Call, _From, State) ->
  {reply, {error, Call}, State}.

handle_info(clean, State) ->
  Now = flu:now_ms(),
  [delete_session(Session) ||
      Session <- ets:select(flu_session:table(),
                            ets:fun2ms(fun(#session{expire_time = T, last_access_time = Last} = S)
                                             when T + Last < Now -> S end))],
  {noreply, State};

handle_info({'DOWN', Ref, _, _Pid, _}, State) ->
  [delete_session(Session) ||
      Session <- ets:select(flu_session:table(),
                            ets:fun2ms(fun(#session{ref = R} = S)
                                             when R == Ref -> S end))],
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_,_) -> ok.



