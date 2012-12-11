-module(flu_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

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


-export([timeout/0]).

% -record(session, {
%   id,
%   expire,
%   path,
%   name,
%   options = []
% }).




merge(List1, List2) ->
  lists:ukeymerge(1, lists:ukeysort(1,List1), lists:ukeysort(1, List2) ).

verify(URL, Identity, Options) when is_list(URL) ->
  verify(list_to_binary(URL), Identity, Options);

verify(URL, Identity, Options) ->
  Now = flu:now_ms(),

  is_binary(URL) orelse throw({error, bad_auth_url}),
  is_list(Identity) orelse throw({error, bad_identity}),
  is_binary(proplists:get_value(token,Identity)) orelse throw({error, bad_token}),
  is_binary(proplists:get_value(ip,Identity)) orelse throw({error, bad_ip}),
  is_binary(proplists:get_value(name,Identity)) orelse throw({error, bad_name}),

  case Options of
    [] -> ok;
    [{_K,_V}|_] -> ok;
    [_] -> throw({error, bad_params})
  end,

  {Session, ErrorMessage} = case find_session(Identity) of
    Sess when Sess == undefined orelse Now > Sess#session.last_access_time + Sess#session.auth_time ->
      RequestType = case Sess of
        undefined -> [{request_type,new_session}];
        _ -> [{request_type,update_session}]
      end,
      Stats = stats(),
      StreamClients = proplists:get_value(proplists:get_value(name,Identity), Stats, 0),
      TotalClients = lists:sum([Count || {_,Count} <- Stats]),
      ClientsInfo = [{stream_clients,StreamClients},{total_clients,TotalClients}],

      case auth_http_backend:verify(URL, Identity, ClientsInfo ++ RequestType ++ Options) of
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



% to_b(undefined) -> undefined;
% to_b(List) when is_list(List) -> list_to_binary(List);
% to_b(Bin) when is_binary(Bin) -> Bin.

clients() ->
  Now = flu:now_ms(),
  Sessions = ets:select(flu_session:table(), ets:fun2ms(fun(#session{flag = granted} = E) -> E end)),
  [[{ip,IP},{name,Name},{start_at,StartAt},{duration,Now - StartAt},{type,Type}] || #session{ip = IP, name = Name, created_at = StartAt, type = Type} <- Sessions].

list() ->
  clients().

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
  AuthTime  = proplists:get_value(auth_time, Opts, 30000),
  DeleteTime  = proplists:get_value(delete_time, Opts, 30000),
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
  Session = OldSession#session{auth_time = AuthTime, delete_time = DeleteTime, last_access_time = Now, flag = Flag},
  ets:insert(flu_session:table(), Session),

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
  ToDelete = ets:select(flu_session:table(),
    ets:fun2ms(fun(#session{auth_time = A, delete_time = D, last_access_time = Last} = S)
                                             when Now > Last + A + D -> S end)),
  % if length(ToDelete) > 0 -> 
  % ?D({deleting, [{Tok, Now - (Last+A+D)} || #session{token = Tok, last_access_time = Last, auth_time = A, delete_time = D} <- ToDelete]});
  % true -> ok end,
  [delete_session(Session) || Session <- ToDelete],
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



