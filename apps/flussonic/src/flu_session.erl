-module(flu_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

%%
%%
%%
%% Структура сессий.
%%
%% Сессии обслуживают два типа запросов: постоянные подключения типа RTMP, RTSP, MPEG-TS
%% и периодические перезапросы типа HLS/HDS
%% 
%% Логика работы сессий выглядит так:
%% 1) пользователь приходит с каким-то token, который сгенерен внешним сервисом
%% 2) этот токен, IP адрес и имя потока вместе образуют идентификатор сессии Identity
%% 3) так же добавляются остальные опции типа pid процесса, referer и прочее
%% 4) проверяется есть ли сессия с этим идентификатором в базе
%% 5) если есть, то проверяется не пора ли её перепроверить
%% 6) если пора перепроверить, то перепроверяется
%% 7) если повторная проверка запрещает сессию, то пользователь отключается
%% 8) иначе продолжаем дальше показывать
%% 9) если сессия не протухла, то просто показываем
%% 10) если сессия запрещена к просмотру, то это запоминается что бы защитить бекенд
%% 
%% Особое внимание надо уделить двум ситуациям, когда сегменты короче чем auth duration и когда длиннее
%% В первом случае access time будет меняться чаще чем истекает auth duration
%% Во втором случае на каждом сегменте будет запрос к бекенду
%%
%% Так же есть опасность того, что при нормальном просмотре сессия будет удаляться между
%% двумя запросами
%%
%% Basically you need only flu_session:verify/3
%%
%% 

-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([find_session/1, new_or_update/2, id/1, ref/1, add_bytes/2]).
-export([info/1]).
-export([table/0]).
-export([stats/0]).
-export([list/0, json_list/0, json_list/1, clients/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("flu_session.hrl").

-export([verify/3, client_count/1]).

-export([backend_request/3]).
-export([recheck_connected/0, delete_all_sessions/0]).

-export([timeout/0]).


-export([bench/0, bench/1, bench0/1]).

-export([per_ip_limit/0, global_limit/0]).

bench() ->
  bench(1000000).

bench(Count) ->
  [gen_event:delete_handler(flu_event, H, []) || H <- gen_event:which_handlers(flu_event)],
  bench0(Count).

bench0(0) -> ok;
bench0(Count) ->
  case Count rem 1000 of
    0 -> ?D(Count);
    _ -> ok
  end,
  verify(true, [{token,list_to_binary(integer_to_list(Count))},{ip,<<"ip">>},{name,<<"ort">>}], [{type,<<"hds">>}]),
  bench0(Count - 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Interface functions
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type auth_url() :: any().

-type auth_option() ::
  {user_id, UserId::non_neg_integer()} |
  {auth_time, AuthTime::non_neg_integer()} |
  {delete_time, DeleteTime::non_neg_integer()} |
  {unique, true|false}.

-type backend_reply() :: {ok, [auth_option()]} | {error, [auth_option()]} | undefined.


-type identity_part() ::
  {token,Token::binary()} | % Token passed via query string
  {ip, IP::binary()} | % IP of user
  {name, Name::binary()}. % Name of stream
-type session_identity() :: [identity_part()].

-type session_option() ::
  {pid,Pid::pid()} |
  {type, Type::binary()}.



-spec backend_request(URL::auth_url(), Identity::session_identity(), Options::list()) -> backend_reply().
backend_request(URL, Identity, Options) ->
  try backend_request0(URL, Identity, Options) of
    {ok, _} = Reply -> Reply;
    {error, _} = Reply -> Reply;
    undefined -> undefined;
    Else -> error({invalid_backend_reply,URL,Identity,Else})
  catch
    Class:Error -> erlang:raise(Class, {error_auth_backend,URL,Identity,Error}, erlang:get_stacktrace())
  end.


backend_request0(true, _Identity, _Options) ->
  {ok, []};

backend_request0(<<"http://", _/binary>> = URL, Identity, Options) when is_list(Identity), is_list(Options) ->
  Reply = auth_http_backend:verify(URL, Identity, Options),
  Reply.


-spec verify(URL::auth_url(), Identity::session_identity(), Options::[session_option()]) ->
  {ok, SessionId::integer()} | {error, Code::non_neg_integer(), ErrorMsg::iolist()}.

verify(URL, Identity, Options) when is_list(URL) ->
  verify(list_to_binary(URL), Identity, Options);


verify(URL, Identity, Options) ->
  try verify0(URL, Identity, Options)
  catch
    throw:Reply -> Reply
  end.


client_count(Name) when is_binary(Name) ->
  ets:select_count(flu_session:table(), ets:fun2ms(fun(#session{name = N}) when N == Name -> true end)).


add_bytes(undefined, _) -> ok;

add_bytes(SessionId, Bytes) when is_integer(SessionId), is_integer(Bytes) ->
  catch ets:update_counter(flu_session:table(), SessionId, {#session.bytes_sent, Bytes}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%  Here go internal functions-helpers required to validate session request.
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


per_ip_limit() -> 10000.

global_limit() -> 1000000.


validate_url(URL) ->
  is_binary(URL) orelse URL == true orelse throw({error, bad_auth_url}),
  ok.


validate_identity(Identity) ->
  is_list(Identity) orelse throw({error, bad_identity}),
  is_binary(proplists:get_value(token,Identity)) orelse throw({error, bad_token}),
  is_binary(proplists:get_value(ip,Identity)) orelse throw({error, bad_ip}),
  is_binary(proplists:get_value(name,Identity)) orelse throw({error, bad_name}),
  ok.

validate_options([]) -> ok;
validate_options([{_K,_V}|_]) -> ok;
validate_options(_) -> throw({error, bad_params}).



check_session_limits(undefined, Identity) ->
  {ip,IP} = lists:keyfind(ip,1,Identity),
  rate_limiter:hit({sessions,IP}) < ?MODULE:per_ip_limit() orelse throw({error,too_many_per_ip}),
  ets:info(flu_session, size) < ?MODULE:global_limit() orelse throw({error,too_many_sessions}),
  ok;

check_session_limits(#session{}, _Identity) ->
  ok.





need_to_ask_backend(undefined, _) -> true;
need_to_ask_backend(#session{last_verify_time = undefined}, _) -> true;
need_to_ask_backend(#session{last_verify_time = LastAccess, auth_time = AuthTime}, Now) 
  when Now > LastAccess + AuthTime -> true;
need_to_ask_backend(#session{pid = Pid}, _) when is_pid(Pid) -> true;
need_to_ask_backend(#session{}, _) -> false.

additional_options(Identity, Session) ->
  RequestType = case Session of
    undefined -> [{request_type,new_session}];
    _ -> [{request_type,update_session}]
  end,
  StreamClients = case gen_tracker:getattr(flu_streams, proplists:get_value(name,Identity), client_count) of
    undefined -> 0;
    {ok, SC} -> SC
  end,
  TotalClients = ets:info(flu_session, size),
  ClientsInfo = [{stream_clients,StreamClients},{total_clients,TotalClients}],
  ClientsInfo ++ RequestType.


disconnect_other_instances(true, UserId, OurId) when is_number(UserId) ->
  ExistingSessions = ets:select(flu_session:table(), 
    ets:fun2ms(fun(#session{user_id = UID, access = granted} = E) when UID == UserId -> E end)),
  case lists:keyfind(OurId, #session.session_id, ExistingSessions) of
    #session{ref = OldRef, pid = OldPid} when is_reference(OldRef), is_pid(OldPid) ->
      ok = gen_server:call(?MODULE, {unregister, OldRef}),
      erlang:exit(OldPid, duplicated_user_id);
    _ -> ok
  end,

  [begin
    ets:update_element(flu_session:table(), Id, {#session.access, denied}),
    case Pid of
      undefined -> ok;
      _ -> 
        gen_server:call(?MODULE, {unregister, Ref}),
        erlang:exit(Pid, duplicated_user_id)
    end
  end || 
  #session{session_id = Id, pid = Pid, ref = Ref} <- ExistingSessions, Id =/= OurId],
  ok;

disconnect_other_instances(_, _, _) ->
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%  Now the main flu_session logic: verifier
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



verify0(URL, Identity, Options) ->
  Now = flu:now_ms(),

  validate_url(URL),
  validate_identity(Identity),
  validate_options(Options),

  Session = find_session(Identity),
  check_session_limits(Session, Identity),
  touch_session(Session, Now),

  case need_to_ask_backend(Session, Now) of
    true ->
      AdditionalOptions = additional_options(Identity, Session),
      case flu_session:backend_request(URL, Identity, AdditionalOptions ++ Options) of
        {error, Opts1} -> 
          new_or_update(Identity, [{access,denied},{last_verify_time,Now}] ++ Opts1 ++ Options),
          {error, 403, "backend_denied"};
        undefined ->
          case Session of
            #session{access = granted, name = Name} -> {ok, Name};
            #session{access = denied} -> {error, 403, "stale_auth_cache"};
            undefined -> {error, 403, "failed_to_open_session"}
          end;
        {ok, Opts1} ->
          disconnect_other_instances(proplists:get_value(unique, Opts1), proplists:get_value(user_id, Opts1), session_id(Identity)),
          Session1 = new_or_update(Identity, [{access,granted},{last_verify_time,Now}] ++ Opts1 ++ Options),
          {ok, id(Session1)}
      end;
    false when Session#session.access == granted ->
      {ok, id(Session)};
    false when Session#session.access == denied ->
      {error, 403, "cached_negative"}
  end.



timeout() ->
  3000.



% to_b(undefined) -> undefined;
% to_b(List) when is_list(List) -> list_to_binary(List);
% to_b(Bin) when is_binary(Bin) -> Bin.

clients() ->
  Now = flu:now_ms(),
  Sessions = ets:select(flu_session:table(), ets:fun2ms(fun(#session{access = granted} = E) -> E end)),
  [[{id,Id},{ip,IP},{name,Name},{start_at,StartAt},{duration,Now - StartAt},{type,Type},{user_id,UserId},{bytes,Sent},{pid,Pid}] || 
    #session{session_id = Id, ip = IP, name = Name, user_id =UserId, created_at = StartAt, type = Type, bytes_sent = Sent, pid = Pid} <- Sessions].

list() ->
  clients().

json_list() ->
  [{event,'user.list'},{sessions,list()}].

json_list(Name) ->
  [{event,'user.list'},{name,Name},{sessions,[ [{K,V} || {K,V} <- Session, is_binary(V) orelse is_list(V) orelse is_number(V)] || Session <- list(), proplists:get_value(name,Session) == Name]}].  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Session creation/updating
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stats() ->
  [{Name, proplists:get_value(client_count,Info)} || {Name,Info} <- gen_tracker:list(flu_streams)].

% cookie_name() ->
%   <<"flu_cookie_">>.

id(#session{session_id = ID}) -> ID.
ref(#session{ref = Ref}) -> Ref.


info(#session{} = Session) -> lists:zip(record_info(fields, session), tl(tuple_to_list(Session)));
info(undefined) -> undefined;
info(Identity) -> info(find_session(Identity)).
  

% hex(Bin) when is_binary(Bin) ->
%   hex(Bin, <<>>).

% hex(<<>>, Acc) -> Acc;
% hex(<<C, Bin/binary>>, Acc) ->
%   C1 = C div 10, C2 = C rem 10,
%   hex(Bin, <<Acc/binary, (if C1 >= 10 -> C1 - 10 + $a; true -> C1 + $0 end), (if C2 >= 10 -> C2 - 10 + $a; true -> C2 + $0 end)>>).
  
%   % iolist_to_binary([string:to_lower(lists:flatten(io_lib:format("~2.16.0B", [H]))) || <<H>> <= Binary]).

% session_id(Identity) -> hex(crypto:sha([V || {_K,V} <- lists:sort(Identity), is_list(V) orelse is_binary(V)])).
session_id(Identity) -> erlang:phash2(lists:sort(Identity)).

find_session(Identity) ->
  SessionId = session_id(Identity),
  case ets:lookup(flu_session:table(), SessionId) of
    [] -> undefined;
    [#session{} = Session] -> Session
  end.

new_or_update(Identity, Opts) ->
  Access    = proplists:get_value(access, Opts, denied),
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
      catch gen_tracker:increment(flu_streams, Name, client_count, 1),
      catch gen_tracker:increment(flu_files, Name, client_count, 1),
      {#session{session_id = SessionId, token = Token, ip = Ip, name = Name, created_at = Now,
      bytes_sent = 0, user_id = UserId, type = proplists:get_value(type, Opts, <<"http">>)}, true}
  end,
  Session1 = OldSession#session{auth_time = AuthTime, delete_time = DeleteTime, last_access_time = Now, access = Access,
    pid = Pid, ref = Ref},
  Session2 = case proplists:get_value(last_verify_time, Opts) of
    undefined -> Session1;
    LastVerify -> Session1#session{last_verify_time = LastVerify}
  end,
  % TODO: race condition here. First try to insert_new and then restart with updating
  ets:insert(flu_session:table(), Session2),

  case New of
    true -> flu_event:user_connected(Name, info(Session2));
    false -> ok
  end,
  Session2.

touch_session(undefined, _Now) -> ok;
touch_session(#session{session_id = SessionId}, Now) ->
  ets:update_element(flu_session:table(), SessionId, {#session.last_access_time, Now}).


delete_all_sessions() ->
  [delete_session(S) || S <- ets:tab2list(flu_session:table())].

delete_session(#session{name = Name} = Session) ->
  % ?D({delete_session,Session}),
  ets:delete_object(flu_session:table(), Session),
  catch gen_tracker:increment(flu_streams, Name, client_count, -1),
  catch gen_tracker:increment(flu_files, Name, client_count, -1),
  flu_event:user_disconnected(Session#session.name, info(Session)).


table() ->
  ?MODULE.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% single flu_session process that coordinates everything
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-define(REFRESH, 1237).
-define(CLEAN, 4759).

recheck_connected() ->
  gen_server:call(?MODULE, recheck_connected).


-record(sess, {
  clean_timer,
  check_timer
}).

init([]) ->
  ets:new(flu_session:table(), [public, named_table, {keypos, #session.session_id}]),
  C = erlang:send_after(?CLEAN, self(), clean),
  T = erlang:send_after(?REFRESH, self(), recheck_connected),
  {ok, #sess{check_timer = T, clean_timer = C}}.

handle_call(recheck_connected, _From, #sess{} = State) ->
  {reply, ok, recheck_connected(State)};

handle_call({register, Pid}, _From, #sess{} = State) when is_pid(Pid) ->
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Ref}, State};

handle_call({unregister, Ref}, _From, #sess{} = State) when is_reference(Ref) ->
  erlang:demonitor(Ref, [flush]),
  {reply, ok, State};

handle_call(Call, _From, #sess{} = State) ->
  {reply, {error, Call}, State}.

handle_info(recheck_connected, #sess{check_timer = Old} = State) ->
  (catch erlang:cancel_timer(Old)),
  State1 = recheck_connected(State),
  T = erlang:send_after(?REFRESH, self(), recheck_connected),
  {noreply, State1#sess{check_timer = T}};

handle_info(clean, #sess{clean_timer = Old} = State) ->
  (catch erlang:cancel_timer(Old)),

  Now = flu:now_ms(),
  ToDelete = ets:select(flu_session:table(),
    ets:fun2ms(fun(#session{auth_time = A, delete_time = D, last_access_time = Last, pid = undefined} = S)
                                             when Now > Last + A + D -> S end)),
  % if length(ToDelete) > 0 -> 
  %   ?D({deleting,ToDelete});
  % % ?D({deleting, [{Tok, Now - (Last+A+D)} || #session{token = Tok, last_access_time = Last, auth_time = A, delete_time = D} <- ToDelete]});
  % true -> ok end,
  [delete_session(Session) || Session <- ToDelete],
  C = erlang:send_after(?CLEAN, self(), clean),
  {noreply, State#sess{clean_timer = C}};

handle_info({'DOWN', Ref, _, _Pid, _}, #sess{} = State) ->
  % ?D({deleting,_Pid,session}),
  [delete_session(Session) ||
      Session <- ets:select(flu_session:table(),
                            ets:fun2ms(fun(#session{ref = R} = S) when R == Ref -> S end))],
  {noreply, State};

handle_info(_Info, #sess{} = State) ->
  {noreply, State}.


terminate(_,_) -> ok.

recheck_connected(State) ->
  Now = flu:now_ms(),
  Refreshing = ets:select(flu_session:table(), ets:fun2ms(
    fun(#session{auth_time = A, last_access_time = Last, pid = P} = S) 
      when P =/= undefined andalso Now > Last + A -> S
    end
  )),
  % ?debugFmt("going to refresh: ~p", [Refreshing]),
  [Pid ! refresh_auth || #session{pid = Pid} <- Refreshing],
  State.



