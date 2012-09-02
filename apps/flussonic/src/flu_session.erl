-module(flu_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-define(TIMEOUT, 120000).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([find_session/3, new_session/4, update_session/1, url/1]).
-export([stats/0]).
-export([list/0, clients/0]).
-include_lib("stdlib/include/ms_transform.hrl").


-export([backend_request/4]).

% -record(session, {
%   id,
%   expire,
%   path,
%   name,
%   options = []
% }).


-record(session, {
  uid,
  token,
  ip,
  url,
  flag,
  type, %  :: <<"hds">>|<<"hls">>|<<"http">>
  created_at,
  expire_time,
  last_access_time,
  bytes_sent,
  options = []
}).


backend_request(URL, Token, Ip, Name) ->
  RequestURL = lists:flatten(io_lib:format("~s?url=~s&token=~s&ip=~s", [URL, Name, Token, Ip])),
  case http_stream:request_body(RequestURL, [{noredirect, true}, {keepalive, false}]) of
    {ok, {_Socket, Code, Headers, _Body}} ->
      Opts0 = case proplists:get_value(<<"X-AuthDuration">>, Headers) of % session expire
        undefined -> [];
        Value -> [{expire, Value}]
      end,
      case Code of
        200 -> {ok,    Name,                                             lists:merge(Opts0, [{access, granted}])};
        302 -> {ok,    proplists:get_value(<<"X-Name">>, Headers, Name), lists:merge(Opts0, [{access, granted}])};
        403 -> {error, 403,                                              lists:merge(Opts0, [{access, denied}])};
        _ ->   {error, 403,                                              lists:merge(Opts0, [{access, denied}])}
      end;
    {error, _} -> throw({return, 404, "not found"})
  end.

  


clients() ->
  Now = flu:now_ms(),
  Sessions = ets:select(?MODULE, ets:fun2ms(fun(#session{flag = granted} = E) -> E end)),
  [[{ip,IP},{name,URL},{start_at,StartAt},{duration,Now - StartAt},{type,Type}] || #session{ip = IP, url = URL, created_at = StartAt, type = Type} <- Sessions].

list() ->
  flu_rtmp:clients() ++ clients().

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stats() ->
  Streams = ets:foldl(fun(#session{url = Name}, Acc) ->
      dict:update_counter(Name, 1, Acc)
    end, dict:new(), ?MODULE),
  dict:to_list(Streams).

% cookie_name() ->
%   <<"flu_cookie_">>.

url(Session) -> Session#session.url.

find_session(Token, Ip, Url) ->
  Uid = crypto:sha([Token, Ip, Url]),
  case ets:lookup(?MODULE, Uid) of
    [] -> undefined;
    [#session{} = Session] -> Session
  end.

new_session(Token, Ip, Url, Opts) ->
  Flag    = proplists:get_value(access, Opts, denied),
  Expire  = proplists:get_value(expire, Opts, ?TIMEOUT),

  Now = flu:now_ms(),

  Uid = crypto:sha([Token, Ip, Url]),
  Session = #session{uid = Uid, token = Token, ip = Ip, url = Url, created_at = Now,
                     expire_time = Expire, last_access_time = Now, type = proplists:get_value(type, Opts, <<"http">>),
                     flag = Flag, bytes_sent = 0},
  ets:insert(?MODULE, Session),
  erlang:put(<<"session_cookie">>, Token),
  Session.

update_session(Session) ->
  ets:update_element(?MODULE, Session#session.uid, {#session.last_access_time, flu:now_ms()}),
  Session#session.flag.

% find_session(Req) ->
%   {Cookie, Req1} = cowboy_http_req:cookie(cookie_name(), Req),
%   if is_binary(Cookie) ->
%     case ets:lookup(?MODULE, Cookie) of
%       [] -> {undefined, Req1};
%       [#session{} = Session] ->
%         ets:insert(?MODULE, Session#session{expire = expire()}),
%         {Session, Req1}
%     end;
%   true -> {undefined, Req1}
%   end.

% new_session(Req, Name, Options) ->
%   Key = uuid:gen(),
%   Path = proplists:get_value(path, Options, <<"/">>),
%   % ?D({new_session,Key,Name,Path}),
%   {ok, Req1} = cowboy_http_req:set_resp_cookie(cookie_name(), Key, [{max_age, 1200},{path, Path}], Req), %% set it to cookie
%   ets:insert(?MODULE, #session{id = Key, name = Name, path = Path, expire = expire(), options = Options}),
%   {ok, Req1}.

% options(#session{options = Options}) ->
%   Options.


init([]) ->
  ets:new(?MODULE, [public, named_table, {keypos, #session.uid}]),
  timer:send_interval(5000, clean),
  {ok, state}.

handle_call(Call, _From, State) ->
  {reply, {error, Call}, State}.

handle_info(clean, State) ->
  Now = flu:now_ms(),
  ets:select_delete(?MODULE, ets:fun2ms(fun(#session{expire_time = T, last_access_time = Last}) when T + Last < Now -> true end)),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_,_) -> ok.
