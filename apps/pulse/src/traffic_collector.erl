-module(traffic_collector).
-include("log.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/0]).
-export([stats/1, ifaces/0, default_iface/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([linux/0, bsd/0]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(entry, {
  iface,
  time,
  ibytes,
  obytes
}).

-record(traffic, {
  os,
  stats
}).

autostart() ->
  case whereis(traffic_collector) of
    undefined -> commercial:start();
    _ -> ok
  end.

stats(Iface) ->
  autostart(),
  case gen_server:call(?MODULE, {stats, Iface}) of
    {ok, Stats} -> parse_stats(Stats);
    Else -> Else
  end.

ifaces() ->
  gen_server:call(?MODULE, ifaces).


default_iface() ->
  gen_server:call(?MODULE, default_iface).

parse_stats(Stats) ->
  Stats1 = lists:ukeysort(1, Stats),
  Precision = if
    length(Stats) < 10 -> 1;
    length(Stats) < 180 -> 10;
    % length(Stats) < 20*60 -> {60, 60};
    % length(Stats) < 4000 -> {60, 60}
    true -> 60
  end,
  Stats2 = group_stats(Stats1, Precision),
  % % Stats2 = Stats1,
  Stats3 = calculate_speed(Stats2, []),
  BaseT = case Stats3 of
    [{T1,_,_}|_] -> T1;
    _ -> 0
  end,
  Stats4 = [{T-BaseT, I*8,O*8} || {T,I,O} <- Stats3],
  {ok, Stats4}.
  % {ok, Stats2}.


group_stats(Stats, Time) ->
  Stats1 = group_stats0([{T div Time, I, O} || {T, I, O} <- Stats], []),
  [{T*Time, I, O} || {T, I, O} <- Stats1].

group_stats0([{T, I1, O1}, {T,I2,O2} | Stats], Acc) when I1 =< I2 andalso O1 =< O2 ->
  group_stats0([{T, I2, O2}|Stats], Acc);

group_stats0([{T1, I1, O1}, {T2,I2,O2} | Stats], Acc) when T1 < T2 andalso I1 =< I2 andalso O1 =< O2 ->
  group_stats0([{T2, I2, O2}|Stats], [{T1,I1,O1}|Acc]);

group_stats0([{T1, I1, O1}], Acc) ->
  group_stats0([], [{T1,I1,O1}|Acc]);

group_stats0([], Acc) ->
  lists:reverse(Acc).

speed(I1, I2, Delta) ->
  (I2 - I1) div (Delta*1024).

calculate_speed([{T1, I1, O1},{T2, I2, O2}|Stats], Acc) when T1 < T2 andalso I1 =< I2 andalso O1 =< O2 ->
  Delta = T2 - T1,
  calculate_speed([{T2,I2,O2}|Stats], [{T1, speed(I1, I2, Delta), speed(O1, O2, Delta)}|Acc]);

calculate_speed([_], Acc) ->
  lists:reverse(Acc).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([]) ->
  OS = case os:cmd("uname") of
    "Linux\n" -> linux;
    _ -> bsd
  end,
  Table = ets:new(?MODULE, [bag, named_table, {keypos, #entry.iface}]),
  timer:send_interval(1000, collect),
  {ok, #traffic{os = OS, stats = Table}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({stats, Iface}, _From, #traffic{stats = Stats} = Server) ->
  Stat = ets:select(Stats, ets:fun2ms(fun(#entry{time = T, iface = If, ibytes = Ibytes, obytes = Obytes}) when If == Iface ->
    {T, Ibytes, Obytes}
  end)),
  {reply, {ok, Stat}, Server};

handle_call(default_iface, _From, #traffic{stats = Stats} = Server) ->
  Stat1 = ets:foldl(fun(#entry{iface = If, ibytes = I, obytes = O}, Acc) ->
    lists:ukeymerge(1, [{If, I, O}], Acc)
  end, [], Stats),
  Stat2 = [{If, I+O} || {If, I, O} <- Stat1],
  Iface = case lists:reverse(lists:keysort(2, Stat2)) of
    [{If, _}|_] -> If;
    _ -> undefined
  end,
  {reply, Iface, Server};

handle_call(ifaces, _From, #traffic{stats = Stats} = Server) ->
  Ifaces = lists:usort(ets:select(Stats, ets:fun2ms(fun(#entry{iface = If}) -> If end))),
  {reply, Ifaces, Server};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(collect, #traffic{os = OS, stats = Stats} = Server) ->
  Now = flu:now(),
  Limit = Now - 3600,
  ets:insert(Stats, [#entry{iface = Iface, time = Now, ibytes = Ibytes, obytes = Obytes} || {Iface,Ibytes,Obytes} <- ?MODULE:OS()]),
  ets:select_delete(Stats, ets:fun2ms(fun(#entry{time = T}) -> T < Limit end)),
  {noreply, Server};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


linux() ->
  Path = "/proc/net/dev",
  {ok, F} = file:open(Path, [raw,read]),
  Content = collect_file(F),
  file:close(F),
  linux(Content).

collect_file(F) ->
  case file:read(F, 1024) of
    {ok, S} -> S ++ collect_file(F);
    eof -> []
  end.

parse_linux_headers(Header1, Header2) ->
  [_|Parts] = [list_to_atom(string:to_lower(string:strip(Part))) || Part <- string:tokens(Header1, "|")],
  [_|HeadersDesc] = [Part || Part <- string:tokens(Header2, "|")],
  Headers1 = lists:zipwith(fun(Part, Header) ->
    [{Part, list_to_atom(string:strip(H))} || H <- string:tokens(Header, " ")]
  end, Parts, HeadersDesc),
  lists:flatten(Headers1).
  
collect_linux_stats(Headers, Lines) ->
  lists:map(fun(Line) ->
    [Iface, LineRest] = [string:strip(P) || P <- string:tokens(Line, ":")],
    RawNumbers = [list_to_integer(P) || P <- string:tokens(LineRest, " ")],
    Info = lists:zip(Headers, RawNumbers),
    {Iface, proplists:get_value({'receive',bytes}, Info), proplists:get_value({transmit,bytes}, Info)}
  end, Lines).


linux(Content) ->
  [Header1, Header2 | Lines] = [Line || Line <- string:tokens(Content, "\n"), Line =/= ""],
  Headers = parse_linux_headers(Header1, Header2),
  IfaceStats = collect_linux_stats(Headers, Lines),
  [Iface || {_,Ibytes,Obytes} = Iface <- lists:ukeysort(1,IfaceStats), Ibytes > 0, Obytes > 0].



bsd() ->
  bsd(os:cmd("netstat -ib")).

bsd(Output) ->
  [HeaderLine|Content] = string:tokens(Output,"\n"),
  Headers = [list_to_atom(string:to_lower(string:strip(H))) || H <- string:tokens(HeaderLine, " ")],
  HeaderCount = length(Headers),
  Lines1 = lists:map(fun(Line) ->
    [string:strip(P) || P <- string:tokens(Line, " ")]
  end, Content),
  Lines2 = lists:filter(fun(Line) ->
    length(Line) == HeaderCount
  end, Lines1),
  Lines3 = [lists:zip(Headers, Line) || Line <- Lines2],
  Lines4 = lists:map(fun(Line) ->
    {proplists:get_value(name,Line), 
    list_to_integer(proplists:get_value(ibytes,Line)),
    list_to_integer(proplists:get_value(obytes,Line))}
  end, Lines3),
  Lines5 = lists:ukeysort(1, Lines4),
  [{Iface,Ibytes,Obytes} || {Iface,Ibytes,Obytes} <- Lines5, Ibytes > 0, Obytes > 0].
  


bsd_test() ->
  Netstat = 
"Name  Mtu   Network       Address            Ipkts Ierrs     Ibytes    Opkts Oerrs     Obytes  Coll
lo0   16384 <Link#1>                         15726     0    5156531    15726     0    5156531     0
lo0   16384 localhost   fe80:1::1            15726     -    5156531    15726     -    5156531     -
lo0   16384 127           localhost          15726     -    5156531    15726     -    5156531     -
lo0   16384 localhost   ::1                  15726     -    5156531    15726     -    5156531     -
gif0* 1280  <Link#2>                             0     0          0        0     0          0     0
stf0* 1280  <Link#3>                             0     0          0        0     0          0     0
en0   1500  <Link#4>    c4:2c:03:2f:ee:55        0     0          0        0     0        342     0
en1   1500  <Link#5>    60:33:4b:20:7b:aa    19980     0   18536931    18983     0    2512375     0
p2p0  2304  <Link#6>    02:33:4b:20:7b:aa        0     0          0        0     0          0     0
fw0   4078  <Link#7>    60:33:4b:ff:fe:90:94:60        0     0          0        0     0        346     0
vnic0 1500  <Link#8>    00:1c:42:00:00:08        0     0          0        0     0          0     0
vnic0 1500  10.211.55/24  10.211.55.2            0     -          0        0     -          0     -
vnic1 1500  <Link#9>    00:1c:42:00:00:09        0     0          0        0     0          0     0
vnic1 1500  10.37.129/24  10.37.129.2            0     -          0        0     -          0     -
en3   1500  <Link#11>   de:2b:61:1c:ef:cd     4452     0    3482280     4962     0     589330     0
en3   1500  maxbp.local fe80:b::dc2b:61ff     4452     -    3482280     4962     -     589330     -
en3   1500  172.20.10/28  172.20.10.3         4452     -    3482280     4962     -     589330     -
",
  ?assertEqual([
    {"en1", 18536931, 2512375}
    ,{"en3", 3482280, 589330}
    ,{"lo0", 5156531, 5156531}
  ], bsd(Netstat)).

linux_test() ->
  Netstat = 
"Inter-|   Receive                                                |  Transmit
 face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
    lo:17553298465162 4087349897    0    0    0     0          0         0 17553298465162 4087349897    0    0    0     0       0          0
  eth0:5232390240044 3712861019    0    0    0     0          0         0 330435663363 2415076634    0   92    0     0       0          0
  tun0:       0       0    0    0    0     0          0         0        0       0    0    0    0     0       0          0
",
  ?assertEqual([
    {"eth0", 5232390240044, 330435663363}
    ,{"lo", 17553298465162, 17553298465162}
  ], linux(Netstat)).


% 
% linux_example() ->
%   "Inter-|   Receive                                                |  Transmit
%    face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
%       lo:146350984573 100878706    0    0    0     0          0         0 146350984573 100878706    0    0    0     0       0          0
%     eth0:48465236392 63972235    0    0    0     0          0         0 47004089579 57288202    0    0    0     0       0          0
%   venet0:   39600     600    0    0    0     0          0         0        0       0    0    0    0     0       0          0
% ".
