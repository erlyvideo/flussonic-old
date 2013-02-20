-module(traffic_collector).
-include("log.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/0]).
-export([stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([linux/0, bsd/0]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(traffic, {
  os,
  stats,
  start_at,
  interval,
  n
}).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

stats() ->
  Stats1 = lists:foldl(fun({{Iface,T},I,O}, Stats_) ->
    case lists:keyfind(Iface,1,Stats_) of
      {Iface, S} ->
        lists:keystore(Iface, 1, Stats_, {Iface, [{T,I,O}|S]});
      false ->
        lists:keystore(Iface, 1, Stats_, {Iface, [{T,I,O}]})
    end
  end, [], ets:tab2list(pulse_traffic)),

  Stats2 = [ [{iface,Iface},{traffic,group_traffic(Traffic)}] || {Iface, Traffic} <- Stats1],
  Stats2.

group_traffic(Traffic) ->
  calculate_speed(group_traffic(lists:sort(Traffic), 60)).

group_traffic([], _) -> [];
group_traffic([{T1,I1,O1},{T2,I2,O2}|Traffic], Period) when T1 div Period == T2 div Period ->
  group_traffic([{T1,I1+I2,O1+O2}|Traffic], Period);

group_traffic([{T1,I1,O1}|Traffic], Period) ->
  [{T1,I1,O1}|group_traffic(Traffic, Period)].

calculate_speed([{T1,_,_},{T2,I2,O2}|Traffic]) ->
  Delta = T2 - T1,
  [[{time,T2},{input,I2*8 div (Delta*1024)},{output,O2*8 div (Delta*1024)}] | calculate_speed([{T2,I2,O2}|Traffic]) ];

calculate_speed([_]) -> [];
calculate_speed([]) -> [].


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
  StartAt = os:timestamp(),
  Interval = 5000,
  self() ! collect,
  {ok, #traffic{os = OS, stats = [], start_at = StartAt, n = 1, interval = Interval}}.

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
handle_info(collect, #traffic{os = OS, stats = Stats1, n = N, start_at = StartAt, interval = Interval} = Server) ->
  Now = flu:now(),

  MomentStats = ?MODULE:OS(),
  Stats2 = lists:foldl(fun({Iface, NewIbytes, NewObytes} = S, Stats_) ->
    case lists:keyfind(Iface, 1, Stats_) of
      {Iface, PrevIbytes, PrevObytes} ->
        Ibytes = NewIbytes - PrevIbytes,
        Obytes = NewObytes - PrevObytes,
        pulse:network_traffic(Now, Iface, Ibytes, Obytes),
        lists:keystore(Iface, 1, Stats_, S);
      false ->
        lists:keystore(Iface, 1, Stats_, S)
    end
  end, Stats1, MomentStats),

  RealDelta = timer:now_diff(os:timestamp(), StartAt) div 1000,
  NeedDelta = N*Interval,
  Sleep = if NeedDelta < RealDelta -> 0;
    true -> NeedDelta - RealDelta end,
  erlang:send_after(Sleep, self(), collect),
  {noreply, Server#traffic{n = N+1, stats = Stats2}};

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
  [{list_to_binary(Iface),Ibytes,Obytes} || {Iface,Ibytes,Obytes} <- lists:ukeysort(1,IfaceStats), Ibytes > 0, Obytes > 0].



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
  [{list_to_binary(Iface),Ibytes,Obytes} || {Iface,Ibytes,Obytes} <- Lines5, Ibytes > 0, Obytes > 0].
  


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
    {<<"en1">>, 18536931, 2512375}
    ,{<<"en3">>, 3482280, 589330}
    ,{<<"lo0">>, 5156531, 5156531}
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
    {<<"eth0">>, 5232390240044, 330435663363}
    ,{<<"lo">>, 17553298465162, 17553298465162}
  ], linux(Netstat)).


% 
% linux_example() ->
%   "Inter-|   Receive                                                |  Transmit
%    face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
%       lo:146350984573 100878706    0    0    0     0          0         0 146350984573 100878706    0    0    0     0       0          0
%     eth0:48465236392 63972235    0    0    0     0          0         0 47004089579 57288202    0    0    0     0       0          0
%   venet0:   39600     600    0    0    0     0          0         0        0       0    0    0    0     0       0          0
% ".
