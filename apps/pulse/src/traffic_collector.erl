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
  minute_timer,
  second_timer,
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
  end, [], lists:sort(ets:tab2list(pulse_traffic_min))),

  Stats2 = [ [{iface,Iface},{minute,minute_traffic(Traffic)},{hour,hour_traffic(Iface)}] || {Iface, Traffic} <- Stats1],
  Stats2.

minute_traffic([]) -> [];
minute_traffic([{T,I,O}|Traffic]) ->
  minute_traffic(Traffic, [{T,I,O}], 1).


minute_traffic(_, Acc, 60) ->
  [[{time,T},{input,I*8 div 1024},{output,O*8 div 1024}] || {T,I,O} <- Acc];

minute_traffic([{T2,_I2,_O2}|_]= Traffic, [{T1,_I1,_O1}|_] = Acc, Count) when T2 < T1 - 1 ->
  minute_traffic(Traffic, [{T1-1,0,0}|Acc], Count+1);

minute_traffic([{T2,I2,O2}|Traffic], [{T1,_I1,_O1}|_] = Acc, Count) when T2 == T1 - 1 ->
  minute_traffic(Traffic, [{T2,I2,O2}|Acc], Count+1);

minute_traffic([], Acc, _) ->
  minute_traffic([], Acc, 60).



hour_traffic(Iface) ->
  Stats1 = ets:select(pulse_traffic_hour, ets:fun2ms(fun({{If,Min},I,O}) when Iface == If ->
    {Min,I,O}
  end)),

  calculate_speed(lists:sort(Stats1)).


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


current_second() ->
  {Mega, Sec, Micro} = os:timestamp(),
  Milli = Micro div 1000,
  Delay = 500 + 1000 - Milli,
  {Mega*1000000 + Sec, Delay}.

init([]) ->
  OS = case os:cmd("uname") of
    "Linux\n" -> linux;
    _ -> bsd
  end,
  StartAt = os:timestamp(),
  Interval = 1000,
  Minute = erlang:send_after(60000, self(), flush_minute),
  {N, Delay} = current_second(),
  Second = erlang:send_after(Delay, self(), collect),
  {ok, #traffic{os = OS, stats = [], start_at = StartAt, n = N, interval = Interval, minute_timer = Minute, second_timer = Second}}.

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
handle_info(collect, #traffic{os = OS, stats = Stats1, second_timer = OldSecond} = Server) ->
  erlang:cancel_timer(OldSecond),
  MomentStats = ?MODULE:OS(),
  {N, Sleep} = current_second(),

  Stats2 = lists:foldl(fun({Iface, NewIbytes, NewObytes} = S, Stats_) ->
    case lists:keyfind(Iface, 1, Stats_) of
      {Iface, PrevIbytes, PrevObytes} ->
        Ibytes = NewIbytes - PrevIbytes,
        Obytes = NewObytes - PrevObytes,
        pulse:network_traffic(N, Iface, Ibytes, Obytes),
        lists:keystore(Iface, 1, Stats_, S);
      false ->
        lists:keystore(Iface, 1, Stats_, S)
    end
  end, Stats1, MomentStats),

  Second = erlang:send_after(Sleep, self(), collect),
  {noreply, Server#traffic{n = N, stats = Stats2, second_timer = Second}};


handle_info(flush_minute, #traffic{minute_timer = OldMinute} = Server) ->
  erlang:cancel_timer(OldMinute),
  {Mega,Sec,_} = os:timestamp(),
  Minute = ((Mega*1000000 + Sec) div 60) - 1,

  Stats1 = ets:select(pulse_traffic_min, ets:fun2ms(fun({{Iface,Sec_},I,O}) when Sec_ div 60 == Minute ->
    {Iface,I,O}
  end)),
  Stats2 = lists:foldl(fun({Iface,I,O}, Stats_) ->
    case lists:keyfind(Iface,1,Stats_) of
      {Iface,I1,O1} -> lists:keystore(Iface,1,Stats_, {Iface,I1+I,O1+O});
      false -> [{Iface,I,O}|Stats_]
    end
  end, [], Stats1),

  [ets:insert(pulse_traffic_hour, {{Iface,Minute*60}, I,O}) || {Iface, I,O} <- Stats2],

  ets:select_delete(pulse_traffic_min, ets:fun2ms(fun({{_Iface,Sec_},_,_}) when Sec_ div 60 < Minute ->
    true
  end)),
  % ets:select_delete(pulse_traffic_min, [{{{'_','$1'},'_','_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]),

  NewMinute = erlang:send_after(60000, self(), flush_minute),
  {noreply, Server#traffic{minute_timer = NewMinute}};

handle_info({set_interval,Interval}, #traffic{} = Server) ->
  {noreply, Server#traffic{interval = Interval}};

handle_info(_Info, #traffic{} = State) ->
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
