%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        MPEG TS demuxer module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlang-mpegts.
%%% 
%%% erlang-mpegts is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-mpegts is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(mpegts_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").
-include("../include/mpegts.hrl").
-include("../include/mpegts_psi.hrl").
% -include("mpegts_reader.hrl").

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_PAYLOAD, 16#100000000).

-record(reader, {
  consumer,
  decoder,
  socket,
  options,
  counters,
  url,
  byte_counter = 0,
  media_info,
  delay_for_config = true,
  delayed_frames = [],
  current_time
}).



-define(PID_TYPE(Pid), case lists:keyfind(Pid, #stream.pid, Pids) of #stream{codec = h264} -> "V"; _ -> "A" end).


-export([start_link/1, set_socket/2]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([media_info/1]).


% load_nif() ->
%   Load = erlang:load_nif(code:lib_dir(mpegts,priv)++ "/mpegts_reader", 0),
%   io:format("Load mpegts_reader: ~p~n", [Load]),
%   ok.


start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

set_socket(Reader, Socket) when is_pid(Reader) andalso is_port(Socket) ->
  ok = gen_tcp:controlling_process(Socket, Reader),
  gen_server:call(Reader, {set_socket, Socket}).




init([]) ->
  init([[]]);

init([Options]) ->
  Consumer = case proplists:get_value(consumer, Options) of
    undefined -> undefined;
    Cons when is_pid(Cons) ->
      erlang:monitor(process, Cons),
      Cons
  end,
  URL = proplists:get_value(url, Options),
  put(name, {mpegts_reader,URL}),
  {ok, #reader{consumer = Consumer, url = URL, options = Options, counters = dict:new(), decoder = mpegts_decoder:init()}}.



handle_call({set_socket, Socket}, _From, #reader{} = Decoder) ->
  ok = inet:setopts(Socket, [{packet,raw},{active,once}]),
  % ?D({passive_accepted, Socket}),
  {reply, ok, Decoder#reader{socket = Socket}};

handle_call(media_info, _From, #reader{media_info = MI} = Decoder) ->
  {reply, MI, Decoder};

handle_call(connect, _From, #reader{options = Options, url = URL} = Decoder) ->
  Timeout = proplists:get_value(timeout, Options, 2000),
  {Schema, _, _Host, _Port, _Path, _Query} = http_uri2:parse(URL),
  if Schema == udp orelse Schema == udp2 ->
      {ok, Socket} = connect_udp(URL),
      % ?DBG("MPEG-TS reader connected to \"~s\"", [URL]),
  	  {reply, ok, Decoder#reader{socket = Socket}};
    true ->
      case  http_stream:request(URL, [{timeout,Timeout}]) of 
      	{ok,{Socket,_Code,_Header}} ->
      	  ok = inet:setopts(Socket, [{packet,raw},{active,once}]),
      	  % ?D({connected, URL, Socket}),
      	  {reply, ok, Decoder#reader{socket = Socket}};
      	{error,_Reason} ->
      	  {stop,normal,{error, _Reason},Decoder}
      end
  end;
    
handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.
  
handle_info({'DOWN', _Ref, process, Consumer, _Reason}, #reader{consumer = Consumer} = State) ->
  {stop, normal, State};
  
handle_info({tcp, Socket, Bin}, #reader{} = Reader) ->
  inet:setopts(Socket, [{active,once}]),
  handle_info({input_data, Socket, Bin}, Reader);

handle_info({udp, Socket, _IP, _InPortNo, Bin}, #reader{counters = Counters, url = _URL} = Reader) ->
  Data = iolist_to_binary([Bin|fetch_udp(Socket)]),
  % Counters2 = validate_mpegts(Data, Counters, URL),
  Counters2 = Counters,
  inet:setopts(Socket, [{active,once}]),
  handle_info({input_data, Socket, Data}, Reader#reader{counters = Counters2});

handle_info({mpegts_udp, Socket, Data}, #reader{counters = Counters, url = _URL} = Reader) ->
  % Counters2 = validate_mpegts(Data, Counters, URL),
  Counters2 = Counters,
  mpegts_udp:active_once(Socket),
  handle_info({input_data, Socket, Data}, Reader#reader{counters = Counters2});  

handle_info({input_data, _Socket, Bin}, #reader{consumer = Consumer, decoder = Decoder, media_info = MI1} = Reader) ->
  try mpegts_decoder:decode(Bin, Decoder) of
    {ok, Decoder2, Frames} ->
      % ?debugFmt("frames: ~p", [[{Codec,Flavor} || #video_frame{codec = Codec, flavor = Flavor} <- Frames]]),
      MI2 = video_frame:define_media_info(MI1, Frames),
      % ?debugFmt("mi: ~p", [MI2]),
      MI3 = case MI2 of
        #media_info{streams = []} -> undefined;
        #media_info{} when MI2 =/= MI1 -> Consumer ! MI2;
        _ -> MI2
        % undefined -> MI2;
        % #media_info{} -> Consumer ! MI2
      end,
      case MI3 of
        undefined -> ok;
        _ -> [gen_server:call(Consumer, Frame) || Frame <- Frames]
      end,
      {noreply, Reader#reader{decoder = Decoder2, media_info = MI3}}
  catch
    error:{desync_adts,_Bin} ->
      {noreply, Reader};
    Class:Error ->
      ?debugFmt("~p:~p~n~240p~n",[Class,Error, erlang:get_stacktrace()]),
      % ?D({udp_mpegts,Class,Error, erlang:get_stacktrace()}),
      {stop, {Class, Error}, Reader}
  end;

handle_info({tcp_closed, _Socket}, #reader{} = Reader) ->
  {stop, normal, Reader};

handle_info(Else, #reader{} = Reader) ->
  {stop, {unknown_message, Else}, Reader}.


fetch_udp(Socket) ->
  case gen_udp:recv(Socket, 2*1024*1024, 0) of
    {ok, {_, _, Bin}} -> [Bin|fetch_udp(Socket)];
    {error, timeout} -> [];
    {error, Error} -> error({udp,Error})
  end.


% validate_mpegts(Data, Counters, URL) ->
%   Packets = [{Pid,Counter} || <<16#47, _:3, Pid:13, _:4, Counter:4, _:184/binary>> <= Data],
%   Counters2 = lists:foldl(fun({Pid,Counter}, Counters1) ->
%     case dict:find(Pid, Counters1) of
%       {ok, Value} when (Value + 1) rem 16 == Counter -> ok;
%       {ok, Value} when (Value + 1) rem 16 =/= Counter ->
%         {Mega,Sec,_} = os:timestamp(),
%         CurrentMinute = (Mega*1000000+Sec) div 60,
%         case get(error_happened_at) of
%           CurrentMinute -> ok;
%           _ -> ?ERR("Pid ~p desync ~B -> ~B (~s)", [Pid,Value,Counter,URL])
%         end,
%         put(error_happened_at, CurrentMinute)
%         ;
%       error -> ok
%     end,
%     dict:store(Pid,Counter,Counters1)
%   end, Counters, Packets),
%   Counters2.


% send_media_info_if_needed(#reader{consumer = Consumer, media_info = #media_info{}} = Decoder, Frames) when length(Frames) > 0 ->
%   case get(sent_media_info) of
%     true -> ok;
%     _ -> put(sent_media_info, true), Consumer ! media_info(Decoder)
%   end;

% send_media_info_if_needed(_, _) -> 
%   ok.


  

handle_cast(Cast, Decoder) ->
  {stop, {unknown_cast, Cast}, Decoder}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

    
connect_udp(URL) ->
  {Proto, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Addr} = inet_parse:address(Host),
  % Yes. Here is active,true because UDP can loose data
  Common = [binary,{active,once},{recbuf,2*1024*1024},inet,{ip,Addr}],
  Options = case is_multicast(Addr) of
    true ->
      Multicast = [{reuseaddr,true},{multicast_ttl,4},{multicast_loop,true}],
      Common ++ Multicast;
    false ->
      Common
  end,

  case Proto of
    udp ->
      {ok, Socket} = gen_udp:open(Port, Options),
      case is_multicast(Addr) of
        true -> inet:setopts(Socket,[{add_membership,{Addr,{0,0,0,0}}}]);
        false -> ok
      end,
      inet:setopts(Socket, [{active,once}]),
      process_flag(priority, high),
      {ok, Socket};
    udp2 ->
      case mpegts_udp:open(Port, Options) of
        {ok, Socket} ->
          process_flag(priority, high),
          mpegts_udp:active_once(Socket),
          {ok, Socket};
        {error, _E} = Error ->
          ?D(Error),
          Error
      end
  end.


is_multicast(Addr) when is_tuple(Addr) ->
  Leading = element(1, Addr),
  case tuple_size(Addr) of
    4 -> (Leading bsr 4) == 14; % IPv4
    8 -> (Leading bsr 8) == 255 % IPv6
  end.


media_info(Decoder) when is_pid(Decoder) -> gen_server:call(Decoder, media_info).


