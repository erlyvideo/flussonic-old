%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2012 Max Lapshin
%%% @doc        UDP Mpeg-TS reader
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%---------------------------------------------------------------------------------------
-module(mpegts_udp).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").

-define(CMD_OPEN, 1).
-define(CMD_ACTIVE_ONCE, 5).

-export([open/2, close/1, active_once/1]).

open(Port, Options) ->
  Path = case code:lib_dir(mpegts,priv) of
    {error, _} -> "priv";
    LibDir -> LibDir
  end,
  Loaded = case erl_ddll:load_driver(Path, mpegts_udp) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> {error, {could_not_load_driver,erl_ddll:format_error(Error)}}
  end,
  case Loaded of
    ok ->
      Socket = open_port({spawn, mpegts_udp}, [binary]),
      Multicast = case proplists:get_value(multicast_ttl, Options) of
        undefined -> <<>>;
        _ -> case proplists:get_value(ip, Options) of
        undefined -> <<>>;
        MC when is_list(MC) -> 
          {ok, {I1,I2,I3,I4}} = inet_parse:address(MC),
          <<I1, I2, I3, I4>>;
        {I1,I2,I3,I4} -> 
          <<I1, I2, I3, I4>>
      end end,
      <<"ok">> = port_control(Socket, ?CMD_OPEN, <<Port:16, Multicast/binary>>),
      erlang:port_set_data(Socket, inet_udp),
      {ok, Socket};
    Else ->
      Else
  end.


active_once(Port) ->
  <<"ok">> = port_control(Port, ?CMD_ACTIVE_ONCE, <<>>),
  ok.


close(Socket) ->
  erlang:port_close(Socket).




