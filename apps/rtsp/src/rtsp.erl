%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%%
%%% License is GPL
%%%---------------------------------------------------------------------------------------
-module(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([start_server/3, stop_server/1]).
-export([read/2]).

-define(TIMEOUT, 1000).

start_server(Port, Name, Callback) ->
  ranch:start_listener(Name, 10, ranch_tcp, [{port, Port}], rtsp_listener, [Callback, []]).

stop_server(Name) ->
  ranch:stop_listener(Name).

-type rtsp_request() :: {rtsp,request, {Method::binary(), URL::binary()}, Headers::list(), Body::binary() | undefined}.
-type rtsp_response() :: {rtsp,response, {Code::integer(), Status::binary()}, Headers::list(), Body::binary() | undefined}.
-type rtsp_rtp() :: {rtsp, rtp, Channel::integer(), undefined, Body::binary()}.


-spec read(Socket::inet:socket(), Data::binary()) -> 
  {ok, rtsp_request(), Rest::binary()} | 
  {ok, rtsp_response(), Rest::binary()} | 
  {ok, rtsp_rtp(), Rest::binary()} | 
  {error, Error::term()}.

read(_Socket, <<$$, Channel, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtsp, rtp, Channel, undefined, RTP}, Rest};

read(Socket, <<$$, _>> = Data) ->
  RequiredBytes = case Data of
    _ when size(Data) < 4 -> 4 - size(Data);
    <<$$, _Channel, Length:16, Rest/binary>> -> Length - size(Rest)
  end,
  {ok, Bin} = gen_tcp:recv(Socket, RequiredBytes, ?TIMEOUT),
  read(Socket, <<Data/binary, Bin/binary>>);


read(Socket, Data) when is_port(Socket), is_binary(Data) ->
  case binary:split(Data, <<"\r\n">>) of
    [<<"RTSP/1.0 ", Response/binary>>, After] -> read_response(Socket, Response, After);
    [RequestLine, After] -> read_request(Socket, RequestLine, After);
    [_] ->
      inet:setopts(Socket, [{packet,line},{active,false}]),
      case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Line} when size(Data) > 0 ->
          read(Socket, <<Data/binary, Line/binary>>);
        {ok, Line} when size(Data) == 0 ->
          read(Socket, Line)
      end
  end.


read_request(Socket, RequestLine, Data) ->
  case binary:split(RequestLine, <<" ">>, [global]) of
    [Request, URL, _Protocol] ->
      {Headers, Rest} = read_headers(Socket, Data),
      inet:setopts(Socket, [{active,false},{packet,raw}]),
      {Body, Rest1} = read_body(Socket, Headers, Rest),
      {ok, {rtsp,request, {Request, URL}, Headers, Body}, Rest1};
    _Else ->
      {error, {invalid_rtsp_request, RequestLine}}
  end.

read_response(Socket, ResponseLine, Data) ->
  case re:run(ResponseLine, "(\\d+) (.*)$", [{capture,all_but_first,binary}]) of
    {match, [Code_, Message]} ->
      Code = list_to_integer(binary_to_list(Code_)),
      {Headers, Rest} = read_headers(Socket, Data),
      inet:setopts(Socket, [{active,false},{packet,raw}]),
      {Body, Rest1} = read_body(Socket, Headers, Rest),
      {ok, {rtsp,response,{Code,Message},Headers,Body}, Rest1};
    nomatch ->
      {error, {invalid_rtsp_response, ResponseLine}}
  end.


read_body(Socket, Headers, Data) ->
  case proplists:get_value(<<"Content-Length">>, Headers) of
    undefined -> {undefined, Data};
    Length_ ->
      Length = list_to_integer(binary_to_list(Length_)),
      if Data == <<>> ->
        {ok, B} = gen_tcp:recv(Socket, Length, 3*?TIMEOUT),
        {B, <<>>};
      size(Data) == Length ->
        {Data, <<>>};
      size(Data) < Length ->
        {ok, B} = gen_tcp:recv(Socket, Length - size(Data), 3*?TIMEOUT),
        {<<Data/binary, B/binary>>, <<>>};
      size(Data) > Length ->
        erlang:split_binary(Data, Length)
      end
  end.




read_headers(_Socket, <<"\r\n", Rest/binary>>) ->
  {[], Rest};

read_headers(Socket, Data) ->
  case binary:split(Data, <<"\r\n">>) of
    [Header, After] ->
      [Key, Value] = binary:split(Header, <<": ">>),
      {Headers, Rest} = read_headers(Socket, After),
      {[{Key,Value}|Headers], Rest};
    [_] ->
      inet:setopts(Socket, [{packet,line},{active,false}]),
      case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Line} when size(Data) > 0 ->
          read_headers(Socket, <<Data/binary, Line/binary>>);
        {ok, Line} when size(Data) == 0 ->
          read_headers(Socket, Line)
      end
  end.
















