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
-export([read/1]).
-export([header/2, to_lower/1, dump/1]).

-define(TIMEOUT, 1000).

start_server(Port, Name, Callback) ->
  ranch:start_listener(Name, 10, ranch_tcp, [{port, Port}], rtsp_listener, [Callback, []]).

stop_server(Name) ->
  ranch:stop_listener(Name).

-type rtsp_request() :: {rtsp,request, {Method::binary(), URL::binary()}, Headers::list(), Body::binary() | undefined}.
-type rtsp_response() :: {rtsp,response, {Code::integer(), Status::binary()}, Headers::list(), Body::binary() | undefined}.
-type rtsp_rtp() :: {rtsp, rtp, Channel::integer(), undefined, Body::binary()}.


-spec read(Data::binary()) -> 
  {ok, rtsp_request(), Rest::binary()} | 
  {ok, rtsp_response(), Rest::binary()} | 
  {ok, rtsp_rtp(), Rest::binary()} | 
  {more, Rest::binary()} |
  {more, Rest::binary(), Bytes::non_neg_integer()} |
  {error, Error::term()}.


read(Bin) ->
  try read0(Bin)
  catch
    throw:more -> more;
    throw:{more,Bytes} -> {more, Bytes}
  end.



read0(<<$$, Channel, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtsp, rtp, Channel, undefined, RTP}, Rest};

read0(<<$$, _/binary>> = Data) ->
  RequiredBytes = case Data of
    _ when size(Data) < 4 -> 4 - size(Data);
    <<$$, _Channel, Length:16, Rest/binary>> -> Length - size(Rest)
  end,
  {more, RequiredBytes};

read0(<<"RTSP/1.0 ", _/binary>> = Data) ->
  case binary:split(Data, [<<"\r\n">>, <<"\n">>]) of
    [<<"RTSP/1.0 ", Response/binary>>, After] -> read_response(Response, After);
    [_] -> more
  end;

read0(Data) when size(Data) < 20 ->
  more;

read0(Data) ->
  case re:run(Data, "^([A-Z]+) ") of
    {match, _} ->
      case binary:split(Data, [<<"\r\n">>,<<"\n">>]) of
        [RequestLine, After] -> read_request(RequestLine, After);
        [_] -> more
      end;
    nomatch ->
      {error, desync}
  end.


read_request(RequestLine, Data) ->
  case binary:split(RequestLine, <<" ">>, [global]) of
    [Request, URL, _Protocol] ->
      {Headers, Rest} = read_headers(Data),
      {Body, Rest1} = read_body(Headers, Rest),
      {ok, {rtsp,request, {Request, URL}, Headers, Body}, Rest1};
    _Else ->
      {error, {invalid_rtsp_request, RequestLine}}
  end.

read_response(ResponseLine, Data) ->
  case re:run(ResponseLine, "(\\d+) (.*)$", [{capture,all_but_first,binary}]) of
    {match, [Code_, Message]} ->
      Code = list_to_integer(binary_to_list(Code_)),
      {Headers, Rest} = read_headers(Data),
      {Body, Rest1} = read_body(Headers, Rest),
      {ok, {rtsp,response,{Code,Message},Headers,Body}, Rest1};
    nomatch ->
      {error, {invalid_rtsp_response, ResponseLine}}
  end.


read_body(Headers, Data) ->
  case header(<<"Content-Length">>, Headers) of
    undefined -> {undefined, Data};
    Length_ ->
      Length = list_to_integer(binary_to_list(Length_)),
      case Data of
        <<Body:Length/binary, Rest/binary>> -> {Body, Rest};
        _ -> throw({more, Length - size(Data)})
      end
  end.




read_headers(<<"\r\n", Rest/binary>>) ->
  {[], Rest};

read_headers(<<"\n", Rest/binary>>) ->
  {[], Rest};

read_headers(Data) ->
  case binary:split(Data, [<<"\r\n">>, <<"\n">>]) of
    [Header, After] ->
      [Key, Value] = binary:split(Header, <<": ">>),
      {Headers, Rest} = read_headers(After),
      {[{Key,Value}|Headers], Rest};
    [_] ->
      throw(more)
  end.


to_lower(Atom) when is_atom(Atom) -> to_lower(atom_to_binary(Atom, latin1) );
to_lower(Bin) -> << <<(if C >= $A andalso C =< $Z -> C bor 2#00100000; true -> C end)/integer>> || <<C>> <= Bin >>.

header(Header, Headers) when is_atom(Header) ->
  header(atom_to_binary(Header, latin1), Headers);
header(Header, Headers) when is_binary(Header) ->
  header0(to_lower(Header), Headers).

header0(_, []) -> undefined;
header0(Header, [{K,V}|Headers]) ->
  case to_lower(K) of
    Header -> V;
    _ -> header0(Header, Headers)
  end.


dump({rtsp, response, {Code, Status}, Headers, Body}) ->
  iolist_to_binary(["RTSP/1.0 ", integer_to_list(Code), " ", Status, "\n",
    [[K, ": ",V,"\n"] || {K,V} <- Headers],
    "\n",
    case Body of undefined -> ""; _ -> Body end
  ]).










