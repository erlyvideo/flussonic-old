%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Starts in-process http stream
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(http_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_REDIRECTS,5).

-export([request/2, request_body/2, head/2]).
-export([calculate_redirected_url/2]).


open_socket(URL, Options) ->
  MaxRedirects = proplists:get_value(max_redirects, Options, ?MAX_REDIRECTS),
  make_request_with_redirect(URL, Options, MaxRedirects).
    
make_request_with_redirect(_URL, _Options, 0) ->
  {error, too_many_redirects};

make_request_with_redirect(URL, Options, RedirectsLeft) ->
  NoRedirect = proplists:get_value(noredirect, Options,false),
  case make_raw_request(URL, Options) of
    {http, Socket, Code} when Code >= 200 andalso Code < 300 ->
      {ok, Code, [{redirected_url, URL}], Socket};
    {http, Socket, Code} when (Code >= 301 orelse Code == 302) andalso NoRedirect  == true ->
      {ok, Code, [], Socket};
    {http, Socket, Code} when Code == 301 orelse Code == 302 ->
      Timeout = proplists:get_value(timeout, Options, 3000),
      case wait_for_headers(Socket, [], Timeout) of
        {ok, Headers} ->
	        Location = proplists:get_value('Location', Headers),
          NewURL = calculate_redirected_url(URL, Location),
          make_request_with_redirect(NewURL, Options, RedirectsLeft - 1);
        {error, Reason} ->
          {error, Reason}
      end;
    {http, _Socket, Code} ->
      {error, {http_code, Code}};
    {tcp_closed, _Socket} ->
      make_request_with_redirect(URL, lists:keydelete(socket, 1, Options), RedirectsLeft);
    {error, Reason} ->
      {error, Reason}
  end.

calculate_redirected_url(URL, Location) ->
  case re:run(Location, "^/(.*)", [{capture,all_but_first,list}]) of
    {match, _} ->
      {match, [Host]} = re:run(URL, "^([^:]+://[^/]+)", [{capture,all_but_first,binary}]),
      iolist_to_binary(io_lib:format("~s~s", [Host, Location]));
    _ ->
      iolist_to_binary(Location)
  end.  

make_raw_request(URL, Options) ->
  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {_HostPort, Path} = http_uri2:extract_path_with_query(URL),

  RequestPath = case proplists:get_value(send_hostpath, Options, false) of
    true -> URL;
    false -> Path
  end,
  PortSpec = 
    case Port of
      80 -> "";
      _ -> ":"++integer_to_list(Port)
    end,

  case find_or_open_socket(Host, Port, Options) of
    {error,Reason} ->
      {error,Reason};
    {ok,NewSock} ->
      case make_request(Host, [{request_path, RequestPath},{port_spec,PortSpec} | Options]) of
	{ok, Request} ->
	  send_request(NewSock, Request,Options);
	{error,Reason} ->
	  {error, Reason}
      end
  end.

find_or_open_socket(Host,Port,Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000), 
  case proplists:get_value(socket, Options) of
    undefined ->
      gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}, {recbuf, 65536}, inet, {reuseaddr,true}], Timeout);
    OldSocket ->
      case inet:setopts(OldSocket, [{packet,http_bin},{active,false}]) of
        ok ->
          {ok, OldSocket};
        {error, _} ->
          (catch gen_tcp:close(OldSocket)),
          gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}, {recbuf, 65536}, inet, {reuseaddr,true}], Timeout)
      end
  end.

make_request(Host,Options) ->
  Method = 
    case proplists:get_value(method, Options, get) of
      Meth when is_atom(Meth) -> string:to_upper(erlang:atom_to_list(Meth))
    end,
  PortSpec = proplists:get_value(port_spec, Options),
  RequestPath = proplists:get_value(request_path, Options,""),
  {StartAcc, Body} = 
    case proplists:get_value(body, Options) of 
      undefined when Method == "POST" ->
	{[{"Content-Length", "0"}], []};
      undefined ->
	{[], []};
      RawBody ->
	{[{"Content-Length", integer_to_list(iolist_size(RawBody))}], RawBody}
    end,
  Headers = 
    lists:foldl(
      fun
	({range,{Start,End}}, Acc) ->
	  [{"Range",lists:flatten(io_lib:format("bytes=~p-~p",[Start,End-1]))} | Acc];
	({basic_auth, {Login, Password}}, Acc) ->
	  [{"Authorization", "Basic " ++ base64:encode_to_string(lists:flatten(io_lib:format("~s:~s", [Login, Password])))} | Acc];
	(_, Acc)->
	  Acc
      end, proplists:get_value(headers, Options, []) ++ StartAcc, Options
     ),
  {ok,iolist_to_binary([Method, " ", RequestPath, " HTTP/1.1\r\nHost: ", Host, PortSpec, "\r\n", [io_lib:format("~s: ~s\r\n", [Key, Value]) || {Key,Value} <- Headers], "\r\n", Body])}.

send_request(Socket, Request,Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  case gen_tcp:send(Socket, Request) of
    ok ->
      case inet:setopts(Socket, [{active, once}]) of
	ok ->
	  receive_response(Socket,Timeout);
	SetoptsError ->
	  SetoptsError
      end;
    SendError ->
      SendError
  end.

receive_response(Socket,Timeout) ->
  receive
    {http, Socket, {http_response, _Version, Code, _Reply}} ->
      {http, Socket, Code};
    {tcp_closed, Socket} ->
      {error, tcp_closed};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  after
    Timeout ->
      gen_tcp:close(Socket),
      {error, timeout}
  end.

request(URL,Options) when is_binary(URL) ->
  request(binary_to_list(URL),Options);

request(URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  case open_socket(URL, Options) of
    {ok, Code, Headers1, Socket} ->
      case wait_for_headers(Socket, [], Timeout) of
        {ok, Headers} ->
          ok = inet:setopts(Socket, [{active, false},{packet,raw}]),
    	    {ok, {Socket, Code, Headers ++ Headers1}};
	      {error, Reason} ->
	        (gen_tcp:close(Socket)),
	        {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

request_body(URL, Options) ->
  case request(URL, [{body_request,true}|Options]) of
    {ok, {Socket, Code, Headers}} ->
      case proplists:get_value('Content-Length', Headers) of
        undefined ->
          case proplists:get_value('Transfer-Encoding', Headers) of
            <<"chunked">> ->
	      handle_response(fun() -> get_chunked_body(Socket) end,Socket,Code,Headers,Options);
            _ ->
              case proplists:get_value('Connection', Headers) of
                <<"close">> ->
                  handle_response(fun() -> gen_tcp:recv(Socket,0) end,Socket,Code,Headers,Options);
                _ ->
                  gen_tcp:close(Socket),
                  {error, no_length}
              end
          end;
        Length ->
	  handle_response(fun() -> gen_tcp:recv(Socket, to_i(Length)) end,Socket,Code,Headers,Options)
      end;
    Else ->
      Else
  end.  

handle_response(F, Socket, Code, Headers,Options) ->
  case F() of
    {ok, Body} ->
      case proplists:get_value(keepalive, Options, true) of
	      true -> ok;
	      false -> gen_tcp:close(Socket)
      end,
      {ok, {Socket, Code, Headers, Body}};
    Error ->
      Error
  end.

to_i(L) when is_list(L) -> list_to_integer(L);
to_i(B) when is_binary(B) -> to_i(binary_to_list(B));
to_i(I) when is_number(I) -> I.
  

get_chunked_body(Socket) ->
  get_chunked_body(Socket, []).

get_chunked_body(Socket, Acc) ->
  ok = inet:setopts(Socket, [{packet,line}]),
  case gen_tcp:recv(Socket, 0) of
    {ok,ChunkHeader} ->
      ok = inet:setopts(Socket, [{packet,raw}]), 
      ChunkSize = parse_size_chunk(ChunkHeader),   
      case get_next_chunk(Socket, ChunkSize) of
	end_of_body ->
	  {ok,iolist_to_binary(lists:reverse(Acc))};	
	{ok, Data} ->
	  get_chunked_body(Socket,[Data|Acc]);
	NextChunkError ->
	  NextChunkError
      end;
    RecvError ->
      RecvError
  end. 

parse_size_chunk(ChunkHeader) ->
  case re:run(ChunkHeader, "^([\\d\\w]+)", [{capture,all_but_first, list}]) of
    {match, [Count]} ->
      list_to_integer(Count, 16);
    nomatch ->
      0 
  end.

get_next_chunk(_Socket, 0) ->
  end_of_body;

get_next_chunk(Socket, Length) ->
  gen_tcp:recv(Socket, Length).

head(URL, Options) ->
  request(URL, [{method,head}|Options]).
  
wait_for_headers(Socket, Headers, Timeout) ->
  ok = inet:setopts(Socket, [{active,once}]),
  receive
    {http, Socket, {http_header, _, Header, _, Value}} ->
      wait_for_headers(Socket, [{Header, Value}|Headers], Timeout);
    {http, Socket, http_eoh} ->
      {ok, lists:reverse(Headers)};
    {tcp_closed, Socket} ->
      {error, tcp_closed};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  after
    Timeout -> 
      gen_tcp:close(Socket),
      {error, Timeout}
  end.




