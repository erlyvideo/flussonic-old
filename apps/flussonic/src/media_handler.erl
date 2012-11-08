%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        media handler
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlyvideo.
%%%
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(media_handler).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-export([check_sessions/3]).

-export([lookup_name/4]).

% -export([backend_request/4]).

init({_Any,http}, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_,_) ->
  ok.

handle(Req, Opts) ->
  try handle1(Req, Opts) of
    Reply -> Reply
  catch
    throw:{return, Code, Msg} ->
      {ok, R1} = cowboy_req:reply(Code, [], [Msg, "\n"], Req),
      {ok, R1, undefined};
    throw:{return,Code,Headers,Msg} ->
      {ok, R1} = cowboy_req:reply(Code, Headers, [Msg, "\n"], Req),
      {ok, R1, undefined};
    Class:Reason ->
      {ok, R1} = cowboy_req:reply(500, [], ["Internal server error\n", io_lib:format("~p:~p~n~p~n", [Class, Reason, erlang:get_stacktrace()])], Req),
      {ok, R1, undefined}
  end.

% 1. Extract stream name from path
% 1.1 also extract prefix, function and headers from path
% 2. Check session if enabled
% 3. call function
% 4. return reply
handle1(Req, Opts) ->
  {MFA, ReplyHeaders, Name} = lookup_name(Req, Opts),
  autostart(MFA, Name, Opts),
  call_mfa(MFA, ReplyHeaders, Name, update_cookie(Req)).

name_or_pi(Opts, []) ->
  proplists:get_value(name, Opts);

name_or_pi(_Opts, Acc) ->
  flu:join(lists:reverse(Acc), "/").

lookup_name(Req, Opts) ->
  {PathInfo, _} = cowboy_req:path_info(Req),
  lookup_name(PathInfo, Opts, Req, []).

no_cache() ->
  [{<<"Cache-Control">>, <<"no-cache">>},{<<"Pragma">>, <<"no-cache">>}].

lookup_name(PathInfo, Opts, Req, Acc) ->
  DefaultModule = proplists:get_value(module, Opts),
  case PathInfo of
    [<<"manifest.f4m">>] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      {{DefaultModule, hds_manifest, []}, [{<<"Content-Type">>, <<"text/xml">>}|no_cache()], Stream};
    [<<"bootstrap">>] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      {{DefaultModule, bootstrap, []}, no_cache(), Stream};
    [<<"hds">>, <<"lang-", Lang/binary>>, SegmentPath] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "Seg(\\d+)-Frag(\\d+)", [{capture,all_but_first,list}]),
      {{DefaultModule, hds_lang_segment, [Lang, list_to_integer(Fragment)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [<<"hds">>, _Bitrate, SegmentPath] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "Seg(\\d+)-Frag(\\d+)", [{capture,all_but_first,list}]),
      {{DefaultModule, hds_segment, [list_to_integer(Fragment)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [<<"index.m3u8">>] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hls">>} | Opts]),
      {{DefaultModule, hls_playlist, []}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], Stream};
    [<<"hls">>, SegmentPath] ->
      Root = proplists:get_value(root, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {match, [Number]} = re:run(SegmentPath, "(\\d+)\\.ts", [{capture,all_but_first,list}]),
      {{DefaultModule, hls_segment, [to_b(Root), to_i(Number)]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
    [<<"archive-", FromDurationSpec/binary>>] ->
      {match, [From, Duration, Extension]} = re:run(FromDurationSpec, "(\\d+)-(\\d+)\\.(\\w+)", [{capture, all_but_first, binary}]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      Function = case Extension of
        <<"mp4">> -> mp4;
        <<"ts">> -> mpeg_file
      end,
      {{dvr_handler, Function, [to_b(Root), to_i(From), to_i(Duration), Req]}, [], name_or_pi(Opts, Acc)};
    [<<"save-mp4-", FromDurationSpec/binary>>] ->
      {match, [From, Duration]} = re:run(FromDurationSpec, "(\\d+)-(\\d+)", [{capture, all_but_first, binary}]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {FileName, _Req1} = cowboy_req:qs_val(<<"file">>, Req),
      File = re:replace(FileName, "\\.\\.", "", [{return,binary}]),
      {{dvr_handler, save_mp4, [to_b(Root), to_i(From), to_i(Duration), File]}, [{<<"Content-Type">>, <<"text/plain">>}], name_or_pi(Opts, Acc)};
    [_Year, _Month, _Day, _Hour, _Minute, <<_Second:2/binary, "-", _Duration:4/binary, ".ts">>] ->
      Root = proplists:get_value(dvr, Opts), % here Root may be undefined, because live is served here also
      {{hls_dvr_packetizer, segment, [to_b(Root), filename:join(PathInfo)]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
    [_Year, _Month, _Day, _Hour, _Minute, <<_Second:2/binary, "-", _Duration:5/binary, ".ts">>] ->
      Root = proplists:get_value(dvr, Opts), % here Root may be undefined, because live is served here also
      {{hls_dvr_packetizer, segment, [to_b(Root), filename:join(PathInfo)]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
    [<<_Year:4/binary,"/", _Month:2/binary, "/", _Day:2/binary, "/", _Hour:2/binary, "/", _Minute:2/binary, "/", _Second:2/binary, "-", _Duration:4/binary, ".ts">> = Seg] ->
      Root = proplists:get_value(dvr, Opts), % here Root may be undefined, because live is served here also
      {{hls_dvr_packetizer, segment, [to_b(Root), Seg]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
    [<<_Year:4/binary,"/", _Month:2/binary, "/", _Day:2/binary, "/", _Hour:2/binary, "/", _Minute:2/binary, "/", _Second:2/binary, "-", _Duration:5/binary, ".ts">> = Seg] ->
      Root = proplists:get_value(dvr, Opts), % here Root may be undefined, because live is served here also
      {{hls_dvr_packetizer, segment, [to_b(Root), Seg]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
    [<<"timeshift">>, From] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_handler, timeshift, [to_b(Root), to_i(From), Req]}, [{<<"Content-Type">>, <<"video/MP2T">>}|no_cache()], Stream};
    [<<"archive">>, From, Duration, <<"mpegts">>] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_handler, mpeg_stream, [to_b(Root), to_i(From), to_duration(Duration), Req]}, [{<<"Content-Type">>, <<"video/MP2T">>}|no_cache()], Stream};
    [<<"archive">>, From, Duration, <<"manifest.f4m">>] ->
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_session, hds_manifest, [to_b(Root), to_i(From), to_duration(Duration)]}, [{<<"Content-Type">>, <<"text/xml">>}|no_cache()], Stream};
    [<<"archive">>, From, Duration, <<"index.m3u8">>] ->
      {Query, _} = cowboy_req:qs(Req),
      throw({return, 302, [{<<"Location">>, <<"/", (name_or_pi(Opts,Acc))/binary, "/index-", From/binary, "-", Duration/binary, ".m3u8?", Query/binary>>}], <<"Redirect\n">>});
    [<<"index-", IndexSpec/binary>>] ->
      {match, [From, Duration]} = re:run(IndexSpec, "(\\d+)-(\\w+)\\.m3u8", [{capture, all_but_first, list}]),
      Root = proplists:get_value(dvr, Opts),
      Stream = check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hls">>} | Opts]),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", Stream]}),
      % {dvr_session, hls_abs_playlist, [list_to_binary(Root), list_to_integer(From), list_to_integer(Duration)], [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}], name_or_pi(Opts, Acc)};
      {{hls_dvr_packetizer, playlist, [to_b(Root), to_i(From), to_duration(Duration)]}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], Stream};
    [<<"archive">>, From, Duration, _Bitrate, <<"Seg", SegmentPath/binary>>] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "(\\d+)-Frag(\\d+)", [{capture,all_but_first,binary}]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_session, hds_fragment, [to_b(Root), to_i(From), to_i(Duration), to_i(Fragment)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [Else|PathInfo1] ->
      lookup_name(PathInfo1, Opts, Req, [Else|Acc]);
    [] ->
      throw({return, 415, ["undefined postfix ", name_or_pi([], Acc)]})
  end.


autostart({Module,_F,_A}, Name, Opts) ->
  case erlang:function_exported(Module, autostart, 2) of
    true ->
      Reply = Module:autostart(Name, Opts),
      wait4(Name, 5),
      Reply;
    false ->
      ok
  end.

wait4(_Name, 0) ->
  false;

wait4(Name, Count) ->
  case flu_media:find(Name) of
    undefined ->
      timer:sleep(100),
      wait4(Name, Count - 1);
    _ ->
      ok
  end.

call_mfa({M,F,A}, ReplyHeaders, Name, Req) ->
  {_Time, Result} = timer:tc(M, F, [Name|A]),
  % ?D({M,F,_Time}),
  case Result of
    {done, R1} ->
      {ok, R1, undefined};
    {ok, Reply} ->
      {ok, R1} = cowboy_req:reply(200, ReplyHeaders, Reply, Req),
      {ok, R1, undefined};
    undefined ->
      {ok, R1} = cowboy_req:reply(404, [], "No playlist found\n", Req),
      {ok, R1, undefined};
    {error, no_segment} ->
      {ok, R1} = cowboy_req:reply(404, [], "No segment found\n", Req),
      {ok, R1, undefined};
    {error, Error} ->
      {ok, R1} = cowboy_req:reply(500, [], iolist_to_binary(["Error: ", io_lib:format("~p~n", [Error]), "\n"]), Req),
      {ok, R1, undefined};
    {return, Code, Msg} ->
      {ok, R1} = cowboy_req:reply(Code, [], iolist_to_binary([Msg, "\n"]), Req),
      {ok, R1, undefined}
  end.



to_i(B) when is_binary(B) -> list_to_integer(binary_to_list(B));
to_i(B) when is_list(B) -> list_to_integer(B);
to_i(B) when is_integer(B) -> B.

to_b(B) when is_binary(B) -> B;
to_b(L) when is_list(L) -> list_to_binary(L);
to_b(undefined) -> undefined.


to_duration(B) ->
  case re:run(B, "^(\\d+)$", []) of
    {match, _} -> to_i(B);
    nomatch -> binary_to_existing_atom(to_b(B), latin1)
  end.

%%% All sessions code is beneath

update_cookie(Req) ->
  case erlang:erase(<<"session_cookie">>) of
    undefined -> Req;
    V ->
      cowboy_req:set_resp_cookie(<<"session">>, V, [{max_age, 10 * 60}], Req)
  end.

retrieve_token(Req0) ->
  case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
    {undefined, Req1} -> cowboy_req:cookie(<<"session">>, Req1, undefined);
    V -> V
  end.

check_sessions(Req, Name, Opts) ->
  case proplists:get_value(sessions, Opts) of
    undefined -> Name;    % no backend specified
    URL -> check_sessions0(URL, Name, Req, proplists:get_value(type, Opts, <<"http">>))
  end.

check_sessions0(URL, Name0, Req0, Type) ->
  % case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
  case retrieve_token(Req0) of
    {undefined, _} -> throw({return, 403, "no_token_passed"}); % no token specified
    {Token, Req1} ->
      Ip = peer_addr(Req1),
      {Referer, _} = cowboy_req:header('Referer', Req1),
      Identity = [{token,Token},{name,Name0},{ip,Ip}],
      Options = [{type,Type},{referer,Referer}],
      case flu_session:verify(URL, Identity, Options) of
        {ok, NewName} ->
          NewName;
        {error, Code, Reply} ->
          throw({return,Code, Reply})
      end
  end.

%% Temporary workaround for cowboy bug in 0.6
peer_addr(Req1) ->
  {ForwardedForRaw, _Req2} = cowboy_req:header('X-Forwarded-For', Req1),
  ForwardedFor = case ForwardedForRaw of
    undefined ->
      undefined;
    ForwardedForRaw ->
      case re:run(ForwardedForRaw, "^(?<first_ip>[^\\,]+)",
          [{capture, [first_ip], binary}]) of
        {match, [FirstIp]} -> FirstIp;
        _Any -> undefined
      end
  end,
  case ForwardedFor of
    undefined ->
      {PeerAddr, _} = cowboy_req:peer_addr(Req1),
      list_to_binary(inet_parse:ntoa(PeerAddr));
    _ ->
      ForwardedFor
  end.




