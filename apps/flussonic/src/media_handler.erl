%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
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
-export([init/3, handle/2, terminate/3]).

-export([check_sessions/3]).

-export([lookup_name/4]).

% -export([backend_request/4]).

init({_Any,http}, Req, Opts) ->
  {ok, Req, Opts}.

terminate(_,_,_) ->
  ok.

handle(Req, Opts) ->
  {Path, Req1} = cowboy_req:path(Req),
  {Method, Req2} = cowboy_req:method(Req1),
  % lager:notice([{tag,http}],"HTTP ~s ~s",[Method, Path]),
  try handle1(Req2, Opts) of
    Reply ->
      Reply
  catch
    throw:{return, Code, Msg} ->
      % lager:notice([{tag,http}],"HTTP ~s ~s ~s",[Method, Path, Code]),
      {ok, R1} = cowboy_req:reply(Code, [], [Msg, "\n"], Req2),
      {ok, R1, undefined};
    throw:{return,Code,Headers,Msg} ->
      % lager:notice([{tag,http}],"HTTP ~s ~s ~s",[Method, Path, Code]),
      {ok, R1} = cowboy_req:reply(Code, Headers, [Msg, "\n"], Req2),
      {ok, R1, undefined};
    Class:Reason ->
      % lager:notice([{tag,http}],"HTTP ~s ~s ~s",[Method, Path, 500]),
      Stacktrace = erlang:get_stacktrace(),
      lager:error("Error handling HTTP ~s ~s: ~p:~p~n~s", [Method, Path, Class, Reason, [io_lib:format("    ~240p~n", [T]) || T <- Stacktrace]]),
      {ok, R1} = cowboy_req:reply(500, [], ["Internal server error\n", lager_format:format("~p:~p~n~p~n", [Class, Reason, Stacktrace], 10000)], Req2),
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
  call_mfa(MFA, ReplyHeaders, Name, Req).

name_or_pi(Opts, []) ->
  proplists:get_value(name, Opts);

name_or_pi(Opts, Acc) ->
  Acc1 = lists:reverse(Acc),
  Acc2 = case proplists:get_value(prefix, Opts) of
    undefined -> Acc1;
    Prefix -> [Prefix | Acc1]
  end,
  flu:join(Acc2, "/").


full_name(Opts, []) ->
  proplists:get_value(name, Opts);

full_name(Opts, Acc) ->
  Acc1 = lists:reverse(Acc),
  Acc2 = case proplists:get_value(prefix, Opts) of
    undefined ->
      case proplists:get_value(file_prefix, Opts) of
        undefined -> Acc1;
        Prefix -> [Prefix | Acc1]
      end;
    Prefix -> [Prefix | Acc1]
  end,
  flu:join(Acc2, "/").

lookup_name(Req, Opts) ->
  {PathInfo, _} = cowboy_req:path_info(Req),
  lookup_name(PathInfo, Opts, Req, []).

no_cache() ->
  [{<<"Cache-Control">>, <<"no-cache">>},{<<"Pragma">>, <<"no-cache">>}].

track_spec(TrackSpec) ->
  [cowboy_http:digits(D) || D <- binary:split(TrackSpec, <<",">>, [global])].

lookup_name(PathInfo, Opts, Req, Acc) ->
  DefaultModule = proplists:get_value(module, Opts),
  case PathInfo of
    [<<"manifest.f4m">>] ->
      {ok, Token} = ?MODULE:check_sessions(Req, full_name(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Args = case DefaultModule of
        flu_stream when Token =/= undefined -> [Token];
        _ -> []
      end,
      {{DefaultModule, hds_manifest, Args}, [{<<"Content-Type">>, <<"text/xml">>}|no_cache()], name_or_pi(Opts,Acc)};
    [<<"bootstrap">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, full_name(Opts, Acc), [{type, <<"hds">>} | Opts]),
      {{DefaultModule, bootstrap, []}, no_cache(), name_or_pi(Opts,Acc)};
    [<<"hds">>, <<"tracks-", TrackSpec/binary>>, SegmentPath] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "Seg(\\d+)-Frag(\\d+)", [{capture,all_but_first,list}]),
      {{DefaultModule, hds_segment, [list_to_integer(Fragment), track_spec(TrackSpec)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [<<"hds">>, _Bitrate, SegmentPath] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "Seg(\\d+)-Frag(\\d+)", [{capture,all_but_first,list}]),
      {{DefaultModule, hds_segment, [list_to_integer(Fragment)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [<<"tracks-", TrackSpec/binary>>, <<"index.m3u8">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, full_name(Opts, Acc), [{type, <<"hls">>} | Opts]),
      {{DefaultModule, hls_playlist, [track_spec(TrackSpec)]}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], name_or_pi(Opts,Acc)};
    [<<"index.m3u8">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, full_name(Opts, Acc), [{type, <<"hls">>} | Opts]),
      {{DefaultModule, hls_playlist, []}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], name_or_pi(Opts,Acc)};
    [<<"mbr.m3u8">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, full_name(Opts, Acc), [{type, <<"hls">>} | Opts]),
      {{DefaultModule, hls_mbr_playlist, []}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], name_or_pi(Opts,Acc)};
    [<<"tracks-",TrackSpec/binary>>, <<"hls">>, SegmentPath] ->
      Root = proplists:get_value(root, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {match, [Number]} = re:run(SegmentPath, "(\\d+)\\.ts", [{capture,all_but_first,list}]),
      {{DefaultModule, hls_segment, [to_b(Root), to_i(Number), track_spec(TrackSpec)]}, [{<<"Content-Type">>, <<"video/MP2T">>}], name_or_pi(Opts, Acc)};
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
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"export">>} | Opts]),
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
    [<<"timeshift_abs">>, From] ->
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"mpegts">>},{pid, self()} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_handler, timeshift_abs, [to_b(Root), to_i(From), Req]}, [{<<"Content-Type">>, <<"video/MP2T">>}|no_cache()], name_or_pi(Opts, Acc)};
    [<<"timeshift_rel">>, From] ->
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"mpegts">>},{pid, self()} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_handler, timeshift_rel, [to_b(Root), to_i(From), Req]}, [{<<"Content-Type">>, <<"video/MP2T">>}|no_cache()], name_or_pi(Opts, Acc)};
    [<<"archive">>, From, Duration, <<"mpegts">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"mpegts">>},{pid, self()} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_handler, mpeg_stream, [to_b(Root), to_i(From), to_duration(Duration), Req]}, [{<<"Content-Type">>, <<"video/MP2T">>}|no_cache()], name_or_pi(Opts, Acc)};
    [<<"archive">>, From, Duration, <<"manifest.f4m">>] ->
      {ok, Token} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      Args = if Duration == <<"now">> andalso Token =/= undefined -> [Token];
        true -> []
      end,
      {{dvr_session, hds_manifest, [to_b(Root), to_i(From), to_duration(Duration)|Args]}, [{<<"Content-Type">>, <<"text/xml">>}|no_cache()], name_or_pi(Opts, Acc)};
    [<<"archive">>, From, Duration, <<"bootstrap">>] ->
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hds">>} | Opts]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_session, hds_bootstrap, [to_b(Root), to_i(From), to_duration(Duration)]}, no_cache(), name_or_pi(Opts, Acc)};
    [<<"archive">>, From, Duration, <<"index.m3u8">>] ->
      {Query, _} = cowboy_req:qs(Req),
      throw({return, 302, [{<<"Location">>, <<"/", (name_or_pi(Opts,Acc))/binary, "/index-", From/binary, "-", Duration/binary, ".m3u8?", Query/binary>>}], <<"Redirect\n">>});
    [<<"index-", IndexSpec/binary>>] ->
      {match, [From, Duration]} = re:run(IndexSpec, "(\\d+)-(\\w+)\\.m3u8", [{capture, all_but_first, list}]),
      Root = proplists:get_value(dvr, Opts),
      {ok, _} = ?MODULE:check_sessions(Req, name_or_pi(Opts, Acc), [{type, <<"hls">>} | Opts]),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      % {dvr_session, hls_abs_playlist, [list_to_binary(Root), list_to_integer(From), list_to_integer(Duration)], [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}], name_or_pi(Opts, Acc)};
      {{hls_dvr_packetizer, playlist, [to_b(Root), to_i(From), to_duration(Duration)]}, [{<<"Content-Type">>, <<"application/vnd.apple.mpegurl">>}|no_cache()], name_or_pi(Opts, Acc)};
    [<<"archive">>, From, Duration, _Bitrate, <<"Seg", SegmentPath/binary>>] ->
      {match, [_Segment, Fragment]} = re:run(SegmentPath, "(\\d+)-Frag(\\d+)", [{capture,all_but_first,binary}]),
      Root = proplists:get_value(dvr, Opts),
      Root =/= undefined orelse throw({return, 424, ["no dvr root specified ", name_or_pi(Opts, Acc)]}),
      {{dvr_session, hds_fragment, [to_b(Root), to_i(From), to_duration(Duration), to_i(Fragment)]}, [{<<"Content-Type">>, <<"video/f4f">>}], name_or_pi(Opts, Acc)};
    [Else|PathInfo1] ->
      case proplists:get_value(static, Opts) of
        true ->
          % Temporary hack to protect from requests like
          % http://localhost:8080/cam0/archive/1359421400/manifest.f4m
          throw({return, 415, ["undefined postfix"]});
        _ ->
          lookup_name(PathInfo1, Opts, Req, [Else|Acc])
      end;
    [] ->
      throw({return, 415, ["undefined postfix ", name_or_pi([], Acc)]})
  end.


autostart({Module,_F,_A}, Name, Opts) ->
  case code:is_loaded(Module) of
    false -> code:load_file(Module);
    _ -> ok
  end,
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

retrieve_token(Req0) ->
  case cowboy_req:qs_val(<<"token">>, Req0, undefined) of
    {undefined, Req1} -> 
      case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
        {undefined, Req1} ->  cowboy_req:cookie(<<"token">>, Req1, undefined);
        V -> V
      end;
    V -> V
  end.

check_sessions(Req, Name, Opts) ->
  case proplists:get_value(sessions, Opts) of
    undefined -> {ok, undefined};    % no backend specified
    URL -> check_sessions0(URL, Name, Req, proplists:get_value(type, Opts, <<"http">>), Opts)
  end.

check_sessions0(URL, Name0, Req0, Type, Opts) ->
  % case cowboy_req:qs_val(<<"session">>, Req0, undefined) of
  case retrieve_token(Req0) of
    {undefined, _} -> throw({return, 403, "no_token_passed"}); % no token specified
    {Token, Req1} ->
      {PeerAddr, _} = cowboy_req:peer_addr(Req1),
      Ip = list_to_binary(inet_parse:ntoa(PeerAddr)),
      {Referer, _} = cowboy_req:header(<<"referer">>, Req1),
      Identity = [{token,Token},{name,Name0},{ip,Ip}],
      Options = [{type,Type}] ++ case Referer of
        undefined -> [];
        _ -> [{referer,Referer}]
      end ++ case proplists:get_value(pid, Opts) of
        undefined -> [];
        Pid -> [{pid,Pid}]
      end,
      case flu_session:verify(URL, Identity, Options) of
        {ok, _} ->
          {ok, Token};
        {error, Code, Reply} ->
          throw({return,Code, Reply})
      end
  end.




