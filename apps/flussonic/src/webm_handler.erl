-module(webm_handler).
-behaviour(cowboy_http_handler).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


-export([init/3, handle/2, terminate/3]).
-export([read_loop/2, write_loop/1]).

-record(webm, {
  options,
  method,
  stream,
  buffer = <<>>,
  header = <<>>,
  dts
}).

init(_, Req, Opts) ->
  {Method, Req2} = cowboy_req:method(Req),

  {ok, Req2, #webm{options = Opts, method = Method}}.


handle(Req, #webm{} = State) ->
  try handle0(Req, State) of
    {ok, Req1, _} ->
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req1), undefined};      
    ok ->
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req), undefined}
  catch
    throw:stop ->
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req), undefined};
    throw:{return,Code,Text} ->
      {ok, R1} = cowboy_req:reply(Code, [], Text, Req),
      {ok, R1, undefined};
    exit:normal -> 
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req), undefined};
    exit:timeout ->
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req), undefined};
    Class:Error ->
      lager:error("~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
      {ok, cowboy_req:set([{connection,close},{resp_state,done}], Req), undefined}      
  end.

handle0(Req, #webm{method = <<"POST">>} = State) ->
  {TE, Req1} = cowboy_req:header(<<"transfer-encoding">>, Req),
  TE == <<"chunked">> orelse throw({return, 401, <<"need chunked body">>}),
  {ok, Req3} = cowboy_req:reply(200, [], <<>>, Req1),
  ?MODULE:read_loop(Req3, State),
  {ok, Req3, State};

handle0(Req, #webm{method = <<"GET">>} = _State) ->
  flu_stream:subscribe(<<"webm">>),
  {ok, Req1} = cowboy_req:chunked_reply(200, [{<<"Content-Type">>,<<"video/webm">>}], Req),
  #media_info{streams = [#stream_info{codec = vp8, config = Header}|_]} = flu_stream:media_info(<<"webm">>),
  ok = cowboy_req:chunk(Header, Req1),
  ?MODULE:write_loop(Req1),
  ok.


write_loop(Req) ->
  receive
    #video_frame{codec = vp8, body = Body} ->
      ?D({send,iolist_size(Body)}),
      ok = cowboy_req:chunk(Body, Req),
      ?MODULE:write_loop(Req)
  after
    10000 ->
      ok
  end.


read_loop(Req, #webm{buffer = Buffer, stream = undefined} = State) ->
  {ok, Chunk, Req1} = cowboy_req:stream_body(Req),
  State1 = init_header(State#webm{buffer = <<Buffer/binary, Chunk/binary>>}),
  ?MODULE:read_loop(Req1, State1);



read_loop(Req, #webm{buffer = Buffer} = State) ->
  case cowboy_req:stream_body(Req) of
    {ok, Chunk, Req1} ->
      State1 = send_frames(State#webm{buffer = <<Buffer/binary, Chunk/binary>>}),
      ?MODULE:read_loop(Req1, State1);
    {done, Req1} ->
      {ok, Req1};
    {error, Error} ->
      lager:error("http error capturing webm over http: ~p", [Error]),
      {ok, Req}
  end.

init_header(#webm{buffer = Bin, header = Header} = State) ->
  case mkv:next_atom(Bin) of
    {ok, cluster, _, _Rest} ->
      {ok, Stream} = flu_stream:autostart(<<"webm">>, [{hls,false},{hds,false},{clients_timeout,false}]),
      flu_stream:set_source(Stream, self()),
      Stream ! #media_info{streams = [#stream_info{codec = vp8, track_id = 1, content = video, config = Header}, 
        #stream_info{codec = vorbis, track_id = 2, content = audio}]},
      State#webm{stream = Stream, dts = 0};
    {ok, ID, _, Rest} ->
      S = size(Bin) - size(Rest),
      {Tag, Rest} = erlang:split_binary(Bin, S),
      ?D(ID),
      init_header(State#webm{buffer = Rest, header = <<Header/binary, Tag/binary>>});
    more ->
      State
  end.

send_frames(#webm{buffer = Bin, stream = Stream, dts = DTS} = Webm) ->
  case mkv:next_atom(Bin) of
    {ok, cluster, _, Rest} ->
      S = size(Bin) - size(Rest),
      {Cluster, Rest} = erlang:split_binary(Bin, S),
      Frame = #video_frame{flavor = keyframe, content = video, codec = vp8, track_id = 1, dts = DTS, pts = DTS, body = Cluster},
      flu_stream:publish(Stream, Frame),
      % ?D({cluster, size(Cluster), DTS}),
      send_frames(Webm#webm{buffer = Rest, dts = DTS+1});
    more ->
      Webm
  end.




terminate(_,_,_) ->
  ok.
