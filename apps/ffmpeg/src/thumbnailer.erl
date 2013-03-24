-module(thumbnailer).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-export([start_link/1, jpeg/2, media_info/2]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).


start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).


jpeg(Pid, #video_frame{flavor = keyframe, codec = h264} = F) ->
  gen_server:call(Pid, F, 10000).


media_info(_Pid, undefined) ->
  {error, no_media_info};
media_info(Pid, #media_info{} = MI) ->
  gen_server:call(Pid, MI).

-record(thumb, {
  port,
  path,
  name,
  n = 1,
  waiting = [],
  config
}).

init([Options]) ->
  Name = case proplists:get_value(name, Options) of
    undefined -> <<"thumbnailer">>;
    N -> iolist_to_binary(["thumbnailer-", N])
  end,
  put(name,Name),
  Path = filename:join(code:lib_dir(ffmpeg, priv), thumbnailer),
  {ok, #thumb{path = Path, name = Name}}.

handle_info({Port, {data, Data}}, #thumb{waiting = W, port = Port} = T) ->
  case erlang:binary_to_term(Data) of
    {jpeg, N, Jpeg} ->
      case lists:keytake(N, 1, W) of
        {value, {N,From}, W1} -> 
          gen_server:reply(From, {ok, Jpeg}),
          {noreply, T#thumb{waiting = W1}};
        false ->
          {noreply, T}
      end
  end;

handle_info(Msg, #thumb{} = T) ->
  lager:info("Unknown message ~p", [Msg]),
  {noreply, T}.



handle_call(#media_info{streams = Streams}, _From, #thumb{config = OldConfig, port = OldPort, name = Name, path = Path} = T) ->
  case lists:keyfind(h264, #stream_info.codec, Streams) of
    #stream_info{config = Config} when Config =/= OldConfig ->
      case OldPort of
        undefined -> ok;
        _ -> erlang:port_close(OldPort)
      end,
      Port = erlang:open_port({spawn_executable, Path}, [{packet,4},{arg0,Name},binary,exit_status]),
      erlang:port_command(Port, erlang:term_to_binary({config, Config})),
      Reply = receive
        {Port, {data, R}} -> erlang:binary_to_term(R)
      after
        1000 -> error(timeout_thumbnailer_config)
      end,
      {reply, Reply, T#thumb{config = Config, port = Port}};
    _ ->
      {reply, {error, no_video_stream}, T}
  end;

handle_call(#video_frame{}, _From, #thumb{port = undefined} = T) ->
  {reply, {error, not_configured_thumbnailer}, T};

handle_call(#video_frame{codec = h264, flavor = keyframe, body = Body}, From, #thumb{port = Port, n = N, waiting = W} = T) ->
  erlang:port_command(Port, erlang:term_to_binary({jpeg,N,Body})),
  {noreply, T#thumb{waiting = [{N,From}|W], n = N+1}}.

terminate(_,_) -> ok.




