%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        file handling
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
-module(flu_file).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([start_link/2, autostart/2, media_info/1]).

-export([hds_manifest/2, hds_fragment/3, hds_fragment/4]).
-export([hls_playlist/2, hls_playlist/3, hls_mbr_playlist/2, hls_segment/3, hls_segment/4]).


-export([get/2]).
-export([list/0, json_list/0]).
-export([read_gop/4, read_gop/3, read_gop/2, keyframes/1]).

-export([init_reader/2, reader_loop/1, read_request/5]).

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/mp4.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-define(SEGMENT_DURATION,10000).

-record(state, {
  readers = [],
  jobs = [],
  starting_workers = [],
  name,
  disk_path,
  options,
  keyframes,
  media_info,
  hds_manifest,
  timeout,
  hls_playlist,
  hls_mbr_playlist,
  hls_length=?SEGMENT_DURATION
}).


list() ->
  [begin
    Pid = proplists:get_value(pid,Info),
    {dictionary,Dict} = erlang:process_info(Pid, dictionary),
    Workers = proplists:get_value(worker_count,Dict,0),
    Clients = proplists:get_value(client_count,Info,0),
    Meta = [
      {worker_count,Workers},
      {client_count,Clients},
      {pid,Pid}
    ],
    {Name,Meta}
  end || {Name,Info} <- gen_tracker:list(flu_files)].

json_list() ->
  Files = [begin
    [{name,Name}] ++ [{K,V} || {K,V} <- Info, lists:member(K,[worker_count,client_count])]
  end || {Name,Info} <- list()],
  [{files,Files},{event,'file.list'}].


autostart(File, _) when is_pid(File) ->
  {ok, File};

autostart(Path, Name) ->
  MFA = {flu_file, start_link, [Name, [{path,Path}]]},
  gen_tracker:find_or_open(flu_files, {Name, MFA, temporary, 200, worker, []}).

start_link(Name, Options) ->
  proc_lib:start_link(?MODULE, init, [[Name, Options]]).

media_info(File) -> get(File, media_info).

get(Path, File, Key) when is_binary(File) ->
  case autostart(Path, File) of
    {ok, Pid} -> get(Pid, Key);
    {error, _} = Error -> Error;
    undefined -> undefined
  end.

get(Pid, Key) ->
  make_call(Pid, Key).

keyframes(Pid) when is_pid(Pid) -> 
  get(Pid, keyframes);

keyframes(Name) when is_binary(Name) ->
  {ok, Pid} = gen_tracker:find(flu_files, Name),
  keyframes(Pid).


hds_manifest(Path, Name) when is_binary(Path),is_binary(Name) ->
  get(Path, Name, hds_manifest).


hds_fragment(Path, Name, Tracks, Fragment) when is_binary(Path),is_binary(Name),is_list(Tracks),is_integer(Fragment) ->
  case autostart(Path, Name) of
    {ok, Pid} -> 
      case disk_request(Pid, hds_fragment, Fragment, Tracks) of
        {error, {busy, Error}} -> lager:info("busy error reading ~s/~p~p: ~p", [Name,Tracks,Fragment,Error]),{error,busy};
        Else -> Else
      end;
    {error, _} = Error -> Error
  end.

hds_fragment(Path, Name, Fragment) when is_binary(Path),is_binary(Name),is_integer(Fragment) ->
  case autostart(Path, Name) of
    {ok, Pid} -> disk_request(Pid, hds_fragment, Fragment, undefined);
    {error, _} = Error -> Error
  end.



make_call(Pid, Call) ->
  make_call(Pid, Call, 5).

make_call(_Pid, _Call, 0) ->
  {error, {busy,too_many_retries}};

make_call(Pid, Call, Retry) ->
  case gen_server:call(Pid, Call, 10000) of
    {error, retry} ->
      timer:sleep(50),
      make_call(Pid, Call, Retry - 1);
    Else ->
      Else
  end.


hls_playlist(Path, Name) when is_binary(Path),is_binary(Name) -> 
  get(Path, Name, hls_playlist).

hls_playlist(Path, Name, Tracks) when is_binary(Path),is_binary(Name),is_list(Tracks) -> 
  get(Path, Name, {hls_playlist, Tracks}).

hls_mbr_playlist(Path, Name) when is_binary(Path),is_binary(Name) -> 
  get(Path, Name, hls_mbr_playlist).

hls_segment(Path, Name, Tracks, Segment) when is_binary(Path),is_binary(Name),is_list(Tracks),is_integer(Segment) ->
  case autostart(Path, Name) of
    {ok, Pid} -> disk_request(Pid, hls_segment, Segment, Tracks);
    {error, _} = Error -> Error
  end.

hls_segment(Path, Name, Segment) when is_binary(Path),is_binary(Name),is_integer(Segment) ->
  case autostart(Path, Name) of
    {ok, Pid} -> disk_request(Pid, hls_segment, Segment, undefined);
    {error, _} = Error -> Error
  end.



disk_request(Pid, Request, Segment, Tracks) when is_pid(Pid) ->
  T1 = os:timestamp(),
  case make_call(Pid, {Request, Segment, Tracks}) of
    {ok, Headers, Bin, Duration, ReadTime} ->
      T2 = os:timestamp(),
      Size = erlang:external_size(Bin),
      SegmentTime = timer:now_diff(T2,T1),
      pulse:read_segment(Size, Duration, ReadTime, SegmentTime),
      ReadHeaders = [{<<"X-DiskTime">>, list_to_binary(integer_to_list(ReadTime))},
      {<<"X-ReadTime">>, list_to_binary(integer_to_list(SegmentTime))}] ++ Headers,
      {ok, ReadHeaders, Bin};
    {error, _} = Error ->
      Error
  end.




read_gop(Root, File, Id) ->
  read_gop(Root, File, undefined, Id).

read_gop(Path, Name, Tracks, Id) when is_binary(Path),is_binary(Name),is_integer(Id)->
  case autostart(Path, Name) of
    {ok, Pid} ->
      case disk_request(Pid, read_gop, Id, Tracks) of
        {ok, _, Gop} -> {ok, Gop};
        {error, _} = Error -> Error
      end;
    Else ->
      Else
  end.

read_gop(Pid, Id) ->
  case disk_request(Pid, read_gop, Id, undefined) of
    {ok, _, Gop} -> {ok, Gop};
    {error, _} = Error -> Error
  end.




init([Name, Options]) ->
  DiskPath = proplists:get_value(path,Options),
  URL = case re:run(Name, "http:/([^/].+)", [{capture,all_but_first,binary}]) of
    {match, [URL2]} -> <<"http://", URL2/binary>>;
    _ when DiskPath =/= undefined -> DiskPath
  end,

  put(name, {flu_file,URL}),
  % File is started ondemand, so we need to check how many active sessions
  % already are asking this file
  ClientCount = flu_session:client_count(Name),
  
  Timeout = proplists:get_value(timeout, Options, 60000),
  gen_tracker:setattr(flu_files, Name, [{client_count,ClientCount}]),

  case proc_lib:start(?MODULE, init_reader, [self(), URL]) of
    {ok, Pid, MediaInfo, Keyframes} ->
      erlang:monitor(process, Pid),

      {ok, HDS} = reader_manifest(Pid, hds_manifest),
      {ok, HLS} = case erlang:module_loaded(hls) of
        true -> reader_manifest(Pid, hls_playlist);
        false -> {ok, undefined}
      end,

      State = #state{
        name = Name,
        disk_path = URL,
        timeout = Timeout,
        options = Options,
        media_info = MediaInfo,
        keyframes = Keyframes, 
        hds_manifest = HDS,
        hls_playlist = HLS,
        readers = [Pid]
      },

      put(worker_count, 1),

      proc_lib:init_ack({ok, self()}),
      lager:notice("open file \"~s\"",[URL]),
      gen_server:enter_loop(?MODULE, [], State, Timeout);
    {error, Error} ->
      Message = case Error of
        enoent -> {return, 404, lists:flatten(io_lib:format("No such file ~s", [Name]))};
        eaccess -> {return, 403, lists:flatten(io_lib:format("Forbidden to open file ~s", [Name]))};
        _ -> {return, 500, lists:flatten(io_lib:format("Error ~p opening file ~s", [Error, Name]))}
      end,
      lager:notice("error opening file \"~s\": ~p",[URL, Error]),
      proc_lib:init_ack({error, Message}),
      ok
  end.




-record(reader, {
  parent,
  format,
  access,
  file,
  headers,
  media_info,
  path,
  reader
}).

init_reader(Parent, Path) ->
  put(name, {file_worker,Path}),
  Access = case re:run(Path, "http://") of
    nomatch -> file;
    _ -> http_file
  end,
  Format = case re:run(Path, "\\.flv$") of
    nomatch -> mp4_reader;
    _ -> flv_reader
  end,
  Options = case Access of
    file -> [read,binary,raw,{read_ahead,5*1024*1024}];
    http_file -> []
  end,
  erlang:monitor(process, Parent),
  case Access:open(Path, Options) of
    {ok, File} ->
      {ok, Reader} = Format:init({Access, File}, []),
      MediaInfo = Format:media_info(Reader),
      Headers = case Access:read_file_info(Path) of
        {ok, #file_info{mtime = Mtime}} when is_tuple(Mtime) ->
          [{<<"Last-Modified">>, list_to_binary(httpd_util:rfc1123_date(Mtime))}];
        _ ->
          []
      end,
      case MediaInfo of
        #media_info{streams = Streams} when length(Streams) > 0 -> ok;
        _ -> proc_lib:init_ack({error, invalid_media_info}), exit(invalid_media_info)
      end,
      Keyframes = video_frame:reduce_keyframes(Format:keyframes(Reader)),
      proc_lib:init_ack({ok, self(), MediaInfo, Keyframes}),
      ?MODULE:reader_loop(#reader{parent = Parent, format = Format, access = Access, path = Path, file = File, 
        headers = Headers, reader = Reader, media_info = MediaInfo});
    {error, _} = Error ->
      proc_lib:init_ack(Error)
  end.

reader_loop(#reader{} = State) ->
  receive
    Message ->
      try handle_reader_message(Message, State) of
        {ok, State1} -> ?MODULE:reader_loop(State1);
        stop -> ok
      catch
        Class:Error ->
          lager:error("File worker error after message ~p with details: ~p:~p\n~p", [Message, Class, Error, erlang:get_stacktrace()]),
          error({Class,Error,erlang:get_stacktrace()})
      end
    after
      60000 ->
        ok
  end.



handle_reader_message(Message, #reader{format = Format, reader = Reader, parent = Parent, path = URL, headers = Headers,
  media_info = #media_info{duration = D} = MI} = State) ->
  case Message of
    {'DOWN', _,_,_,_} -> 
      stop;
    {manifest, hds_manifest} ->
      {ok, HdsManifest} = hds:file_manifest(Format, Reader),
      Parent ! {hds_manifest, HdsManifest},
      {ok, State};
    {manifest, {hls_playlist,Tracks}} ->
      {ok, Playlist} = hls:playlist(Format, Reader, [{name,URL},{tracks,Tracks},{duration,D}]),
      Parent ! {{hls_playlist,Tracks}, Playlist},
      {ok, State};
    {manifest, hls_playlist} ->
      {ok, Playlist} = hls:playlist(Format, Reader, [{name,URL},{duration,D}]),
      Parent ! {hls_playlist, Playlist},
      {ok, State};
    {read_gop, From, SegmentFormat, Fragment, Tracks} ->
      T1 = os:timestamp(),
      Reply = case Format:read_gop(Reader, Fragment, Tracks) of
        {ok, Gop_} -> 
          Gop = limited_gop(Fragment, Gop_),
          T2 = os:timestamp(),
          ReadTime = timer:now_diff(T2,T1),
          Duration = gop_duration(Gop),

          Segment = case SegmentFormat of
            hds_fragment ->
              HasVideo = case Gop of
                [#video_frame{content = video}|_] -> true;
                _ -> false
              end,
              {ok, F4V} = hds:segment(Gop, MI, [{tracks,Tracks},{no_metadata,not HasVideo}]),
              F4V;
            hls_segment ->
              hls:segment(Gop, MI, [{tracks,Tracks}]);
            raw ->
              Gop
          end,
          {ok, Headers, Segment, Duration, ReadTime};
        {error, _} = Error ->
          Error
      end,
      gen_server:reply(From, Reply),
      Parent ! {reader_ready, self()},
      {ok, State};
    Else ->
      error({unknown,Else})
  end.

read_request(Reader, {Caller,CallRef} = Ref, SegmentFormat, Fragment, Tracks) when
  is_pid(Reader), is_pid(Caller),is_reference(CallRef),is_atom(SegmentFormat),is_number(Fragment)  ->
  Reader ! {read_gop, Ref, SegmentFormat, Fragment, Tracks},
  Ref.

reader_manifest(Reader, Manifest) ->
  Reader ! {manifest, Manifest},
  receive
    {Manifest, Bin} -> {ok, Bin}
  after
    1000 -> error({timeout,Manifest})
  end.


handle_info(timeout, State) ->
  {stop, normal, State};

handle_info({reader_ready, Reader}, #state{timeout = Timeout, jobs = Jobs, readers = Readers} = State) ->
  Jobs1 = lists:keydelete(Reader, 1, Jobs),
  % It is very, very important that we put fresh worker in the end of queue
  % If we do not do so, than last workers will shutdown and start again and it is very expensive
  {noreply, State#state{jobs = Jobs1, readers = Readers ++ [Reader]}, Timeout};

% Add here notifying of waiting clients
handle_info({'DOWN', _, _, Reader, Reason}, #state{timeout = Timeout, jobs = Jobs, readers = Readers, starting_workers = Starting} = State) ->
  case Reason of
    normal ->
      NewJobs = lists:keydelete(Reader, 1, Jobs),
      NewReaders = lists:delete(Reader, Readers),
      NewStarting = lists:delete(Reader, Starting),
      put(worker_count, length(NewJobs) + length(NewReaders)),
      % ?D({spare_worker,Reader,gracefully_down, left, get(worker_count)}),
      {noreply, State#state{jobs = NewJobs, readers = NewReaders, starting_workers = NewStarting}, Timeout};
    _ -> {stop, Reason, State}
  end;


handle_info({ack, Pid, {ok, Pid, _, _}}, #state{timeout = Timeout, readers = Readers, jobs = Jobs, starting_workers = Starting} = State) ->
  erlang:monitor(process, Pid),
  put(worker_count, length(Jobs) + length(Readers) + 1),
  % ?D({start_new_worker,State#state.disk_path,get(worker_count)}),
  {noreply, State#state{readers = Readers ++ [Pid], starting_workers = lists:delete(Pid, Starting)}, Timeout};

handle_info({ack, Pid, {error, _}}, #state{timeout = Timeout, starting_workers = Starting} = State) ->
  {noreply, State#state{starting_workers = lists:delete(Pid, Starting)}, Timeout};

handle_info(Info, State) ->
  {stop, {unknown_info,Info}, State}.




schedule_read_request({From, Format, Fragment, Tracks}, #state{timeout = Timeout, readers = [Pid|Readers], jobs = Jobs} = State) ->
  read_request(Pid, From, Format, Fragment, Tracks),
  {noreply, State#state{readers = Readers, jobs = [{Pid,From}|Jobs]}, Timeout};

% We should refuse to start more than 4 spare workers simultaneously
schedule_read_request(_Request, #state{starting_workers = Starting, timeout = Timeout} = State) when length(Starting) > 4 ->
  {reply, {error, retry}, State, Timeout};

schedule_read_request(_Request, #state{jobs = Jobs, readers = [], disk_path = URL, timeout = Timeout, 
    starting_workers = Starting} = State) when length(Jobs) + length(Starting) < 25 -> 
  % ?D(ask_to_retry),
  Pid1 = proc_lib:spawn(?MODULE, init_reader, [self(), URL]),
  Pid2 = proc_lib:spawn(?MODULE, init_reader, [self(), URL]),
  Pid3 = proc_lib:spawn(?MODULE, init_reader, [self(), URL]),
  {reply, {error, retry}, State#state{starting_workers = [Pid1,Pid2,Pid3|Starting]}, Timeout};

schedule_read_request(_Request, #state{timeout = Timeout} = State) ->
  {reply, {error, {busy,too_many_jobs}}, State, Timeout}.



handle_call({read_gop, Fragment, Tracks}, From, #state{} = State) ->
  schedule_read_request({From, raw, Fragment, Tracks}, State);


handle_call({Format, Fragment, Tracks}, From, #state{} = State) 
  when Format == hds_fragment orelse Format == hls_segment ->
  schedule_read_request({From, Format, Fragment, Tracks}, State);





handle_call(media_info, _From, #state{media_info = MI, timeout = Timeout} = State) ->
  {reply, MI, State, Timeout};

handle_call(keyframes, _From, #state{keyframes = Keyframes, timeout = Timeout} = State) ->
  {reply, Keyframes, State, Timeout};






handle_call(hds_manifest, _From, #state{hds_manifest = undefined, readers = Readers} = State) ->
  [Pid|_] = Readers,
  {ok, HdsManifest} = reader_manifest(Pid, hds_manifest),
  gen_tracker:setattr(flu_files, State#state.name, [{hds_manifest, HdsManifest}]),
  handle_call(hds_manifest, _From, State#state{hds_manifest = HdsManifest});

handle_call(hds_manifest, _From, #state{hds_manifest = HdsManifest, timeout = Timeout} = State) ->
  {reply, {ok, HdsManifest}, State, Timeout};




handle_call(hls_mbr_playlist, _From, #state{hls_mbr_playlist = undefined, 
  media_info = #media_info{} = MediaInfo} = State) ->
  {ok, Playlist} = hls:variant_playlist(MediaInfo),
  handle_call(hls_mbr_playlist, _From, State#state{hls_mbr_playlist = Playlist});

handle_call(hls_mbr_playlist, _From, #state{hls_mbr_playlist = Playlist, timeout = Timeout} = State) ->
  {reply, {ok, Playlist}, State, Timeout};




handle_call({hls_playlist, Tracks}, _From, #state{readers = [Pid|_], timeout = Timeout} = State) ->
  {ok, Playlist} = reader_manifest(Pid, {hls_playlist,Tracks}),
  {reply, {ok, Playlist}, State, Timeout};





handle_call(hls_playlist, _From, #state{hls_playlist = undefined, readers = Readers} = State) ->
  [Pid|_] = Readers,
  {ok, HlsPlaylist} = reader_manifest(Pid, hls_playlist),
  gen_tracker:setattr(flu_files, State#state.name, [{hls_playlist, HlsPlaylist}]),
  handle_call(hls_playlist, _From, State#state{hls_playlist = HlsPlaylist});

handle_call(hls_playlist, _From, #state{hls_playlist = Playlist, timeout = Timeout} = State) ->
  {reply, {ok, Playlist}, State, Timeout};




handle_call(Call, _From, State) ->
  {stop, {unknown_call,Call}, State}.


terminate(_,_) ->
  ok.


gop_duration([#video_frame{dts = DTS}|_] = Frames) ->
  gop_duration(Frames, DTS).

gop_duration([#video_frame{dts = DTS}], StartDTS) -> round(DTS - StartDTS);
gop_duration([_|Frames], StartDTS) -> gop_duration(Frames, StartDTS).



limited_gop(N, Gop) ->
  Count = length(Gop),
  if Count < 5000 -> Gop;
    true ->
      {flu_file,URL} = get(name),
      lager:error("File ~p has broken gop ~p with ~p frames", [URL, N, Count]),
      {Frames, _} = lists:split(5000, Gop),
      Frames
  end.




