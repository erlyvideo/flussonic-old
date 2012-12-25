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
-export([hds_manifest/1, hds_segment/2, hds_lang_segment/3,  hds_segment/3]).
-export([hls_segment/3, hls_segment/4, hls_segment/2, hls_mbr_playlist/1, hls_playlist/1, hls_playlist/2]).
-export([get/2]).
-export([read_frame/2, keyframes/1]).
-export([reduce_keyframes/1]).
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/mp4.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(SEGMENT_DURATION,10000).

-record(state, {
  access,
  format,
  reader,
  file,
  requested_path,
  path,
  disk_path,
  options,
  keyframes,
  media_info,
  hds_manifest,
  timeout,
  hls_playlist,
  hls_mbr_playlist,
  hls_length=?SEGMENT_DURATION,
  mpegts
}).



autostart(File, _) when is_pid(File) ->
  {ok, File};

autostart(Name, Options) ->
  gen_tracker:find_or_open(flu_files, Name, fun() -> flussonic_sup:start_flu_file(Name, Options) end).

start_link(Name, Options) ->
  gen_server:start_link(?MODULE, [Name, Options], []).

media_info(File) -> get(File, media_info).

get(File, Key) when is_list(File) ->
  get(list_to_binary(File), Key);

get(File, Key) when is_binary(File) ->
  case gen_tracker:find(flu_files, File) of
    {ok, Pid} -> get(Pid, Key);
    undefined -> undefined
  end;

get(File, Key) ->
  gen_server:call(File, Key).

keyframes(File) -> get(File, keyframes).

hds_manifest(File) -> get(File, hds_manifest).

hds_segment(File, Fragment) -> 
  case get(File, {hds_segment, Fragment}) of
    {ok, {Format, Reader, Id, StopDTS}} ->
      Reply = hds:segment(Format, Reader, Id, [{stop_dts, StopDTS}]),
      Reply;
    {error, Error} ->
      {error, Error}
  end.

segment_info(File, Fragment, Tracks) ->
  case get(File, reader) of
    {ok, {Format, Reader}} ->
      % Need to determine if this audio-only track or has video
      #media_info{streams = Streams1} = Format:media_info(Reader),
      Streams = [S || #stream_info{track_id = Id} = S <- Streams1, lists:member(Id,Tracks)],
      HasVideo = [S || #stream_info{content = video} = S <- Streams] =/= [],

      Keyframes = case HasVideo of
        true -> flu_file:reduce_keyframes(Format:keyframes(Reader, [{tracks,Tracks}]));
        false -> [{T, Id#frame_id{tracks = Tracks}} || {T,#frame_id{} = Id} <- flu_file:reduce_keyframes(Format:keyframes(Reader))]
      end,
      {_DTS,Id}=lists:nth(Fragment,Keyframes),
      StopDts = if length(Keyframes) >= Fragment + 1 ->
        {S, _} = lists:nth(Fragment+1, Keyframes), S;
        true -> 0 
      end,
      Options = case HasVideo of
        true -> [];
        false -> [{no_metadata,true},{hardstop,true}]
      end,
      {ok, {Format, Reader, Id, StopDts, Options}};
    {error, _} = Error ->
      Error
  end.

hds_segment(File, Fragment, Tracks) ->
  case segment_info(File, Fragment, Tracks) of
    {ok, {Format, Reader, Id, StopDts, Options}} ->
      Reply = hds:segment(Format, Reader, Id, [{stop_dts, StopDts},{tracks,Tracks}|Options]),
      Reply;
    {error, Error} ->
      {error, Error}
  end.


hds_lang_segment(File, Lang_, Fragment) -> 
  % case get(File, reader) of
    % {ok, {Format, Reader}} ->
    %   Lang = list_to_integer(binary_to_list(Lang_)),
    %   StartDTS = Fragment * hds:lang_frag_duration(),
    %   StopDTS = (Fragment+1) * hds:lang_frag_duration(),
    %   {Id, _} = Format:seek(Reader, StartDTS, [{language,Lang},{bitrate,false}]),
    %   Reply = hds:segment(Format, Reader, Id, [{stop_dts, StopDTS},{hardstop,true}]),
    %   Reply;
  case get(File, {hds_segment, Fragment}) of
    {ok, {Format = mp4_reader, Reader, #frame_id{} = Id, StopDTS}} ->
      Lang = list_to_integer(binary_to_list(Lang_)),
      Reply = hds:segment(Format, Reader, Id#frame_id{tracks = [Lang]}, [{no_metadata,true},{stop_dts, StopDTS},{hardstop,true}]),
      Reply;
    {error, Error} ->
      {error, Error}
  end.

hls_playlist(File) -> get(File, hls_playlist).
hls_playlist(File, Tracks) -> get(File, {hls_playlist, Tracks}).

hls_mbr_playlist(File) -> get(File, hls_mbr_playlist).

hls_segment(Name, Root, Segment) ->
  {ok, File} = autostart(Name, [{root,Root}]),
  hls_segment(File, Segment).

hls_segment(File,Segment) ->
  case get(File, {hls_segment,Segment}) of
    {ok, {Format, Reader, Id1, StopDTS}} ->
      Id = case Id1 of
        #frame_id{} -> Id1#frame_id{tracks = mp4_reader:tracks_for(Reader, [{language,all}])};
        _ -> Id1
      end,
      {ok, hls:segment(Format,Reader,Id,StopDTS)};
    {error, Error} ->
      {error, Error}
  end.


hls_segment(Name, Root, Segment, Tracks) ->
  {ok, File} = autostart(Name, [{root,Root}]),
  case segment_info(File, Segment, Tracks) of
    {ok, {Format, Reader, Id, StopDts, _Options}} ->
      Reply = hls:segment(Format, Reader, Id, [{stop_dts,StopDts},{tracks,Tracks}]),
      {ok, Reply};
    {error, Error} ->
      {error, Error}
  end.



read_frame(File, Id) ->
  gen_server:call(File, {read_frame, Id}).
  

init([Path, Options]) ->
  Root = proplists:get_value(root, Options),
  URL = case re:run(Path, "http:/([^/].+)", [{capture,all_but_first,binary}]) of
    {match, [URL2]} -> <<"http://", URL2/binary>>;
    _ when Root =/= undefined -> binary_to_list(iolist_to_binary([Root, "/", Path]));
    _ -> Path
  end,

  put(name, {flu_file,URL}),

  Access = case re:run(URL, "http://") of
    nomatch -> flu:default_file_access();
    _ -> http_file
  end,
  ?DBG("open ~s file \"~s\", fullpath: \"~s\", options: ~p",[Access, Path, URL, Options]),
  Format = case re:run(URL, "\\.flv$") of
    nomatch -> mp4_reader;
    _ -> flv_reader
  end,
  
  Timeout = proplists:get_value(timeout, Options, 60000),
  
  State = #state{
    access = Access,
    format = Format,
    disk_path = URL,
    path = proplists:get_value(path, Options, URL),
    timeout = Timeout,
    options = Options,
    requested_path = Path
  },
  {ok, State, Timeout}.


handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  {stop, {unknown_info,Info}, State}.



handle_call(Call, From, #state{file = undefined} = State) ->
  handle_call(Call, From, open(State));

handle_call(media_info, _From, #state{format = Format, reader = Reader, timeout = Timeout} = State) ->
  {reply, Format:media_info(Reader), State, Timeout};

handle_call(keyframes, _From, #state{keyframes = Keyframes, timeout = Timeout} = State) ->
  {reply, Keyframes, State, Timeout};

handle_call(hds_manifest, _From, #state{hds_manifest = undefined, format = Format, reader = Reader} = State) ->
  {ok, HdsManifest} = hds:file_manifest(Format, Reader),
  gen_tracker:setattr(flu_files, State#state.path, [{hds_manifest, HdsManifest}]),
  handle_call(hds_manifest, _From, State#state{hds_manifest = HdsManifest});

handle_call(hds_manifest, _From, #state{hds_manifest = HdsManifest, timeout = Timeout} = State) ->
  {reply, {ok, HdsManifest}, State, Timeout};

handle_call({read_frame, Id}, _From, #state{timeout = Timeout, format = Format, reader = Reader} = State) ->
  Frame = Format:read_frame(Reader, Id),
  {reply, Frame, State, Timeout};

handle_call({Type, Fragment}, _From, #state{keyframes = Keyframes, timeout = Timeout} = State) when
  (Fragment =< 0 orelse Fragment > length(Keyframes)) andalso (Type == hls_segment orelse Type == hds_segment) ->
  {reply, {error, no_segment}, State, Timeout};  

handle_call({Type, Fragment}, _From, #state{keyframes = Keyframes, timeout = Timeout, format = Format, reader = Reader} = State) 
  when Type == hls_segment orelse Type == hds_segment ->
  {_DTS,Id}=lists:nth(Fragment,Keyframes),
  StopDTS = if length(Keyframes) >= Fragment + 1 ->
    {S, _} = lists:nth(Fragment+1, Keyframes), S;
  true -> 0
  end,  
  {reply, {ok, {Format, Reader, Id, StopDTS}}, State, Timeout};

handle_call(reader, _From, #state{timeout = Timeout, format = Format, reader = Reader} = State) ->
  {reply, {ok, {Format, Reader}}, State, Timeout};

handle_call(hls_mbr_playlist, _From, #state{hls_mbr_playlist = undefined, 
  media_info = #media_info{} = MediaInfo} = State) ->
  {ok, Playlist} = hls:variant_playlist(MediaInfo),
  handle_call(hls_mbr_playlist, _From, State#state{hls_mbr_playlist = Playlist});

handle_call({hls_playlist, Tracks}, _From, #state{format = Format, reader = Reader,path = Path} = State) ->
  {ok, Playlist} = hls:playlist(Format, Reader, [{name,Path},{tracks,Tracks}]),
  handle_call(hls_playlist, _From, State#state{hls_playlist = Playlist});

handle_call(hls_playlist, _From, #state{path = Path, hls_playlist = undefined, keyframes = Keyframes,
  media_info = #media_info{duration = Duration}} = State) ->
  {ok, Playlist} = hls:playlist(Keyframes, [{name,Path},{duration,Duration}]),
  handle_call(hls_playlist, _From, State#state{hls_playlist = Playlist});

handle_call(hls_playlist, _From, #state{hls_playlist = Playlist, timeout = Timeout} = State) ->
  {reply, {ok, Playlist}, State, Timeout};

handle_call(hls_mbr_playlist, _From, #state{hls_mbr_playlist = Playlist, timeout = Timeout} = State) ->
  {reply, {ok, Playlist}, State, Timeout};

handle_call(Call, _From, State) ->
  {stop, {unknown_call,Call}, State}.


terminate(_,_) ->
  ok.

open(#state{disk_path = Path, requested_path = Path1, access = Access, format = Format, file = undefined} = State) ->
  Options = case Access of
    file -> [read,binary];
    mmap -> [];
    http_file -> []
  end,
  case Access:open(Path, Options) of
    {ok, File} ->
      {ok, Reader} = Format:init({Access, File}, []),
      MediaInfo = Format:media_info(Reader),
      case MediaInfo of
        #media_info{streams = Streams} when length(Streams) > 0 -> ok;
        _ -> throw({stop, {invalid_media_info, MediaInfo}, {return, 500, "Invalid media_info in file"}, State})
      end,
      Keyframes = reduce_keyframes(Format:keyframes(Reader)),
      
      MPEGTS = mpegts:init([{interleave,3}]),
      MPEGTS1 = mpegts:encode(MPEGTS, MediaInfo),
      % MPEGTS2 = lists:foldl(fun(Mpeg, Frame) -> {Mpeg1,_} = mpegts:encode(Frame, Mpeg), Mpeg1 end, MPEGTS1, video_frame:config_frames(MediaInfo)),
      State#state{file = File, media_info = MediaInfo, keyframes = Keyframes, reader = Reader, mpegts = MPEGTS1};
    {error, enoent} ->
      throw({stop, normal, {return, 404, lists:flatten(io_lib:format("No such file ~s", [Path1]))}, State});
    {error, eaccess} ->
      throw({stop, normal, {return, 403, lists:flatten(io_lib:format("Forbidden to open file ~s", [Path1]))}, State});
    {error, Error} ->
      throw({stop, normal, {return, 500, lists:flatten(io_lib:format("Error ~p opening file ~s", [Error, Path1]))}, State})
  end;

open(State) ->
  State.

-define(MIN_DTS_STEP, 3000).

reduce_keyframes(Keyframes) ->
  {Keyframes1, _} = lists:mapfoldl(fun
    ({DTS,Id}, PrevDTS) when DTS - PrevDTS >= ?MIN_DTS_STEP -> {{DTS, Id}, DTS};
    (_, PrevDTS) -> {undefined, PrevDTS}
  end, -2*?MIN_DTS_STEP, Keyframes),
  Keyframes2 = [{DTS, Id} || {DTS, Id} <- Keyframes1],
  Keyframes2.




