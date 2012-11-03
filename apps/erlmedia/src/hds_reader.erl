-module(hds_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-include("../include/video_frame.hrl").

-export([last_fragment/1, read/1]).
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2]).

-record(reader, {
  url,
  clients = [],
  first_dts,
  started_at,
  prev_url,
  socket
}).

-record(manifest, {
  base,
  media_url,
  fragment_urls = [],
  fragment_numbers = []
}).

b2l(URL) when is_binary(URL) ->
  binary_to_list(URL);
b2l(URL) when is_list(URL) ->
  URL.

% http://erlybuild.local/hds-vod/sample1_700kbps.f4v.f4m

last_fragment(URL) ->
  case manifest(b2l(URL)) of
    {ok, #manifest{fragment_urls = Fragments}} -> hd(lists:reverse(Fragments));
    {error, Reason} -> {error, Reason}
  end.


read(URL) ->
  {ok, Pid} = start_link(URL),
  gen_server:call(Pid, {subscribe, self()}),
  {ok, Pid}.

start_link(URL) ->
  gen_server:start_link(?MODULE, [b2l(URL)], []).

init([URL]) when is_list(URL) ->
  self() ! fetch,
  ets:insert(http_bench_stats, {required, 0}),
  {ok, #reader{url = URL}}.


get_url("http://" ++ _ = URL) ->
  % ?D({request,URL}),
  case http_stream:request_body(URL, [{socket,get(http_socket)}]) of
    {ok,{Socket1, Code, Headers, Body}} when Code >= 200 andalso Code < 300 ->
      gen_tcp:close(Socket1),
      % put(http_socket,Socket1),
      {ok, Headers, Body};
    {ok,{_Socket,Code,_Headers,_Body}} ->
      {error,Code};
    {error,Reason} ->
      {error, Reason}
  end.

handle_call({subscribe, Pid}, _From, #reader{clients = Clients} = Reader) ->
  erlang:monitor(process, Pid),
  {reply, ok, Reader#reader{clients = [Pid|Clients]}};

handle_call(_Call, _From, #reader{} = Reader) ->
  {stop,{unknown_call,_Call},Reader}.

handle_info({'DOWN', _, _, Pid, _}, #reader{clients = Clients} = Reader) ->
  {noreply, Reader#reader{clients = lists:delete(Pid, Clients)}};

handle_info(fetch, #reader{url = URL, prev_url = PrevURL, clients = Clients} = Reader) ->
  % ?D({fetch, URL}),
  try last_fragment(URL) of
    PrevURL ->
      timer:send_after(1000, fetch),
      {noreply, Reader};
    {error, Reason} ->
      ?D({fail_to_fetch,URL, Reason}),
      timer:send_after(500, fetch),
      {noreply, Reader};
    FragmentURL ->        
      % ?D({reading,FragmentURL}),
      {ok, {Socket1, _Code, _Headers, <<_Size:32, "mdat", FLV/binary>> = S}} = http_stream:request_body(FragmentURL, [{socket,get(http_socket)}]),
      % put(http_socket,Socket1),
      gen_tcp:close(Socket1),
      ets:update_counter(http_bench_stats, required, [{2,size(S)}]),
      Frames = unpack_flv(FLV),
      Timestamps = [DTS || #video_frame{dts = DTS} <- Frames],
      Duration = lists:max(Timestamps) - lists:min(Timestamps),
      Reader1 = init_dts(Frames, Reader),
      Delay = calculate_delay(Frames, Reader1),
      ?D({FragmentURL, got,{frames, length(Frames)}, {delay, Delay}, {size,size(FLV)}}),
      [Client ! {fetch, FragmentURL, Duration} || Client <- Clients],
      timer:send_after(Delay, fetch),
      {noreply, Reader1#reader{prev_url = FragmentURL}}
  catch
    throw:{tcp_error, Reason} ->
      ?D({fail_to_fetch,URL, Reason}),
      timer:send_after(500, fetch),
      {noreply, Reader}
  end;

handle_info(close, Reader) ->
  {stop, normal, Reader}.

terminate(_,_) -> ok.


init_dts([#video_frame{dts = DTS}|_], #reader{first_dts = undefined} = Reader) ->
  Reader#reader{first_dts = DTS, started_at = erlang:now()};

init_dts(_, Reader) ->
  Reader.


calculate_delay(Frames, #reader{first_dts = FirstDTS, started_at = StartedAt}) when length(Frames) > 0 ->
  #video_frame{dts = DTS} = hd(lists:reverse(Frames)),
  DtsDelta = DTS - FirstDTS,
  RealDelta = timer:now_diff(erlang:now(), StartedAt) div 1000,
  Delta = DtsDelta - RealDelta - 300,
  if
    Delta < 0 -> 0;
    true -> round(Delta)
  end.
  

  

unpack_flv(FLV) ->
  unpack_flv(FLV, 0, []).

unpack_flv(FLV, Offset, Acc) ->
  case flv:read_frame({mmap, FLV}, Offset) of
    #video_frame{next_id = Next} = Frame ->
      unpack_flv(FLV, Next, [Frame|Acc]);
    eof ->
      lists:reverse(Acc)
  end.  
    

manifest(URL) ->
  case get_url(URL) of
    {ok, _Header, Body} ->
      {ok, Manifest} = parse_manifest(filename:dirname(URL) ++ "/", Body),
      {ok, Manifest};
    {error, Reason} ->
      {error, Reason}
  end.  

parse_manifest(Base, Body) ->
  NameFun = fun(Name, _Namespace, _Prefix) -> Name end,
	{ok, XML, _Rest} = erlsom:simple_form(Body, [{nameFun, NameFun}]),
	Info = parse_xml(XML, Base),
	{'Bootstrap', Bootstrap, _} = proplists:get_value(bootstrapInfo, Info),
	{'FragmentRunEntry', _, Fragments} = proplists:get_value(fragments, Bootstrap),
	Media = proplists:get_value(media, Info),
	MediaURL = proplists:get_value(url, Media),
	FragmentNumbers = [Number || {Number, _, _} <- Fragments],
	FragmentURLs = [Base ++ MediaURL++ "Seg1-Frag" ++ integer_to_list(Number) || Number <- FragmentNumbers],
	
	{ok, #manifest{
	  base = Base,
	  media_url = MediaURL,
	  fragment_urls = FragmentURLs,
	  fragment_numbers = FragmentNumbers
	}}.
  % CurrentFragment = lists:nth(length(Fragments) div 2, Fragments),
	

parse_xml({"manifest", [], Entries}, Base) ->
  [{list_to_atom(Key), parse_manifest_entry(list_to_atom(Key), Attrs, Value, Base)} || {Key, Attrs, Value} <- Entries].

parse_manifest_entry(id, _, [Id], _) ->
  clean(Id);

parse_manifest_entry(streamType, _, [Type], _) ->
  list_to_atom(clean(Type));

parse_manifest_entry(bootstrapInfo, Attrs, Value, Base) ->
  Bootstrap = case {Value, proplists:get_value("url", Attrs)} of
    {[Bootstrap_],_} -> base64:decode(Bootstrap_);
    {[],URL} ->
      case get_url(Base ++ URL) of
        {ok, _, Bootstrap_} -> Bootstrap_;
        {error, Reason} -> erlang:throw({tcp_error,Reason})
      end
  end,
  mp4:parse_atom(Bootstrap, state);

parse_manifest_entry(media, Attrs, _Value, _) ->
  [{url, proplists:get_value("url", Attrs)}];

parse_manifest_entry(_Key, _Attrs, Value, _) ->
  Value.

clean(String) ->
  re:replace(String, "[\\n\\t]*([^\\n\\t]+)[\\n\\t]+", "\\1", [{return,list}]).
