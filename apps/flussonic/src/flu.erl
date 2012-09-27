%% Misc features
-module(flu).
-include("log.hrl").

-export([rebuild/0, reload/1, reconf/0]).

-export([now/0, now_ms/0]).

-export([join/2]).
-export([copy_nifs/0]).
-export([extract_config_if_required/0]).
-export([to_hex/1]).
-export([version/0]).
-export([default_file_access/0]).


version() ->
  application:load(flussonic),
  {ok, Version} = application:get_key(flussonic,vsn),
  Version.


copy(Prefix, File, Folder) ->
  Path = filename:join(Prefix, File),
  Dest = filename:join(Folder, File),
  case filelib:is_file(Dest) of
    true -> ok;
    false -> ?D({copy_nif,Path,Dest}), filelib:ensure_dir(Dest),
      case filelib:is_file(Path) of
        true -> file:copy(Path, Dest);
        false ->
          {ok, Bin} = static_file:read_internal(Path),
          file:write_file(Dest, base64:decode(Bin))
      end
  end.

extract_config_if_required() ->
  case file:read_file_info("priv/flussonic.conf") of
    {error, enoent} -> extract_config();
    _ -> ok
  end.

extract_config() ->
  [begin
    filelib:ensure_dir(Path),
    {ok, Bin} = static_file:read_internal(Path),
    file:write_file(Path, Bin)
  end || Path <- static_file:wildcard("priv/[^/]*\\.conf")].

copy_nifs() ->
  
  {unix, OS} = os:type(),
  Prefix = "priv/binaries/" ++ atom_to_list(OS) ++ "/",
  copy(Prefix, "mpegts_reader.so", "apps/mpegts/priv/"),
  copy(Prefix, "mpeg2_crc32.so", "apps/mpegts/priv/"),
  copy(Prefix, "mmap.so", "apps/flussonic/priv/"),
  file:make_dir("apps/mpegts/ebin"), code:add_path("apps/mpegts/ebin"),
  file:make_dir("apps/flussonic/ebin"), code:add_path("apps/flussonic/ebin"),
  ok.


join([], _) -> <<>>;
join(Items, Sep) ->
  [[Sep,Item]|It1] = [[Sep,Item] || Item <- Items],
  iolist_to_binary([Item|It1]).
  
rebuild() ->
  io:format("Recompiling flussonic ...~n"),
	make:all([load]),
  flussonic_app:unload_config(),
	flussonic_app:load_config(),
  ok.

reconf() ->
  error_logger:info_msg("Reloading config"),
  flussonic_app:unload_config(),
  flussonic_app:load_config().

reload(App) ->
	application:load(App),
	case application:get_key(App,modules) of
		undefined ->
			ok;
		{ok,Modules} ->
			io:format("Reloading ~p Modules: ~p~n", [App, Modules]),
			[reload_mod(Module) || Module <- Modules]
	end.

reload_mod(Module) when is_atom(Module) ->
	code:soft_purge(Module),
	code:purge(Module),
	code:load_file(Module),
	Module.


default_file_access() ->
  {ok,Config} = application:get_env(flussonic,config),
  case proplists:get_value(file_access, Config, file) of
    file -> file;
    mmap -> mmap
  end.


now() ->
  {Mega,Sec,_} = erlang:now(),
  Mega*1000000 + Sec.

now_ms() ->
  {Mega, Sec, Micro} = erlang:now(),
  (Mega*1000000 + Sec)*1000 + (Micro div 1000).

to_hex(L) when is_binary(L) ->
  to_hex(binary_to_list(L));

to_hex(L) when is_list(L) -> 
  list_to_binary(lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).
