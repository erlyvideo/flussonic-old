-module(mpegts_decoder_c).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-export([init/0, decode/2]).
-export([init_nif/0]).


-on_load(init_nif/0).


init_nif() ->
  Path = case code:lib_dir(mpegts,priv) of
    {error, _} -> case file:read_file_info("apps/mpegts/priv") of
      {ok, _} -> "apps/mpegts/priv";
      {error, _} -> "priv"
    end;
    Dir -> Dir
  end ++ "/mpegts_decoder",
  case erlang:load_nif(Path, 0) of
    ok -> lager:info("mpegts_decoder loaded, acceleration enabled."), ok;
    {error, Error} -> lager:info("Loading mpegts_decoder failed: ~p. Acceleration disabled", [Error]), ok
  end.



init() ->
  error(load_nif).


decode(_Bin, _Decoder) ->
  error(load_nif).




