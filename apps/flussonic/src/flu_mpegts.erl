-module(flu_mpegts).

-export([read/3]).

read(Name, URL, Options) ->
  Opts = [{url,URL}|Options] ++ [{consumer,self()}],
  {ok, Reader} = flu_stream:start_helper(Name, mpegts_reader, {mpegts_reader, start_link, [Opts]}),
  case (catch gen_server:call(Reader, connect)) of
    ok -> {ok, Reader};
    {error, _} = Error -> Error;
    {'EXIT', Error} -> {error, Error}
  end.
