-module(flu_event_http).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start/2, init/1, handle_event/2, terminate/2]).
-include("log.hrl").
-include("flu_event.hrl").
-include_lib("eunit/include/eunit.hrl").


start(URL, Options) ->
  Installed = lists:member(?MODULE, gen_event:which_handlers(flu_event)),
  case Installed of
    true -> ok;
    false -> gen_event:add_handler(flu_event, ?MODULE, [URL, Options])
  end.

-record(evt, {
  url,
  options = [],
  headers
}).

init([URL, Options]) ->
  Headers = [{"Content-Type","application/json"},{"Client", "Flussonic "++flu:version() }],
  {ok, #evt{url = URL, options = Options, headers = Headers}}.


handle_event(#flu_event{} = Event, #evt{url = URL, headers = Headers} = State) ->
  JSON = flu_event:to_json(Event),
  lhttpc:request(binary_to_list(URL), post, Headers, JSON, 1000),
  {ok, State}.



terminate(_,_) -> ok.
