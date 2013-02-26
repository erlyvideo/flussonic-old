%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        event
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
-module(flu_event).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("jsonerl.hrl").
-include("flu_event.hrl").
-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start_link/0, notify/1, add_handler/2, subscribe_to_events/1, add_sup_handler/2, remove_handler/1]).
-export([start_handlers/0]).

-export([user_connected/2, user_disconnected/2, user_play/3, user_stop/3]).
-export([stream_created/2, stream_stopped/1]).
-export([add_dvr_fragment/2, delete_dvr_fragment/2]).

-export([hls_bitrate_down/2, hls_bitrate_up/2]).
-export([hls_segment_drop/2]).

-export([to_json/1, to_xml/1, to_proplist/1]).

%% gen_event callbacks

start_link() ->
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  {ok, Pid}.



start_handlers() ->
  Config = flu_config:get_config(),
  Handlers = [{Handler, Args} || {flu_event, Handler, Args} <- Config],
  NewHandlers = [H || {H,_} <- Handlers],
  OldHandlers = gen_event:which_handlers(?MODULE),
  ToInstall = NewHandlers -- OldHandlers,
  ToUpdate = NewHandlers -- ToInstall,
  ToRemove = OldHandlers -- NewHandlers,
  [gen_event:delete_handler(?MODULE, Handler, []) || Handler <- ToRemove],
  [gen_event:add_handler(?MODULE, Handler, Args) || {Handler, Args} <- Handlers, lists:member(Handler, ToInstall)],
  [gen_event:call(?MODULE, Handler, {update_options, Args}) || {Handler,Args} <- Handlers, lists:member(Handler, ToUpdate)],
  ok.


%%--------------------------------------------------------------------
%% @spec (Event::any()) -> ok
%%
%% @doc Convert event to JSON
%% @end
%%----------------------------------------------------------------------
to_json(#flu_event{options = _Options} = Event) ->
  List = lists:zip(record_info(fields, flu_event), tl(tuple_to_list(Event))),
  iolist_to_binary(mochijson2:encode(clean(List))).


clean([]) -> [];
clean([{_,undefined}|Rest]) -> clean(Rest);
clean([{K,true}|Rest]) -> [{K,true}|clean(Rest)];
clean([{K,false}|Rest]) -> [{K,false}|clean(Rest)];
clean([{K,V}|Rest]) when is_atom(V) -> [{K,V}|clean(Rest)];
clean([{K,V}|Rest]) when is_integer(V) -> [{K,V}|clean(Rest)];
clean([{K,V}|Rest]) when is_list(V) -> [{K,clean(V)}|clean(Rest)];
clean([{K,V}|Rest]) when is_binary(V) -> [{K,V}|clean(Rest)];
clean([_|Rest]) -> clean(Rest).



to_proplist(#flu_event{} = Event) ->
  lists:zip(record_info(fields, flu_event), tl(tuple_to_list(Event))).

to_xml(Event) ->
  Content = xmlize(tuple_to_list(?record_to_struct(flu_event, Event)), []),
  XML = xmerl:export_simple([{event, [], Content}], xmerl_xml),
  iolist_to_binary(XML).

xmlize([{K,V}|Attr], Acc) when is_binary(K) ->
  xmlize([{binary_to_atom(K,utf8),V}|Attr], Acc);


xmlize([{K,undefined}|Attr], Acc) ->
  xmlize(Attr, [{K, [], ["null"]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_atom(V) ->
  xmlize(Attr, [{K, [], [atom_to_list(V)]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_binary(V) ->
  xmlize(Attr, [{K, [], [io_lib:format("~s", [V])]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_number(V) ->
  xmlize(Attr, [{K, [], [io_lib:format("~p", [V])]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_pid(V) ->
  xmlize(Attr, [{K, [], [erlang:pid_to_list(V)]}|Acc]);

xmlize([{K,V}|Attr], Acc) when is_reference(V) ->
  xmlize(Attr, [{K, [], [erlang:ref_to_list(V)]}|Acc]);

xmlize([{K,[{_,_}|_] = V}|Attr], Acc) ->
  xmlize(Attr, [{K, [], xmlize(V, [])}|Acc]);

xmlize([{K,V}|Attr], Acc) ->
  xmlize(Attr, [{K, [], [V]}|Acc]);
  
xmlize([], Acc) ->
  lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @spec (Event::any()) -> ok
%%
%% @doc Send event to ems_event subscribers
%% @end
%%----------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?MODULE, Event).

  
%%--------------------------------------------------------------------
%% @spec (Handler::any(), Args::[any()]) -> ok
%%
%% @doc Subscribe to ems_event
%% @end
%%----------------------------------------------------------------------
add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @spec (Handler::any(), Args::[any()]) -> ok
%%
%% @doc Subscribe to ems_event
%% @end
%%----------------------------------------------------------------------
subscribe_to_events(Pid) ->
  add_sup_handler(flu_event_consumer, [Pid]).

%%--------------------------------------------------------------------
%% @spec (Handler::any(), Args::[any()]) -> ok
%%
%% @doc Subscribe to ems_event
%% @end
%%----------------------------------------------------------------------
add_sup_handler(Handler, Args) ->
  gen_event:add_sup_handler(?MODULE, Handler, Args).
  
%%--------------------------------------------------------------------
%% @spec (Handler::any()) -> ok
%%
%% @doc Unsubscribe from ems_event
%% @end
%%----------------------------------------------------------------------
remove_handler(Handler) ->
  gen_event:delete_handler(?MODULE, Handler, []).

%%--------------------------------------------------------------------
%% @spec (Stream, Stats) -> ok
%%
%% @doc send event that user has connected
%% @end
%%----------------------------------------------------------------------
user_connected(Stream, Stats) ->
  UserId = proplists:get_value(user_id, Stats),
  SessionId = proplists:get_value(session_id, Stats),
  gen_event:notify(?MODULE, #flu_event{event = 'user.connected', stream = Stream, session_id = SessionId, user_id = UserId, options = Stats}).

%%--------------------------------------------------------------------
%% @spec (Stream, Stats) -> ok
%%
%% @doc send event that user has disconnected
%% @end
%%----------------------------------------------------------------------
user_disconnected(Stream, Stats) ->
  UserId = proplists:get_value(user_id, Stats),
  SessionId = proplists:get_value(session_id, Stats),
  gen_event:notify(?MODULE, #flu_event{event = 'user.disconnected', stream = Stream, session_id = SessionId, user_id = UserId, options = Stats}).

%%--------------------------------------------------------------------
%% @spec (User, Name) -> ok
%%
%% @doc send event that user has started playing
%% @end
%%----------------------------------------------------------------------
user_play(User, StreamName, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'user.play', user = User, stream = StreamName, options = Options}).

%%--------------------------------------------------------------------
%% @spec (User, Name, Stats) -> ok
%%
%% @doc send event that user has finished playing
%% @end
%%----------------------------------------------------------------------
user_stop(User, StreamName, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'user.stop', user = User, stream = StreamName, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Name, Stream, Options) -> ok
%%
%% @doc send event that stream has been created
%% @end
%%----------------------------------------------------------------------
stream_created(Name, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'stream.created', stream = Name, options = Options}).

%%--------------------------------------------------------------------
%% @spec (Name, Stream) -> ok
%%
%% @doc send event that stream was completely stopped
%% @end
%%----------------------------------------------------------------------
stream_stopped(Name) ->
  gen_event:notify(?MODULE, #flu_event{event = 'stream.stopped', stream = Name}).

%%--------------------------------------------------------------------
%%
%% @doc send event that new fragment was recorded for stream
%% @end
%%----------------------------------------------------------------------
add_dvr_fragment(Name, Time) ->
  gen_event:notify(?MODULE, #flu_event{event = 'stream.add_dvr_fragment', stream = Name, options = [{time,Time}]}).


%%--------------------------------------------------------------------
%%
%% @doc send event that new fragment was recorded for stream
%% @end
%%----------------------------------------------------------------------
delete_dvr_fragment(Name, Time) ->
  gen_event:notify(?MODULE, #flu_event{event = 'stream.delete_dvr_fragment', stream = Name, options = [{time,Time}]}).


%%--------------------------------------------------------------------
%% @spec (Name, Stream, Options) -> ok
%%
%% @doc send event that stream has been created
%% @end
%%----------------------------------------------------------------------
hls_bitrate_up(Name, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'hls.bitrate.up', stream = Name, options = Options}).

hls_bitrate_down(Name, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'hls.bitrate.down', stream = Name, options = Options}).

hls_segment_drop(Name, Options) ->
  gen_event:notify(?MODULE, #flu_event{event = 'hls.segment.drop', stream = Name, options = Options}).



