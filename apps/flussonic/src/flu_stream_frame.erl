%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2012 Max Lapshin
%%% @doc        stream_frame
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
-module(flu_stream_frame).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("flu_stream.hrl").
-include("log.hrl").


-export([calculate_new_stream_shift/2, shift_dts_delta/2, fix_large_dts_jump/2]).
-export([check_dts_wallclock/2, store_gop/2, save_config/2, save_last_dts/2]).
-export([handle_frame/2]).

handle_frame(#video_frame{} = Frame, Media) ->
  Handlers = [
    fix_large_dts_jump,
    calculate_new_stream_shift,
    shift_dts_delta,
    save_last_dts,
    store_gop,
    save_config
  ],
  
  {F2,M2} = lists:foldl(fun(Handler, {F,M}) ->
    ?MODULE:Handler(F,M)
  end, {Frame, Media}, Handlers),
  
  {reply, F2, M2}.
  
  % {reply, F5, M5} = check_dts_wallclock(F4, M4),
  % {reply, F5, M5}.


% utc() ->
%   {Mega, Sec, Micro} = erlang:now(),
%   (Mega*1000000+Sec)*1000 + Micro / 1000.

calculate_new_stream_shift(#video_frame{dts = DTS} = Frame, #stream{ts_delta = undefined, last_dts_at = LostAt, last_dts = LDTS} = Media) ->
  GlueDelta = case LostAt of
    undefined -> 0;
    _ -> timer:now_diff(erlang:now(), LostAt) div 1000
  end,
  {LastDTS, TSDelta} = case LDTS of
    undefined -> Now = DTS, {Now, Now - DTS};
    _ -> {LDTS, LDTS - DTS + GlueDelta}
  end,
  ?D({"New ts_delta", Media#stream.name, round(LastDTS), round(DTS), round(TSDelta)}),
  % ems_event:stream_started(proplists:get_value(host,Media#stream.options), Media#stream.name, self(), Media#stream.options),
  {Frame, Media#stream{ts_delta = TSDelta, last_dts_at = undefined}}; %% Lets glue new instance of stream to old one plus small glue time

calculate_new_stream_shift(Frame, Media) ->
  {Frame, Media}.


shift_dts_delta(#video_frame{dts = DTS, pts = PTS} = Frame, #stream{ts_delta = Delta} = Media) ->
  {Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta}, Media}.


-define(DTS_THRESHOLD, 30000).
-define(GLUE_DELTA, 25). %% 25 milliseconds is an average time shift between frames

fix_large_dts_jump(#video_frame{dts = DTS} = Frame, #stream{last_dts = LastDTS, ts_delta = Delta} = Media) when DTS + Delta - LastDTS > ?DTS_THRESHOLD ->
  ?D({large_dts_jump,forward,Media#stream.name,round(DTS),round(Delta),round(LastDTS),round(DTS+Delta - LastDTS)}),
  {Frame, Media#stream{ts_delta = undefined}};

fix_large_dts_jump(#video_frame{dts = DTS} = Frame, #stream{last_dts = LastDTS, ts_delta = Delta} = Media) when LastDTS - DTS - Delta > ?DTS_THRESHOLD ->
  ?D({large_dts_jump,backward,Media#stream.name,round(DTS),round(Delta),round(LastDTS),round(LastDTS - DTS - Delta)}),
  {Frame, Media#stream{ts_delta = undefined}};

% fix_large_dts_jump(#video_frame{dts = DTS, pts = PTS} = Frame, #stream{last_dts = LastDTS} = Media) when is_number(LastDTS) andalso LastDTS > DTS ->
%   ?D({small_dts_jump,backward,Media#stream.name,Media#stream.last_dts,DTS}),
%   {reply, Frame#video_frame{dts = LastDTS, pts = PTS + LastDTS - DTS}, Media#stream{ts_delta = undefined}};

fix_large_dts_jump(Frame, Media) ->
  % ?D({round(Frame#video_frame.dts - Media#stream.last_dts)}),
  {Frame, Media}.

save_last_dts(#video_frame{content = metadata, body = [<<"onFI">> | _]} = Frame, Media) ->
  {Frame, Media};

save_last_dts(#video_frame{dts = DTS} = Frame, Media) ->
  {Frame, Media#stream{last_dts = DTS, last_dts_at = os:timestamp()}}.

check_dts_wallclock(#video_frame{dts = DTS} = Frame, Media) ->
  {Mega, Sec, Micro} = erlang:now(),
  T = (Mega*1000*1000 + Sec)*1000 + Micro div 1000,
  Delta = round(T - DTS),
  if abs(Delta) > 1000 -> ?D({too_variable_dts, round(DTS), T, Delta});
    true -> ok
  end,
  {Frame, Media}.

store_gop(#video_frame{flavor = keyframe} = F, #stream{gop = GOP} = Stream) when GOP == undefined orelse length(GOP) > 150 ->
  Stream1 = case GOP of
    undefined -> Stream;
    _ -> flu_stream:pass_message({gop, lists:reverse(GOP)}, Stream)
  end,
  {F, Stream1#stream{gop = [F]}};

store_gop(#video_frame{} = F, #stream{gop = GOP} = Stream) when is_list(GOP) ->
  {F, Stream#stream{gop = [F|GOP]}};

store_gop(#video_frame{} = F, #stream{gop = undefined} = Stream) ->
  {F, Stream}.


save_config(#video_frame{flavor = config, content = Content} = F, Stream) ->
  case Content of
    audio -> put(audio_config, F);
    video -> put(video_config, F)
  end,
  put(configs, [C || C <- [get(audio_config), get(video_config)], C =/= undefined]),
  {F, Stream};

save_config(#video_frame{} = F, Stream) ->
  {F, Stream}.

