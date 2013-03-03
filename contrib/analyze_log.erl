#!/usr/bin/env escript

-mode(compile).
-include_lib("stdlib/include/ms_transform.hrl").

main([]) ->
  main(["1"]);

main([CacheSize_]) ->
  CacheSize = list_to_integer(CacheSize_)*1024*1024*1024,
  ets:new(cache, [public,named_table,ordered_set]),
  ets:new(cache_info, [public,named_table]),
  ets:insert(cache_info, {size, 0}),
  ets:insert(cache_info, {flow_size, 0}),
  ets:insert(cache_info, {count, 0}),
  ets:insert(cache_info, {i, 0}),
  ets:insert(cache_info, {hit, 0}),
  ets:insert(cache_info, {hit_size, 0}),
  ets:insert(cache_info, {miss, 0}),
  ets:insert(cache_info, {miss_size, 0}),
  ets:insert(cache_info, {limit, CacheSize}),
  loop([], 40),
  io:format("~3.. B% ~3.. B% ~5.. BMB\n", [hit_flow_ratio(), miss_flow_ratio(), cache_size()]),
  ok.

loop([], 0) ->
  ok;

loop([], Number) ->
  case file:read_file("access.log."++integer_to_list(Number)) of
    {ok, Bin} ->
      io:format("~3.. B% ~3.. B% ~5.. BMB ~p\n", [hit_flow_ratio(), miss_flow_ratio(), cache_size(), Number]),
      Rows = binary:split(Bin, <<"\n">>, [global]),
      loop(Rows, Number - 1);
    {error, _} ->
       loop([], Number - 1)
  end;

% <<"2013-03-03 15:55:37.723 503   157 0 /vod/muvi.43.mp4/hds/tracks-2,4/Seg1-Frag40">>
loop([Row|Rows], Number) ->
  case re:run(Row, "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})\\.(\\d{3}) (\\d{3}) +(\\d+) (\\d+) (.*)$", [{capture,all_but_first,list}]) of
    nomatch ->
      % io:format("badrow: ~p~n", [Row]),
      loop(Rows, Number);
    {match, [Y,Mon,D,H,M,S,MS,Code,Time,Size_,Path]} ->
      Size = list_to_integer(Size_),
      if Size < 100 ->
        % bad reply
        loop(Rows, Number);
      true ->
        case ets:select(cache, ets:fun2ms(fun({_,P,_} = Rec) when P == Path -> Rec end)) of
          [] ->
            ets:update_counter(cache_info, i,1),
            ets:update_counter(cache_info, count,1),
            ets:update_counter(cache_info, miss,1),
            ets:update_counter(cache_info, miss_size,Size),
            ets:update_counter(cache_info, flow_size,Size),
            prepare_place_in_cache(Size),
            I = ets:lookup_element(cache_info, i, 2),
            ets:update_counter(cache_info, size, Size),
            ets:insert(cache, {I,Path,Size});
          [{I,Path,Size1}] ->
            Size == Size1 orelse io:format("Different size ~p: ~p,~p\n", [Path,Size1,Size]),
            ets:update_counter(cache_info, i,1),
            ets:update_counter(cache_info, count,1),
            ets:update_counter(cache_info, hit,1),
            ets:update_counter(cache_info, hit_size,Size),
            ets:update_counter(cache_info, flow_size,Size),
            ets:delete(cache, I),
            ets:insert(cache, {ets:lookup_element(cache_info, i, 2),Path,Size})
        end,
        % io:format("~3.. B% ~3.. B% ~5.. BMB ~s\n", [hit_flow_ratio(), miss_flow_ratio(), cache_size(), Path]),
        loop(Rows, Number)
      end
  end.


div_(A,B) ->
  Avalue = ets:lookup_element(cache_info, A, 2),
  Bvalue = ets:lookup_element(cache_info, B, 2),
  case Bvalue of
    0 -> 0;
    _ -> Avalue*100 div Bvalue
  end.


hit_ratio() -> div_(hit,count).
hit_flow_ratio() -> div_(hit_size,flow_size).

miss_ratio() -> div_(miss,count).
miss_flow_ratio() -> div_(miss_size,flow_size).

cache_size() ->
  ets:lookup_element(cache_info, size, 2) div (1024*1024).

prepare_place_in_cache(Size) ->
  case ets:lookup_element(cache_info, size, 2) + Size > ets:lookup_element(cache_info, limit, 2) of
    true ->
      [{I,P,S}] = ets:lookup(cache, ets:first(cache)),
      % io:format("remove ~p\n", [P]),
      ets:delete(cache, I),
      ets:update_counter(cache_info, size,-S),
      prepare_place_in_cache(Size);
    false ->
      ok
  end.

