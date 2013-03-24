%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2010 Max Lapshin
%%% @doc        MP4 decoding module
%%% 
%
% порядок чтения mp4 файла приблизительно такой:
%
% читаем stsc, из него выясняем сколько семплов в каждом чанке. Это неприятный момент в силу сложной упаковки
% из stco узнаем смещение каждого чанка
% из stsz знаем сколько всего фреймов в треке. Перебираем i от 1 до N
% по i вычисляем в каком мы сейчас чанке находимся из первого атома
% по второму атому вычисляем смещение чанка
% суммируем размеры всех предыдущих фрейма в этом чанке и получаем смещение нужного фрейма
% суммируем временные длины всех предыдущих семплов и получаем таймстемп (DTS) нужного фрейма
%
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
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

-module(mp4).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/mp4.hrl").
-include("../include/srt.hrl").
-include("../include/video_frame.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-export([ftyp/2, moov/2, mvhd/2, trak/2, tkhd/2, mdia/2, mdhd/2, stbl/2, stsd/2, esds/2, avcC/2]).
-export([btrt/2, stsz/2, stts/2, stsc/2, stss/2, stco/2, co64/2, minf/2, ctts/2, udta/2]).
-export([mp4a/2, mp4v/2, avc1/2, s263/2, samr/2, free/2, wave/2]).
-export([edts/2, elst/2]).
-export([hdlr/2, dinf/2, dref/2, 'url '/2, 'pcm '/2, 'spx '/2, '.mp3'/2]).
-export([meta/2, data/2]).

-export([abst/2, asrt/2, afrt/2]).

-export([extract_language/1]).
-export([parse_atom/2]).

-export([read_gop/3]).

-record(esds, {
  object_type,
  stream_type,
  upstream,
  buffer_size,
  max_bitrate,
  avg_bitrate,
  specific
}).


-export([mp4_desc_length/1, open/2, mp4_read_tag/1]).
-export([keyframes/2, parse_esds/1]).
-export([dump/1]).

-define(FRAMESIZE, 32).


dump(Path) when is_list(Path) ->
  {ok, Bin} = file:read_file(Path),
  dump(Bin);
  
dump(Bin) when is_binary(Bin) ->
  dump_list(Bin, 0).

dump_list(<<>>, _) ->
  ok;

dump_list(Bin, Indent) ->
  Rest = dump_atom(Bin, Indent),
  dump_list(Rest, Indent).

dump_atom(<<1:32, Atom:4/binary, Length:64, Data/binary>>, Indent) ->
  AtomLen = Length - 16,
  <<Content:AtomLen/binary, Rest/binary>> = Data,
  dump_atom(binary_to_atom(Atom, latin1), Content, Indent),
  Rest;
  

dump_atom(<<Length:32, Atom:4/binary, Data/binary>>, Indent) when Length =/= 1 ->
  AtomLen = Length - 8,
  <<Content:AtomLen/binary, Rest/binary>> = Data,
  dump_atom(binary_to_atom(Atom, latin1), Content, Indent),
  Rest;

dump_atom(<<>>, _) ->
  <<>>.
  

dump_atom(abst = Atom, Content, Indent) ->
  {Format, Args, <<SegmentCount, SubContent/binary>>} = atom_dump_info(Atom, Content),
  indented_dump(Atom, Indent, Format, Args),
  SegmentCount == 1 orelse throw(non_one_segment_count),
  <<FragmentCount, FragmentTables/binary>> = lists:foldl(fun
    (0, Bin) -> Bin;
    (_N, Bin) -> dump_atom(Bin, Indent+1)
  end, SubContent, lists:seq(1, SegmentCount)),

  lists:foldl(fun
    (0, Bin) -> Bin;
    (_N, Bin) -> dump_atom(Bin, Indent+1)
  end, FragmentTables, lists:seq(1,FragmentCount));

dump_atom(Atom, Content, Indent) ->
  {Format, Args, SubContent} = atom_dump_info(Atom, Content),
  indented_dump(Atom, Indent, Format, Args),
  dump_list(SubContent, Indent+1).

indented_dump(Atom, Indent, Format, Args) ->
  Tab = [" " || _N <- lists:seq(1,Indent)],
  io:format(lists:flatten([Tab, "~p ", Format, "~n"]), [Atom|Args]).
  
  
atom_dump_info(abst, ABST) ->
  <<_Version, 0:24, _BootstrapVersion:32, Profile:2, Live:1, Update:1, 0:4, Timescale:32, Duration:64, _TimeOffset:64, 
    MovieId, ServerEntryCount, QualityCount, DrmData, MetaData, Rest/binary>> = ABST,
  MovieId == 0 orelse throw({abst,non_null_movie_id}),
  ServerEntryCount == 0 orelse throw({abst,server_entry}),
  QualityCount == 0 orelse throw({abst,quality}),
  DrmData == 0 orelse throw({abst,drm}),
  MetaData == 0 orelse throw({abst,meta}),
  {"profile=~p,live=~p,update=~p,timescale=~p,duration=~p", [Profile, Live, Update, Timescale,Duration], Rest};

atom_dump_info(asrt, <<0:32, QualityCount, SegmentCount:32, ASRT/binary>>) ->
  QualityCount == 0 orelse throw({asrt,non_zero_quality}),
  
  {Rest, Seg1} = lists:foldl(fun(_N, {<<FirstSegment:32, Fragments:32, R/binary>>, Acc}) -> 
    {R, [{FirstSegment,Fragments}|Acc]} 
  end, {ASRT, []}, lists:seq(1, SegmentCount)),
  Segments = lists:reverse(Seg1),
  
  {"segments=~p,~p", [SegmentCount, Segments], Rest};

atom_dump_info(afrt, <<0:32, Timescale:32, QualityCount, TotalFragments:32, Rest/binary>>) ->
  {_Rest, Info} = dump_afrt_fragments(Rest, TotalFragments, []),
  {"timescale=~p,quality_count=~p,total_fragments=~p,~p ~n    (~p)~n    rest=~p", [Timescale, QualityCount, TotalFragments, length(Info), Info, _Rest], <<>>};

atom_dump_info(afra, <<0:32, _LongIDs:1, LongOffsets:1, GlobalEntries:1, _:5, _Timescale:32, EntryCount:32, Rest1/binary>>) ->
  {_Rest2, AfraEntries} = dump_afra_entries(Rest1, LongOffsets, EntryCount, []),
  {"global_entries=~p,afra_entries=~p (~p)", [GlobalEntries,EntryCount, AfraEntries], <<>>};

atom_dump_info(moof, Rest) ->
  {"", [], Rest};

atom_dump_info(mfhd, <<0:32, SequenceNumber:32>>) ->
  {"sequence=~p", [SequenceNumber], <<>>};

atom_dump_info(traf, Rest) ->
  {"", [], Rest};

atom_dump_info(tfhd, <<0, Flags:24, TrackId:32, Rest/binary>>) ->
  <<_:7, DurationEmpty:1, _, _:2, SampleFlagsPresent:1, SampleSizePresent:1, SampleDurationPresent:1, _:1, 
  DescriptionPresent:1, DataOffsetPresent:1>> = <<Flags:24>>,
  {BaseDataOffset, Rest1} = extract_number(Rest, 64, DataOffsetPresent),
  {SampleDescriptionIndex, Rest2} = extract_number(Rest1, 32, DescriptionPresent),
  {DefaultSampleDuration, Rest3} = extract_number(Rest2, 32, SampleDurationPresent),
  {DefaultSampleSize, Rest4} = extract_number(Rest3, 32, SampleSizePresent),
  {DefaultSampleFlags, _Rest5} = extract_number(Rest4, 32, SampleFlagsPresent),
  
  {"duration_empty=~p,flags=~p,size=~p,duration=~p,description=~p,offset=~p,track=~p", [
    DurationEmpty, DefaultSampleFlags, DefaultSampleSize, DefaultSampleDuration, SampleDescriptionIndex, BaseDataOffset, TrackId], <<>>};

atom_dump_info(trun, <<0, Flags:24, SampleCount:32, Rest1/binary>>) ->
  <<_:16, _:5, FirstSampleFlagPresent:1, _:1, DataOffsetPresent:1>> = <<Flags:24>>,
  {DataOffset, Rest2} = extract_number(Rest1, 32, DataOffsetPresent),
  {_FirstSampleFlag, Rest3} = extract_number(Rest2, 32, FirstSampleFlagPresent),
  TrunEntries = dump_trun_entries(Rest3, Flags, SampleCount, []),
  
  Dump = [io_lib:format("~n       duration=~p,size=~p,flags=~p,ctime=~p", [D,S,F,C]) || [{duration,D},{size,S},{flags,F},{ctime,C}] <- TrunEntries],
  {"sample_count=~p,data_offset=~p~s", [SampleCount, DataOffset, lists:flatten(Dump)], <<>>};
      
atom_dump_info(mdat, <<Info:10/binary, _/binary>> = Content) ->
  file:write_file("mmm.flv", <<(flv:header())/binary, Content/binary>>),
  {"mdat=~p,~p", [size(Content), Info], <<>>};
  
atom_dump_info(_, _) ->
  {"", [], <<>>}.

extract_number(Bin, _Size, 0) -> {undefined, Bin};
extract_number(Bin, Size, _) -> <<Data:Size, R0/binary>> = Bin, {Data, R0}.


dump_afrt_fragments(Rest, TotalFragments, List) when length(List) >= TotalFragments ->
  {Rest, lists:reverse(List)};

dump_afrt_fragments(<<Frag:32, Time:64, 0:32, _Discontinuity, Rest/binary>>, TotalFragments, List) ->
  dump_afrt_fragments(Rest, TotalFragments, [{Frag,Time,0}|List]);
  
dump_afrt_fragments(<<Frag:32, Time:64, Duration:32, Rest/binary>>, TotalFragments, List) ->
  dump_afrt_fragments(Rest, TotalFragments, [{Frag,Time,Duration}|List]).


dump_afra_entries(Rest, _, 0, List) ->
  {Rest, lists:reverse(List)};

dump_afra_entries(<<Time:64, Offset:32, Rest/binary>>, 0, Count, List) ->
  dump_afra_entries(Rest, 0, Count - 1, [{Time,Offset}|List]);

dump_afra_entries(<<Time:64, Offset:64, Rest/binary>>, 1, Count, List) ->
  dump_afra_entries(Rest, 1, Count - 1, [{Time,Offset}|List]).


dump_trun_entries(_, _, 0, List) ->
  lists:reverse(List);
  
dump_trun_entries(Bin, Flags, SampleCount, List) ->
  {SampleDuration, Rest1} = extract_number(Bin, 32, Flags band 16#0100),
  {SampleSize, Rest2} = extract_number(Rest1, 32, Flags band 16#0200),
  {SampleFlags, Rest3} = extract_number(Rest2, 32, Flags band 16#0400),
  {SampleCompositionTimeOffset, Rest3} = extract_number(Rest2, 32, Flags band 16#0800),
  Info = [{duration,SampleDuration},{size,SampleSize},{flags,SampleFlags},{ctime,SampleCompositionTimeOffset}],
  dump_trun_entries(Rest3, Flags, SampleCount - 1, [Info|List]).

open(Reader, _Options) ->
  {_T1, {ok, Mp4Media}} = timer:tc(fun() -> read_header(Reader) end),
  % {_T2, Index} = timer:tc(fun() -> build_index(Tracks) end),
  {ok, Mp4Media#mp4_media{reader = Reader}}.



keyframes(_Media, []) ->
  {error, no_keyframes};

keyframes(#mp4_media{tracks = Tracks} = Media, [TrackId|TrackIds]) ->
  case element(TrackId, Tracks) of
    #mp4_track{content = video, track_id = TrackId, keyframes = Keyframes} -> Keyframes;
    #mp4_track{} -> keyframes(Media, TrackIds)
  end.


read_header(Reader) ->
  read_header(#mp4_media{tracks = {}}, Reader, 0).




read_header(#mp4_media{additional = Additional} = Mp4Media, {Module, Device} = Reader, Pos) -> 
  case read_atom_header(Reader, Pos) of
    eof -> {ok, Mp4Media};
    {error, Reason} -> {error, Reason};
    {atom, mdat, Offset, all_file} ->
      {ok, Mp4Media#mp4_media{data_borders = {Offset, eof}}};
    {atom, mdat, Offset, Length} ->
      read_header(Mp4Media#mp4_media{data_borders = {Offset, Length}}, Reader, Offset + Length);
    {atom, _AtomName, Offset, 0} -> 
      read_header(Mp4Media, Reader, Offset);
    {atom, AtomName, Offset, Length} -> 
      % ?D({"Root atom", AtomName, Length}),
      {ok, AtomData} = Module:pread(Device, Offset, Length),
      % ?D({atom, AtomName, size(AtomData)}),
      NewMedia = case atom_to_binary(AtomName, latin1) of
        <<"EV", _/binary>> ->
          Mp4Media#mp4_media{additional = [{AtomName,AtomData}| Additional]};
        _ ->
          case erlang:function_exported(?MODULE, AtomName, 2) of
            true -> ?MODULE:AtomName(AtomData, Mp4Media);
            false -> Mp4Media
          end
      end,
      read_header(NewMedia, Reader, Offset + Length)
  end.

read_atom_header({Module, Device}, Pos) ->
  case Module:pread(Device, Pos, 8) of
    {ok, <<0:32, AtomName/binary>>} ->
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, all_file};
    {ok, <<1:32, AtomName/binary>>} ->
      case Module:pread(Device, Pos+4, 12) of
        {ok, <<AtomName:4/binary, AtomLength:64>>} when AtomLength >= 12 -> 
          {atom, binary_to_atom(AtomName, latin1), Pos + 16, AtomLength - 16};
        eof ->
          eof;
        {ok, Bin} ->
          ?D({invalid_atom, Bin}),
          {error, {invalid_atom, Bin}};
        {error, Error} ->
          {error, Error}
      end;
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      % ?D({"Atom", binary_to_atom(AtomName, latin1), Pos, AtomLength}),
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, AtomLength - 8};
    eof ->
      eof;
    {ok, Bin} ->
      ?D({invalid_atom, Bin}),
      {error, {invalid_atom, Bin}};
    {error, Error} ->
      {error, Error}
  end.


parse_atom(<<>>, State) ->
  State;

parse_atom(Bin, State) ->
  _T1 = erlang:now(),
  {ok, _Atom, NewState, Rest} = decode_atom(Bin, State),
  _T2 = erlang:now(),
  % ?D({_Atom, timer:now_diff(_T2, _T1)}),
  parse_atom(Rest, NewState).

decode_atom(<<8:32, 0:32>>, Mp4Parser) ->
  Mp4Parser;
  
decode_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  % ?D({atom,BinaryAtomName}),
  AtomName = case binary:bin_to_list(BinaryAtomName) of
   % [169|Value] -> 
   %   binary_to_atom(binary:list_to_bin(Value),latin1);
   _ValidValue ->
     binary_to_atom(BinaryAtomName,latin1)
  end,
  % T1 = erlang:now(),
  NewMp4Parser = case erlang:function_exported(?MODULE, AtomName, 2) of
    true -> ?MODULE:AtomName(Atom, Mp4Parser);
    false -> Mp4Parser
    % false -> Mp4Parser
  end,
  % case timer:now_diff(erlang:now(),T1) of
  %   Delta when Delta > 1000 -> ?DBG("atom ~s took ~B us", [AtomName, Delta]);
  %   _ -> ok
  % end,

  {ok, AtomName, NewMp4Parser, Rest};

decode_atom(<<0:32>>, Mp4Parser) ->
  ?D("NULL atom"),
  Mp4Parser.


%% Adobe bootstrap

abst(<<_Version, 0:24, InfoVersion:32, Profile:2, Live:1, Update:1, 0:4, 
       TimeScale:32, CurrentMediaTime:64, _SmpteTimeCodeOffset:64, Rest1/binary>>, State) ->
  {IdLen, _} = binary:match(Rest1, [<<0>>]),
  <<MovieIdentifier:IdLen/binary, 0, Rest2/binary>> = Rest1,
  <<ServerEntryCount, QualityEntryCount, DrmData, MetaData, SegmentRunTableCount, Rest3/binary>> = Rest2,
  ServerEntryCount = 0,
  QualityEntryCount = 0,
  DrmData = 0,
  MetaData = 0,
  SegmentRunTableCount = 1,
  {ok, asrt, SegmentRunTable, <<FragmentRunTableCount, Rest4/binary>>} = decode_atom(Rest3, State),
  FragmentRunTableCount = 1,
  {ok, afrt, FragmentRunTable, <<>>} = decode_atom(Rest4, State),
  {'Bootstrap', [{version,InfoVersion},{profile,Profile},{live,Live},{update,Update},{timescale,TimeScale},{current_time,CurrentMediaTime},
  {id,MovieIdentifier},{segments,SegmentRunTable},{fragments, FragmentRunTable}], <<>>}.


asrt(<<_Version, Update:24, QualityEntryCount, SegmentRunEntryCount:32, Rest/binary>>, _State) ->
  QualityEntryCount = 0,
  Segments = [{FirstSegment, FragmentsPerSegment} || <<FirstSegment:32, FragmentsPerSegment:32>> <= Rest],
  SegmentRunEntryCount = length(Segments),
  {'SegmentRunTable', [{update,Update}], Segments}.

afrt(<<_Version, Update:24, TimeScale:32, QualityEntryCount, FragmentRunEntryCount:32, Rest/binary>>, _State) ->
  QualityEntryCount = 0,
  Fragments = read_afrt_fragments(Rest),
  FragmentRunEntryCount = length(Fragments),
  {'FragmentRunEntry', [{update,Update},{timescale,TimeScale}],Fragments}.
  
read_afrt_fragments(Bin) -> read_afrt_fragments(Bin, []).

read_afrt_fragments(<<FirstFragment:32, FirstFragmentTimestamp:64, 0:32, DiscontinuityIndicator, Rest/binary>>, Acc) ->
   read_afrt_fragments(Rest, [{FirstFragment, FirstFragmentTimestamp, 0, DiscontinuityIndicator}|Acc]);

read_afrt_fragments(<<FirstFragment:32, FirstFragmentTimestamp:64, FragmentDuration:32, Rest/binary>>, Acc) ->
  read_afrt_fragments(Rest, [{FirstFragment, FirstFragmentTimestamp, FragmentDuration}|Acc]);

read_afrt_fragments(_, Acc) ->
  lists:reverse(Acc).



% FTYP atom
ftyp(<<_Major:4/binary, _Minor:4/binary, _CompatibleBrands/binary>>, MediaInfo) ->
  % ?D({"File", _Major, _Minor, ftyp(_CompatibleBrands, [])}),
  % NewParser = Mp4Parser#mp4_header{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  MediaInfo;

ftyp(<<>>, BrandList) when is_list(BrandList) ->
  lists:reverse(BrandList);

ftyp(<<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  ftyp(CompatibleBrands, [Brand|BrandList]).
  
% Movie box
moov(Atom, MediaInfo) ->
  Media = parse_atom(Atom, MediaInfo),
  Media.

free(_Atom, Media) ->
  Media.

% MVHD atom
mvhd(<<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, _RateDelim:16,
      Volume:16, 0:16, _Reserved1:64, Matrix:36/binary, _Reserved2:24/binary, NextTrackId:32>>, #mp4_media{} = Media) ->
        
  _Meta = [{ctime,CTime},{mtime,MTime},{timescale,TimeScale},{duration,Duration},{rate,Rate},
          {volume,Volume},{matrix,Matrix},{next_track,NextTrackId}],
  % ?D(Meta),
  Media#mp4_media{timescale = TimeScale, duration = Duration*1000 div TimeScale}.

udta(UDTA, Media) ->
  parse_atom(UDTA, Media).

meta(<<0:32, Meta/binary>>, #mp4_media{} = Media) ->
  parse_atom(Meta, Media);

meta(_, #mp4_media{} = Media) ->
  Media.


data(<<_Flags:32, 0:32, Data/binary>>, _Meaning) ->
  Data.

% '----'(Meta, #mp4_media{} = Media) ->

% Track box
trak(<<>>, MediaInfo) ->
  MediaInfo;
  
trak(Atom, #mp4_media{tracks = Tracks, duration = Duration} = Media) ->
  #mp4_track{codec = Codec, duration = TrackDuration, timescale = Timescale} =Track = parse_atom(Atom, #mp4_track{number = size(Tracks)+1}),
  case Codec of
    undefined -> ?D({skip_mp4_track, undefined_codec}),Media;
    _ -> Media#mp4_media{tracks = erlang:append_element(Tracks,Track), duration = lists:max([Duration,TrackDuration*1000/Timescale])}
  end.



  

% Track header
tkhd(<<0, Flags:24, CTime:32, MTime:32, TrackID:32, _Reserved1:32, 
       Duration:32, _Reserved2:64, Layer:16, _AlternateGroup:16,
       Volume, _VolDelim, _Reserved3:16, Matrix:36/binary, Width:16, _WidthDelim:16, Height:16, _HeightDelim:16>>, Mp4Track) ->
  _Meta = [{flags,Flags},{ctime,CTime},{mtime,MTime},{track_id,TrackID},{duration,Duration},{layer,Layer},
         {volume,Volume},{matrix,Matrix},{width,Width},{height,Height}],
  % ?D(Meta),
  Mp4Track#mp4_track{track_id = TrackID, width = Width, height = Height}.

% Media box
mdia(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Media header
mdhd(<<0:8, _Flags:24, _Ctime:32, _Mtime:32, TimeScale:32, Duration:32,
       _:1, Language:15/bitstring, _Quality:16>>, #mp4_track{} = Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)};

mdhd(<<1:8, _Flags:24, _Ctime:64, _Mtime:64, TimeScale:32, Duration:64, 
       _:1, Language:15/bitstring, _Quality:16>>, Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)}.
  
extract_language(<<L1:5, L2:5, L3:5>>) ->
  case list_to_binary([L1+16#60, L2+16#60, L3+16#60]) of
    <<"und">> -> undefined;
    Else -> Else
  end.


%% Handler Reference Box
hdlr(<<0:32, 0:32, _Handler:4/binary, _Reserved:8/binary, NameNull/binary>>, #mp4_media{} = Mp4Media) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  % ?D({hdlr, Handler, Name}),
  Mp4Media;


hdlr(<<0:32, _Mhdl:32, Handler:4/binary, _Reserved:8/binary, _NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  case Handler of
    <<"vide">> -> Mp4Track#mp4_track{content = video};
    <<"soun">> -> Mp4Track#mp4_track{content = audio};
    _ ->
      Content = case Mp4Track#mp4_track.content of
        undefined -> binary_to_atom(Handler, latin1);
        OldContent -> OldContent
      end,
      Mp4Track#mp4_track{content = Content}
  end.


  
% Media information
minf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).


edts(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

elst(<<Version, _Flags:24, _EntryCount:32, ELST/binary>>, Mp4Track) ->
  Records = case Version of
    0 -> [{Duration, Time, Rate} || <<Duration:32, Time:32, Rate:16, _Fraction:16>> <= ELST];
    1 -> [{Duration, Time, Rate} || <<Duration:64, Time:64, Rate:16, _Fraction:16>> <= ELST]
  end,
  Mp4Track#mp4_track{elst = Records}.


  
dinf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).
  

dref(<<0:32, _Count:32, Atom/binary>> = _Dref, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

'url '(_URL, Mp4Track) ->
  Mp4Track.

% Sample table box
stbl(Atom, #mp4_track{} = Mp4Track1) ->
  _T1 = erlang:now(),
  Mp4Track2 = parse_atom(Atom, Mp4Track1),
  _T2 = erlang:now(),
  Mp4Track3 = unpack_samples_in_chunk(Mp4Track2),

  #mp4_track{
    total_bytes = TotalBytes
    ,duration = Duration
  %   chunk_offsets = ChunkOffsets,
  %   chunk_sizes = ChunkSizes,
  %   sample_sizes = SampleSizes,
    ,sample_dts = Timestamps
  %   sample_composition = Compositions,
    ,keyframes = Keyframes1
    ,timescale = Timescale
  } = Mp4Track3,
  Bitrate = (TotalBytes*8*Timescale div Duration) div 1024, 
  % ?D({bitrate,TrackId,TotalBytes, Duration div Timescale, Bitrate}),
  {_T3, Keyframes} = timer:tc(fun() -> lists:zipwith(fun(Id, DTS) -> {DTS*1000/Timescale, Id} end,
    Keyframes1, lookup_dts(Timestamps, Keyframes1)) end),
  % {_T2, {Frames, IndexInfo}} = timer:tc(fun() -> fill_track(TrackId, SampleSizes, ChunkOffsets, ChunkSizes, Keyframes, Timestamps, Compositions, Timescale) end),
  
  % [{Duration,_}|_] = IndexInfo,
  TrackDuration = lists:max([Duration, lists:last([0|Keyframes1]) + 1000]),
  R = Mp4Track3#mp4_track{keyframes = Keyframes, bitrate = Bitrate, duration = TrackDuration},

  _T4 = erlang:now(),
  % ?DBG("stbl: No ~B ~s, ~B kbps, load ~B us, unpack ~B us, ~B us keyframing, ~B keyframes", [TrackId, R#mp4_track.content, Bitrate, 
  %   timer:now_diff(_T2,_T1), timer:now_diff(_T4,_T2), _T3, length(Keyframes)]),
  R.

% Sample description
stsd(<<0:8, _Flags:3/binary, _EntryCount:32, EntryData/binary>>, Mp4Track) ->
  % ?D({_EntryCount, EntryData}),
  parse_atom(EntryData, Mp4Track).

mp4a(<<0:32>>, Mp4Track) ->
  Mp4Track;

mp4a(<<_Reserved:6/binary, _RefIndex:16, SoundVersion:16, _Unknown:6/binary, _ChannelsCount:16,
       _SampleSize:16, _PacketSize:16, _TimeScale:32, _Reserved3:16, Atom/binary>>, Mp4Track) when SoundVersion == 0 ->
  parse_atom(Atom, Mp4Track#mp4_track{codec = aac});

mp4a(<<_Reserved:6/binary, _RefIndex:16, SoundVersion:16, _Reserved2:6/binary, _ChannelsCount:16,
       _SampleSize:16, _Reserved3:16, _PacketSize:16, _TimeScale:16, _Reserved4:16, 
       _SamplesPerPacket:32, _BytesPerPacket:32, _BytesPerFrame:32, _BytesPerSample:32, Atom/binary>>, Mp4Track) when SoundVersion == 1 ->
  parse_atom(Atom, Mp4Track#mp4_track{codec = aac}).

wave(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

mp4v(_T, Mp4Track) ->
  Mp4Track#mp4_track{codec = mpeg4}.


'.mp3'(_, Mp4Track) ->
  Mp4Track#mp4_track{codec = mp3}.


avc1(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
      HorizRes:16, _:16, VertRes:16, _:16, _FrameCount:16, _CompressorName:32/binary,
      Depth:16, _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  _Meta = [{width,Width},{height,Height},
          {horiz_res, HorizRes},{vert_res, VertRes},
          {depth,Depth}],
  % ?D({"Video size:", Meta}),
  parse_atom(Atom, Mp4Track#mp4_track{codec = h264, width = Width, height = Height}).

s263(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
       _HorizRes:32, _VertRes:32, _FrameCount:16, _CompressorName:32/binary, _Depth:16, 
       _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{codec = s263, width = Width, height = Height}).

samr(<<_Reserved:2/binary, _RefIndex:16, Atom/binary>> = AMR, Mp4Track) ->
  ?D(AMR),
  parse_atom(Atom, Mp4Track#mp4_track{codec = samr}).


'pcm '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{codec = pcm_le}.

'spx '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{codec = speex}.

  
%%%%%%%%%%%%%%%%%%    ESDS     %%%%%%%%%%%%%%
esds(<<Version:8, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{} = Mp4Track) when Version == 0 ->
  % ?D({"Extracted audio config", DecoderConfig}),
  ESDS = config_from_esds_tag(DecoderConfig),
  % ?D(ESDS),
  Mp4Track#mp4_track{decoder_config = ESDS#esds.specific, codec = ESDS#esds.object_type}.

% avcC atom
avcC(DecoderConfig, #mp4_track{} = Mp4Track) ->
  % ?D({"Extracted video config", DecoderConfig, h264:unpack_config(DecoderConfig)}),
  Mp4Track#mp4_track{decoder_config = DecoderConfig}.

btrt(<<_BufferSize:32, MaxBitRate:32, AvgBitRate:32>>, #mp4_track{} = Mp4Track) ->
  % ?D({btrt,MaxBitRate, AvgBitRate}),
  Mp4Track#mp4_track{max_bitrate = MaxBitRate, bitrate = AvgBitRate}.


%% FIXME: Code here must be relocated in some more generic place and way. 
%% Here goes not some esds tag, but IOD (Initial Object Description)
%% Look how to parse it at vlc/modules/demux/ts.c:2400
%%

parse_esds(Data) -> config_from_esds_tag(Data).

config_from_esds_tag(Data) ->
  config_from_esds_tag(Data, #esds{}).

config_from_esds_tag(Data, ESDS) ->
  case mp4_read_tag(Data) of
    % Good description is vlc/modules/mux/ts.c:2068 GetPMTmpeg4
    {es_descr, <<_ID1:16, _Priority1, Description/binary>>, <<>>} ->
      config_from_esds_tag(Description, ESDS);
    {decoder_config, <<ObjectType, StreamType, BufferSize:24, MaxBitrate:32, AvgBitrate:32, Rest1/binary>>, Rest2} ->
      ESDS1 = config_from_esds_tag(Rest1, ESDS#esds{
        object_type = mp4_object_type(ObjectType), stream_type = StreamType, buffer_size = BufferSize,
        max_bitrate = MaxBitrate, avg_bitrate = AvgBitrate}),
      config_from_esds_tag(Rest2, ESDS1);
    {decoder_specific, <<Config/binary>>, _} ->
      ESDS#esds{specific = Config};
    {sl, _, Rest} ->
      config_from_esds_tag(Rest, ESDS);
    {_Tag, _Data, Rest} ->
      ?D({"Unknown esds tag. Send this line to max@maxidoors.ru: ", _Tag, _Data}),
      config_from_esds_tag(Rest, ESDS);
    undefined ->
      ESDS
  end.

mp4_object_type(8) -> text;
mp4_object_type(16#20) -> mpeg4;
mp4_object_type(16#21) -> h264;
mp4_object_type(16#40) -> aac;
mp4_object_type(16#66) -> aac;
mp4_object_type(16#67) -> aac;
mp4_object_type(16#68) -> aac;
mp4_object_type(16#69) -> mp3;
mp4_object_type(16#6B) -> mp3.

mp4_desc_length(<<0:1, Length:7, Rest:Length/binary, Rest2/binary>>) ->
  {Rest, Rest2};

mp4_desc_length(<<1:1, Length1:7, 0:1, Length:7, Rest/binary>>) ->
  TagLength = Length1 * 128 + Length,
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length3:7, 1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length3 bsl 21 + Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2}.

mp4_read_tag(<<>>) ->
  undefined;

mp4_read_tag(<<TagId, Data/binary>>) ->
  {Body, Rest} = mp4_desc_length(Data),
  Tag = case TagId of
    2 -> es;
    3 -> es_descr;
    4 -> decoder_config;
    5 -> decoder_specific;
    6 -> sl;
    _ -> TagId
  end,
  {Tag, Body, Rest}.











%%%%%%%%%%%%%%%%%%    STSZ  %%%%%%%%%%%%%%%%
% Sample sizes in bytes are stored here
%%
stsz(<<_Version:8, _Flags:24, 0:32, SampleCount:32, SampleSizeData/binary>>, Mp4Track) -> % case for different sizes
  size(SampleSizeData) == SampleCount*4 orelse erlang:error({broken_stsz,SampleCount,size(SampleSizeData)}),
  % ?D({stsz, SampleCount}),
  TotalBytes = aggregate_stsz(SampleSizeData, 0),
  Mp4Track#mp4_track{sample_sizes = SampleSizeData, sample_count = SampleCount, total_bytes = TotalBytes};
  
stsz(<<_Version:8, _Flags:24, Size:32, SampleCount:32>>, Mp4Track) ->
  Mp4Track#mp4_track{sample_sizes = Size, sample_count = SampleCount, total_bytes = Size*SampleCount}.
  

aggregate_stsz(<<>>, Total) -> Total;
aggregate_stsz(<<Size:32, Rest/binary>>, Total) -> aggregate_stsz(Rest, Total + Size).



  

%%%%%%%%%%%%%%%%%%% STTS %%%%%%%%%%%%%%%%%%%
% Sample durations (dts delta between neigbour frames)
%%
stts(<<0:8, _Flags:3/binary, EntryCount:32, Timestamps/binary>>, Mp4Track) ->
  size(Timestamps) == EntryCount*8 orelse erlang:error({invalid_stts,EntryCount,size(Timestamps)}),
  Duration = stts_duration(Timestamps),
  Mp4Track#mp4_track{sample_dts = Timestamps, duration = Duration}.

stts_duration(STTS) -> stts_duration(STTS, 0).
stts_duration(<<>>, Total) -> Total;
stts_duration(<<Count:32, Duration:32, STTS/binary>>, Total) -> stts_duration(STTS, Total + Count*Duration).

  

lookup_dts(Timestamps, Id) when is_number(Id) -> lookup_dts(Timestamps, [Id]);
lookup_dts(_, []) -> [];
lookup_dts(STTS, [Id|Ids]) -> lookup_dts(STTS, Id, Ids, 0, 0).


lookup_dts(<<Count:32, Duration:32, _/binary>> = STTS, Id, Ids, DTS, TotalCount) when Id < Count ->
  [DTS + Id*Duration|case Ids of
    [Id1|Ids1] -> lookup_dts(STTS, Id1 - TotalCount, Ids1, DTS, TotalCount);
    [] -> []
  end];
lookup_dts(<<Count:32, Duration:32, STTS/binary>>, Id, Ids, DTS, TotalCount) ->
  lookup_dts(STTS, Id - Count, Ids, DTS + Count*Duration, TotalCount+Count).


lookup_audio_dts(STTS, StartDTS, EndDTS) ->
  lookup_adts(STTS, StartDTS, EndDTS, 0, 0).

lookup_adts(<<Count:32, Duration:32, STTS/binary>>, StartDTS, EndDTS, DTS, FirstId) when Count*Duration + DTS < StartDTS ->
  % ?D({skip_adts,DTS,Count,Duration,StartDTS}),
  lookup_adts(STTS, StartDTS, EndDTS, DTS + Count*Duration, FirstId + Count);

lookup_adts(<<Count:32, Duration:32, STTS/binary>>, StartDTS, EndDTS, DTS, FirstId) ->
  % ?D({work_adts,Count,Duration,StartDTS,EndDTS}),
  Before = if 
    (StartDTS - DTS) rem Duration == 0 -> (StartDTS - DTS) div Duration;
    true -> ((StartDTS - DTS) div Duration) + 1
  end,
  Start = Before + FirstId,
  RealStartDTS = Before*Duration + DTS,
  % ?debugFmt("~B + ~B*~B = ~B <?> ~B", [DTS, Count, Duration, DTS + Count*Duration, EndDTS]),
  if DTS + Count*Duration < EndDTS ->
    OurTimestamps = lists:seq(RealStartDTS, DTS + (Count-1)*Duration, Duration),
    {_, End, NextTimestamps} = lookup_adts(STTS, DTS + Count*Duration, EndDTS, DTS + Count*Duration, FirstId + Count),
    {Start, End, OurTimestamps ++ NextTimestamps};
  true ->
    Samples = lists:min([(EndDTS - RealStartDTS) div Duration, Count - Before - 1]),
    Timestamps = lists:seq(RealStartDTS, DTS + (Before+Samples)*Duration, Duration),
    {Start, Start+Samples, Timestamps}
  end.


  


%%%%%%%%%%%%%%%%%%%%% STSS atom %%%%%%%%%%%%%%%%%%%
% List of keyframes
%
stss(<<0:8, _Flags:3/binary, KFCount:32, KeyframeData/binary>>, Mp4Track) ->
  size(KeyframeData) == KFCount*4 orelse erlang:error({invalid_stss,KFCount, size(KeyframeData)}),
  % ?D({stss, [Number - 1 || <<Number:32>> <= KeyframeData]}),
  Mp4Track#mp4_track{keyframes = [Number - 1 || <<Number:32>> <= KeyframeData]}.




%%%%%%%%%%%%%%%%%%%%%% CTTS atom  %%%%%%%%%%%%%%%%%%%%%%%
% list of B-Frames offsets
%%
ctts(<<0:32, EntryCount:32, CTTS/binary>>, Mp4Track) ->
  size(CTTS) == EntryCount*8 orelse erlang:error({invalid_ctts,EntryCount,size(CTTS)}),
  % Info = [{Count,Offset} || <<Count:32, Offset:32>> <= CTTS],
  Mp4Track#mp4_track{sample_composition = CTTS}.

compositions(undefined, _, _) -> undefined;
compositions([], _, _) -> undefined;
compositions(_, Start, End) when Start > End -> [];
compositions(<<Count:32, _Offset:32, CTTS/binary>>, Start, End) when Start >= Count ->
  compositions(CTTS, Start - Count, End - Count);

compositions(<<Count:32, Offset:32, _/binary>> = CTTS, Start, End) when Start < Count ->
  [Offset|compositions(CTTS, Start +1, End)].

  
%%%%%%%%%%%%%%%%%%%%% STSC %%%%%%%%%%%%%%%%%%%%%%%
% Samples per chunk
%%
stsc(<<0:8, _Flags:3/binary, EntryCount:32, ChunkSizes/binary>>, Mp4Track) ->
  size(ChunkSizes) == EntryCount*12 orelse erlang:error({invalid_stsc,EntryCount,size(ChunkSizes)}),
  Mp4Track#mp4_track{chunk_sizes = [{ChunkId, SamplesPerChunk} || <<ChunkId:32, SamplesPerChunk:32, _SampleId:32>> <= ChunkSizes]}.
  % Mp4Track#mp4_track{chunk_sizes = ChunkSizes}.



%%%%%%%%%%%%%%%%%%%%%% STCO/CO64 atom %%%%%%%%%%%%%%%%%%%%
% sample table chunk offset
%%
stco(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  size(Offsets) == OffsetCount*4 orelse erlang:error({invalid_stco,OffsetCount,size(Offsets)}),
  Mp4Track#mp4_track{chunk_offsets = Offsets, offset_size = 4}.

co64(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  size(Offsets) == OffsetCount*8 orelse erlang:error({invalid_co64,OffsetCount,size(Offsets)}),
  % ?D({co64,OffsetCount}),
  Mp4Track#mp4_track{chunk_offsets = Offsets, offset_size = 8}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_gop(#mp4_media{} = _, N, _) when N =< 0 ->
  {error, no_segment};

read_gop(#mp4_media{} = Media, N, TrackIds) ->
  try read_gop0(Media, N, TrackIds)
  catch
    throw:Reply -> Reply
  end.

read_gop0(#mp4_media{tracks = Tracks} = Media, N, undefined) ->
  F = fun
    (_, _, I) when I > tuple_size(Tracks) -> [];
    (G, Content, I) ->
    case element(I, Tracks) of
      #mp4_track{content = C} when C == Content -> [I];
      _ -> G(G, Content, I+1)
    end
  end,
  read_gop0(Media, N, F(F, video, 1) ++ F(F, audio, 1));

read_gop0(#mp4_media{tracks = Tracks, reader = {Module,Device}} = Media, N, [A_]) when (element(A_,Tracks))#mp4_track.content == audio ->
  Keyframes = video_frame:reduce_keyframes(default_keyframes(Media, A_)),
  N < length(Keyframes) orelse throw({error, no_segment}),
  #mp4_track{content = audio, sample_dts = ATimestamps, timescale = AScale, codec = ACodec, duration = Duration} = A = element(A_, Tracks),
  {{VStartDTS, _}, {VEndDTS, _}} = case lists:nthtail(N-1, video_frame:reduce_keyframes(Keyframes)) of
    [K1,K2|_] -> {K1,K2};
    [K1] -> {K1,{Duration + 1000, undefined}}
  end,
  {AStart, AEnd, TSList} = lookup_audio_dts(ATimestamps, round(VStartDTS*AScale/1000), round(VEndDTS*AScale/1000)),
  AOffsets = lookup_offsets(A, AStart, AEnd),
  AFrames1 = collect_frames(TSList, undefined, AOffsets, audio, ACodec, A_, AScale),
  AFrames = read_disk_frames(Module, Device, AFrames1),
  {ok, AFrames};


read_gop0(#mp4_media{tracks = Tracks, reader = {Module, Device}}, N, [V_]) when (element(V_,Tracks))#mp4_track.content == video ->
  #mp4_track{content = video, keyframes = Keyframes1, sample_count = VideoCount} = V = element(V_, Tracks),
  Keyframes = video_frame:reduce_keyframes(Keyframes1),
  N =< length(Keyframes) orelse throw({error, no_segment}),
  
  {{_VStartDTS, VStart}, {_VEndDTS, VEnd}} = case lists:nthtail(N-1, Keyframes) of
    [K1,K2|_] -> {K1,K2};
    [K1] -> {K1,{undefined, VideoCount}}
  end,
  VideoFrames1 = load_frames(V, VStart, VEnd-1),
  VideoFrames2 = mark_keyframes(VideoFrames1, VStart, Keyframes1),
  VideoFrames3 = read_disk_frames(Module, Device, VideoFrames2),
  VideoFrames = VideoFrames3,

  {ok, VideoFrames};


read_gop0(#mp4_media{tracks = Tracks} = Media, N, [A_,V_]) when 
  (element(A_,Tracks))#mp4_track.content == audio, (element(V_,Tracks))#mp4_track.content == video ->
  read_gop0(Media, N, [V_,A_]);

read_gop0(#mp4_media{tracks = Tracks, reader = {Module, Device}}, N, [V_,A_]) when
  (element(A_,Tracks))#mp4_track.content == audio, (element(V_,Tracks))#mp4_track.content == video ->

  #mp4_track{content = video, keyframes = Keyframes1, sample_count = VideoCount} = V = element(V_, Tracks),
  #mp4_track{content = audio, sample_dts = ATimestamps, timescale = AScale, codec = ACodec, duration = Duration} = A = element(A_, Tracks),
  Keyframes = video_frame:reduce_keyframes(Keyframes1),
  N =< length(Keyframes) orelse throw({error, no_segment}),
  
  {{VStartDTS, VStart}, {VEndDTS, VEnd}} = case lists:nthtail(N-1, Keyframes) of
    [K1,K2|_] -> {K1,K2};
    [K1] -> {K1,{Duration*1000/AScale, VideoCount}}
  end,
  VideoFrames1 = load_frames(V, VStart, VEnd-1),
  VideoFrames2 = mark_keyframes(VideoFrames1, VStart, Keyframes1),

  VideoFrames3 = read_disk_frames(Module, Device, VideoFrames2),
  VideoFrames = VideoFrames3,

  % ?debugFmt("lookup aframes in (~.1f,~.1f) -> (~B,~B) (dur: ~p)", 
  %   [VStartDTS, VEndDTS, round(VStartDTS*AScale/1000), round(VEndDTS*AScale/1000), Duration]),
  {AStart, AEnd, TSList} = lookup_audio_dts(ATimestamps, round(VStartDTS*AScale/1000), round(VEndDTS*AScale/1000)),
  AOffsets = lookup_offsets(A, AStart, AEnd),
  length(TSList) == length(AOffsets) orelse error({bad_audio,length(TSList), length(AOffsets)}),
  AFrames1 = collect_frames(TSList, undefined, AOffsets, audio, ACodec, A_, AScale),
  AFrames = read_disk_frames(Module, Device, AFrames1),
  {ok, video_frame:sort(VideoFrames ++ AFrames)}.

mark_keyframes([], _, _) -> [];
mark_keyframes(Frames, _, []) -> Frames;
mark_keyframes([#video_frame{} = Frame|Frames], N, [{_,N}|Keyframes]) ->
  [Frame#video_frame{flavor = keyframe}|mark_keyframes(Frames,N+1,Keyframes)];
mark_keyframes([#video_frame{} = Frame|Frames], N1, [{_,N2}|_] = Keyframes) when N1 < N2 ->
  [Frame|mark_keyframes(Frames,N1+1, Keyframes)];
mark_keyframes(Frames, N1, [{_,N2}|Keyframes]) when N1 > N2 ->
  mark_keyframes(Frames, N1, Keyframes).


unok({ok, Bin}) -> Bin.

read_disk_frames(_Module, _Device, []) ->
  [];

read_disk_frames(Module, Device, Frames) ->
  Requests = [{RequestOffset,_}|_] = lists:sort([Body || #video_frame{body = Body} <- Frames]),
  {LastOffset,LastSize} = lists:last(Requests),
  RequestSize = LastOffset + LastSize - RequestOffset,
  SumSize = lists:sum([Size || {_,Size} <- Requests]),
  if SumSize > 0.3*RequestSize ->
    {ok, Bin} = Module:pread(Device, RequestOffset, RequestSize),
    [begin
      SmallOffset = Offset - RequestOffset,
      <<_:SmallOffset/binary, Body:Size/binary, _/binary>> = Bin,
      F#video_frame{body = Body}
    end || #video_frame{body = {Offset,Size}} = F <- Frames];
  true ->
    [F#video_frame{body = unok(Module:pread(Device, Offset, Size))} || #video_frame{body = {Offset,Size}} = F <- Frames]
  end.

% Code above does very simple request gluing. This should once replace it
%
% reduce_requests([]) -> [];
% reduce_requests([Request]) -> [Request];
% reduce_requests([{O1,S1},{O2,S2}|Requests]) when O2 - O1 > 3*S1 ->
%   [{O1,S1}|reduce_requests([{O2,S2}|Requests])];
% reduce_requests([{O1,_},{O2,S2}|Requests]) ->
%   reduce_requests([{O1,S2+O2-O1}|Requests]).



default_keyframes(#mp4_media{} = Media, TrackId) -> default_keyframes(Media, TrackId, 1).
default_keyframes(#mp4_media{tracks = Tracks} = Media, TrackId, N) ->
  case element(N, Tracks) of
    #mp4_track{content = video} -> keyframes(Media, [N, TrackId]);
    _ -> default_keyframes(Media, TrackId, N+1)
  end.







-record(chunk, {
  first,
  last,
  first_id,
  last_id,
  samples
}).


unpack_samples_in_chunk(#mp4_track{chunk_offsets = Offsets, offset_size = OffsetSize, chunk_sizes = ChunkSizes} = Mp4Track) ->
  ChunkCount = size(Offsets) div OffsetSize,
  Mp4Track#mp4_track{chunk_sizes = unpack_samples_in_chunk(ChunkSizes, ChunkCount, 0)}.


unpack_samples_in_chunk([{FirstChunk,SamplesInChunk}], ChunkCount, FirstSample) ->
  LastSample = FirstSample + (ChunkCount - FirstChunk + 1)*SamplesInChunk,
  [#chunk{first = FirstChunk-1, last = ChunkCount - 1, first_id = FirstSample, last_id = LastSample, samples = SamplesInChunk}];

unpack_samples_in_chunk([{FirstChunk,SamplesInChunk}|[{NextChunk, _}|_] = ChunkSizes], ChunkCount, FirstSample) ->
  NextSample = FirstSample + (NextChunk - FirstChunk)*SamplesInChunk,
  [#chunk{first = FirstChunk-1, last = NextChunk-2, first_id = FirstSample, last_id = NextSample - 1, samples = SamplesInChunk}|
    unpack_samples_in_chunk(ChunkSizes, ChunkCount, NextSample)].



load_frames(#mp4_track{timescale = Timescale, sample_dts = Timestamps, track_id = TrackId, 
    sample_composition = CTTS, content = Content, codec = Codec} = Track, Start, End) ->
  Ids = lists:seq(Start, End),
  TSList = lookup_dts(Timestamps, Ids),
  Compositions = compositions(CTTS, Start, End),
  Offsets = lookup_offsets(Track, Start, End),
  Frames = collect_frames(TSList, Compositions, Offsets, Content, Codec, TrackId, Timescale),
  Frames.

collect_frames([], _, [], _, _, _, _) -> [];
collect_frames([TS|TSList], Compositions, [OffsetAndSize|Offsets], Content, Codec, TrackId, Timescale) ->
  % if Content == audio -> io:format("~4.. s ~5.. B:  ~B@~B~n", [Codec, round(DTS)] ++ tuple_to_list(OffsetAndSize));
  %   true -> ok end,
  DTS = TS*1000/Timescale,
  {PTS, Compositions1} = case Compositions of
    undefined -> {DTS, undefined};
    [C|C_] -> {(TS+C)*1000/Timescale, C_}
  end,
  [#video_frame{dts = DTS, pts = PTS, body = OffsetAndSize, content = Content, codec = Codec, flavor = frame, track_id = TrackId}|
  collect_frames(TSList, Compositions1, Offsets, Content, Codec, TrackId, Timescale)].




lookup_offsets(#mp4_track{chunk_sizes = ChunkSizes, chunk_offsets = ChunkOffsets, offset_size = OffsetSize, sample_sizes = SampleSizes} = _T, Start, End) ->
  % if _T#mp4_track.codec == aac -> ?D({Start, End, ChunkSizes}); true -> ok end,
  lookup_offsets(ChunkSizes, {ChunkOffsets, OffsetSize}, SampleSizes, Start, End).

lookup_offsets([#chunk{last_id = Last} = _C|ChunkSizes], ChunkOffsets, SampleSizes, Start, End) when Start > Last ->
  lookup_offsets(ChunkSizes, ChunkOffsets, SampleSizes, Start, End);
  
lookup_offsets([#chunk{first = First, first_id = FirstId, samples = Samples}|_] = ChunkSizes, ChunkOffsets, SampleSizes, Start, End) ->
  % ?D({go, hd(ChunkSizes)}),
  ChunkId = (Start - FirstId) div Samples + First,
  SamplesInChunkBefore = (Start - FirstId) rem Samples,
  LeftSamplesInChunk = Samples - SamplesInChunkBefore,
  FirstSampleInChunk = Start - SamplesInChunkBefore,
  SkipSampleSizes = FirstSampleInChunk*4,
  NeedSampleSizes = SamplesInChunkBefore*4,
  <<_:SkipSampleSizes/binary, SizesBefore:NeedSampleSizes/binary, OtherSampleSizes/binary>> = SampleSizes,
  OffsetInChunk = lists:sum([S || <<S:32>> <= SizesBefore]),
  ChunkOffset = chunk_offset(ChunkOffsets, ChunkId),
  % ?DBG("first sample. chunk:~B, samples_before:~B, chunk_offset: ~B, offset_in_chunk:~B", [ChunkId, SamplesInChunkBefore, ChunkOffset, OffsetInChunk]),
  OffsetsAndSizes = collect_offsets(ChunkSizes, ChunkId, LeftSamplesInChunk, ChunkOffsets, ChunkOffset + OffsetInChunk, OtherSampleSizes, Start, End),
  % ?D(OffsetsAndSizes),
  OffsetsAndSizes.


collect_offsets(_ChunkSizes, _ChunkId, _LeftSamplesInChunk, _ChunkOffsets, _Offset, _SampleSizes, Start, End) when Start > End ->
  [];

collect_offsets([#chunk{last = Last, samples = Samples}|_] = ChunkSizes, ChunkId, 
  _LeftSamplesInChunk = 0, ChunkOffsets, _Offset, SampleSizes, Start, End) when ChunkId < Last ->
  % ?D({next_chunk,ChunkId+1,Samples}),
  collect_offsets(ChunkSizes, ChunkId + 1, Samples, ChunkOffsets, chunk_offset(ChunkOffsets, ChunkId + 1), SampleSizes, Start, End);

collect_offsets([#chunk{last = Last}|[#chunk{samples = Samples} |_] = ChunkSizes], ChunkId, 
  _LeftSamplesInChunk = 0, ChunkOffsets, _Offset, SampleSizes, Start, End) when ChunkId >= Last ->
  collect_offsets(ChunkSizes, ChunkId + 1, Samples, ChunkOffsets, chunk_offset(ChunkOffsets, ChunkId + 1), SampleSizes, Start, End);


collect_offsets(ChunkSizes, ChunkId, LeftSamplesInChunk, ChunkOffsets, Offset, <<Size:32, SampleSizes/binary>>, Start, End) 
  when LeftSamplesInChunk > 0 ->
  % ?D({ChunkId,Offset,Size,LeftSamplesInChunk}),
  [{Offset, Size}|collect_offsets(ChunkSizes, ChunkId, LeftSamplesInChunk - 1, ChunkOffsets, Offset + Size, SampleSizes, Start+1, End)].
  


chunk_offset({ChunkOffsets, 4}, ChunkId) ->
  Skip = ChunkId*4,
  <<_:Skip/binary, Offset:32, _/binary>> = ChunkOffsets,
  Offset;

chunk_offset({ChunkOffsets, 8}, ChunkId) ->
  Skip = ChunkId*8,
  <<_:Skip/binary, Offset:64, _/binary>> = ChunkOffsets,
  Offset.


%%
%% Tests
%%

% fill_track_test() ->
%   ?assertEqual(<<1:1, 300:63, 0:64, 0.0:64/float, 0.0:64/float, 0:1, 10:63, 300:64, 25.0:64/float, 25.0:64/float>>,
%   fill_track(<<>>, [300, 10], [0,300], [true,false], [0.0,25.0], [0.0,0.0],1000, 0)).

% prepare_index_tracks_test() ->
%   ?assertEqual([[{{0,1},0},{{25,1},1},{{50,1},2}], [{{0,2},0},{{30,2},1},{{45,2},2}]], prepare_tracks_for_index(test_tracks())).
% 
% test_tracks() ->
%   [#mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 25}, #mp4_frame{dts = 50}]}, 
%    #mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 30}, #mp4_frame{dts = 45}]}].
%   
% build_index_test() ->
%   ?assertEqual(<<1, 0:24, 2, 0:24, 1, 1:24, 2, 1:24, 2, 2:24, 1, 2:24>>, build_index(test_tracks())).

mp4_desc_tag_with_length_test() ->
  ?assertEqual({es_descr, <<0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>, <<>>}, mp4_read_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)),
  ?assertEqual({decoder_config, <<64,21,0,0,0,0,0,100,239,0,0,0,0>>, <<6,1,2>>}, mp4_read_tag(<<4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).
  

% unpack_chunk_samples_test() ->
%   Mp4Track = #mp4_track{chunk_offsets = [1,2,3,4,5,6], chunk_sizes = [{0, 4}, {3, 5}]},
%   Mp4Track1 = unpack_samples_in_chunk(Mp4Track),
%   Mp4Track2 = Mp4Track1#mp4_track{chunk_sizes = [4,4,4,5,5,5]},
%   ?assertEqual(Mp4Track2, Mp4Track1).

  

esds_tag1_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 0, max_bitrate = 25839, avg_bitrate = 0}, config_from_esds_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).

esds_tag2_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 428, max_bitrate = 139608, avg_bitrate = 101944, specific = <<18,16>>}, config_from_esds_tag(<<3,25,0,0,0,4,17,64,21,0,1,172,0,2,33,88,0,1,142,56,5,2,18,16,6,1,2>>)).

% get_coverart_validMeta_test () ->
%   {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
%   Reader = {file,Dev},
%   Metadata = get_coverart(Reader),
%   ?assertMatch(<<_:6/binary,"JFIF",_/binary>>, Metadata).
% 
% get_coverart_sizeMeta_test () ->
%   {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
%   Reader = {file,Dev},
%   Metadata = get_coverart(Reader),
%   ?assertEqual(114121,size(Metadata)).
% 
% get_coverart_unvalid_test () ->
%   {ok,Dev} = file:open("test/files/without_coverart.mp4",[read,raw,binary]),
%   Reader = {file,Dev},
%   Metadata = get_coverart(Reader),
%   ?assertEqual(<<>>,Metadata).


