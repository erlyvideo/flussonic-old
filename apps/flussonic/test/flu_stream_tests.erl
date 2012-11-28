-module(flu_stream_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


rewrite_stream_manifest_test() ->
  Manifest1 = <<"<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">
<id>livestream</id>
<streamType>live</streamType>
<duration>0</duration>
<bootstrapInfo id=\"bootstrap0\" profile=\"named\" url=\"bootstrap\"></bootstrapInfo>
<media streamId=\"video_0\" url=\"hds/0/\" bootstrapInfoId=\"bootstrap0\" bitrate=\"\">
<metadata> AgAKb25NZXRhRGF0YQMADWF1ZGlvY2hhbm5lbHMAQAAAAAAAAAAADGF1ZGlvY29kZWNpZABAJAAAAAAAAAAPYXVkaW9zYW1wbGVyYXRlAEDncAAAAAAAAAhkdXJhdGlvbgAAAAAAAAAAAAAGaGVpZ2h0AEByAAAAAAAAAAx2aWRlb2NvZGVjaWQAQBwAAAAAAAAABXdpZHRoAECAAAAAAAAAAAAJ </metadata>
</media>
</manifest>">>,
  {ok, Manifest2} = flu_stream:rewrite_manifest(Manifest1, <<"token1">>),
  ?assertMatch({_,_}, binary:match(Manifest2, <<"url=\"bootstrap?token=token1\"">>)),
  ok.


no_rewrite_stream_manifest_test() ->
  Manifest1 = <<"<manifest xmlns=\"http://ns.adobe.com/f4m/1.0\">
<id>livestream</id>
<streamType>live</streamType>
<duration>0</duration>
<bootstrapInfo id=\"bootstrap0\" profile=\"named\">bootstrap</bootstrapInfo>
<media streamId=\"video_0\" url=\"hds/0/\" bootstrapInfoId=\"bootstrap0\" bitrate=\"\">
<metadata> AgAKb25NZXRhRGF0YQMADWF1ZGlvY2hhbm5lbHMAQAAAAAAAAAAADGF1ZGlvY29kZWNpZABAJAAAAAAAAAAPYXVkaW9zYW1wbGVyYXRlAEDncAAAAAAAAAhkdXJhdGlvbgAAAAAAAAAAAAAGaGVpZ2h0AEByAAAAAAAAAAx2aWRlb2NvZGVjaWQAQBwAAAAAAAAABXdpZHRoAECAAAAAAAAAAAAJ </metadata>
</media>
</manifest>">>,
  {ok, Manifest2} = flu_stream:rewrite_manifest(Manifest1, <<"token1">>),
  ?assertEqual(Manifest1,Manifest2),
  ok.
