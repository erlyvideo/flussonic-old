-ifndef(VIDEO_FRAME_TYPES).
-define(VIDEO_FRAME_TYPES, true).

-type(frame_content() ::audio|video|metadata).
-type(frame_flavor()  ::frame|keyframe|config|disposable).
-type(frame_codec()   ::h264|sorenson|vp6|vp6f|mpeg4|mpeg2|aac|mp3|pcma|pcmu|pcm|pcm_le|g726_16|speex|nellymoser|nellymoser8).

-type(frame_sound_channels() ::mono|stereo).
-type(frame_sound_size() ::bit8|bit16).
-type(frame_sound_rate() ::rate5|rate11|rate22|rate44).
% -type(frame_sound() ::{frame_sound_channels(), frame_sound_size(), frame_sound_rate()}).

-record(video_frame,{
	content        = undefined ::frame_content(),
	dts            = 0.0 ::float(),
	pts            = 0.0 ::float(),
	stream_id      = 0         ::non_neg_integer(),
	codec 	       ::frame_codec(),
	flavor         ::frame_flavor(),
	% options        = {undefined, undefined, undefined} ::frame_sound() | undefined,
	track_id       = 0 ::non_neg_integer(),
	body           = <<>>      ::binary(),
	next_id        = undefined ::any()
}).

-type(video_frame() :: #video_frame{}).
-endif.
