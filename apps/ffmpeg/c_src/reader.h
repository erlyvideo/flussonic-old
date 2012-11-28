#include <ei.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

struct Binary{uint8_t *data; ssize_t size;};
enum frame_content {frame_content_audio = 1, frame_content_video = 2, frame_content_metadata = 3};
enum frame_content read_frame_content(char *buf, int *idx);
int write_frame_content(ei_x_buff *x, enum frame_content t);

enum frame_flavor {frame_flavor_frame = 1, frame_flavor_keyframe = 2, frame_flavor_config = 3, frame_flavor_disposable = 4};
enum frame_flavor read_frame_flavor(char *buf, int *idx);
int write_frame_flavor(ei_x_buff *x, enum frame_flavor t);

enum frame_codec {frame_codec_h264 = 1, frame_codec_sorenson = 2, frame_codec_vp6 = 3, frame_codec_vp6f = 4, frame_codec_mpeg4 = 5, frame_codec_mpeg2 = 6, frame_codec_aac = 7, frame_codec_mp3 = 8, frame_codec_pcma = 9, frame_codec_pcmu = 10, frame_codec_pcm = 11, frame_codec_pcm_le = 12, frame_codec_g726_16 = 13, frame_codec_speex = 14, frame_codec_nellymoser = 15, frame_codec_nellymoser8 = 16};
enum frame_codec read_frame_codec(char *buf, int *idx);
int write_frame_codec(ei_x_buff *x, enum frame_codec t);

enum frame_sound_channels {frame_sound_channels_mono = 1, frame_sound_channels_stereo = 2};
enum frame_sound_channels read_frame_sound_channels(char *buf, int *idx);
int write_frame_sound_channels(ei_x_buff *x, enum frame_sound_channels t);

enum frame_sound_size {frame_sound_size_bit8 = 1, frame_sound_size_bit16 = 2};
enum frame_sound_size read_frame_sound_size(char *buf, int *idx);
int write_frame_sound_size(ei_x_buff *x, enum frame_sound_size t);

enum frame_sound_rate {frame_sound_rate_rate5 = 1, frame_sound_rate_rate11 = 2, frame_sound_rate_rate22 = 3, frame_sound_rate_rate44 = 4};
enum frame_sound_rate read_frame_sound_rate(char *buf, int *idx);
int write_frame_sound_rate(ei_x_buff *x, enum frame_sound_rate t);

struct video_frame{
  enum frame_content content;
  double dts;
  double pts;
  long stream_id;
  enum frame_codec codec;
  enum frame_flavor flavor;
  long track_id;
  struct Binary body;
};
struct video_frame* read_video_frame(char *buf, int *idx);
int write_video_frame(ei_x_buff* x, struct video_frame r);

struct video_params{
  long width;
  long height;
  long fps;
  long length_size;
};
struct video_params* read_video_params(char *buf, int *idx);
int write_video_params(ei_x_buff* x, struct video_params r);

struct audio_params{
  long channels;
  long sample_rate;
  long samples;
};
struct audio_params* read_audio_params(char *buf, int *idx);
int write_audio_params(ei_x_buff* x, struct audio_params r);

struct stream_info{
  enum frame_content content;
  long track_id;
  enum frame_codec codec;
  struct Binary config;
  long bitrate;
  struct Binary language;
  long timescale;
};
struct stream_info* read_stream_info(char *buf, int *idx);
int write_stream_info(ei_x_buff* x, struct stream_info r);

enum flow_type {flow_type_file = 1, flow_type_stream = 2};
enum flow_type read_flow_type(char *buf, int *idx);
int write_flow_type(ei_x_buff *x, enum flow_type t);

struct media_info{
  enum flow_type flow_type;
  long duration;
};
struct media_info* read_media_info(char *buf, int *idx);
int write_media_info(ei_x_buff* x, struct media_info r);

struct init_output{
  enum frame_content content;
  enum frame_codec codec;
  long track_id;
};
struct init_output* read_init_output(char *buf, int *idx);
int write_init_output(ei_x_buff* x, struct init_output r);

struct init_input{
  enum frame_content content;
  enum frame_codec codec;
  struct Binary config;
};
struct init_input* read_init_input(char *buf, int *idx);
int write_init_input(ei_x_buff* x, struct init_input r);

