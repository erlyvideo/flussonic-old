// @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
// @copyright  2010-2012 Max Lapshin
// @doc        multibitrate packetizer
// @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
// @end
//
//
// This file is part of erlyvideo.
// 
// erlyvideo is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// erlyvideo is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
//
//---------------------------------------------------------------------------------------

#include <ei.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include "reader.h"


typedef struct Track {
  AVCodec *codec;
  AVCodecContext *ctx;
  int track_id;
  enum AVMediaType content;
  enum AVCodecID codec_id;
  int width;
  int height;
} Track;

#define MAX_OUTPUT_TRACKS 4


Track input_audio;
Track input_video;
Track output_audio[MAX_OUTPUT_TRACKS];
int out_audio_count = 0;
Track output_video[MAX_OUTPUT_TRACKS];
int out_video_count = 0;

extern int out_fd;
extern int in_fd;

void loop();
void pong(void);
void debug_loop(int argc, char *argv[], void (*loop)(void));
void reply_atom(char *a);
void reply_avframe(AVPacket *pkt, AVCodec *codec);
void error(const char *fmt, ...);
ssize_t read1(int fd, void *buf, ssize_t len);


int main(int argc, char *argv[]) {
  avcodec_register_all();
  av_log_set_level(AV_LOG_ERROR);

  bzero(&input_audio, sizeof(Track));
  bzero(&input_video, sizeof(Track));
  bzero(&output_audio, sizeof(output_audio));
  bzero(&output_video, sizeof(output_video));

  argc--;
  argv++;
  if(argc >= 2  && !strcmp(argv[0], "-d")) {
    debug_loop(argc, argv, loop);
  } else {
    loop();
  }
}


void loop() {
  int64_t dts_shift = AV_NOPTS_VALUE;

  uint32_t buf_size = 10240;
  char *buf = (char *)malloc(buf_size);
  while(1) {
    uint32_t len;
    int idx = 0;
    int read_bytes = 0;
    if((read_bytes = read1(in_fd, &len, 4)) != 4) {
      if(read_bytes == 0) {
        _exit(0);
      }
      error("Can't read input length: %d", read_bytes);
    }
    len = ntohl(len);
    if(len > buf_size) {
      buf_size = len;
      free(buf);
      buf = (char *)malloc(buf_size);
    }

    if((read_bytes = read1(in_fd, buf, len)) != len) error("Can't read %d bytes from input: %d", len, read_bytes);
    int version = 0;
    ei_decode_version(buf, &idx, &version);
    int command_idx = idx;

    int arity = 0;
    if(ei_decode_tuple_header(buf, &idx, &arity) == -1) error("must pass tuple");


    int t = 0;
    int size = 0;
    ei_get_type(buf, &idx, &t, &size);
    if(t != ERL_ATOM_EXT) error("first element must be atom");
    char command[MAXATOMLEN+1];
    ei_decode_atom(buf, &idx, command); arity--;


    if(!strcmp(command, "ping")) {
      pong();
      continue;
    }
    if(!strcmp(command, "exit")) {
      return;
    }
    if(!strcmp(command, "init_input")) {
      if(arity != 3) error("Must provide 3 arguments to init_input command");
      char content[1024];
      char codec[1024];
      if(ei_decode_atom(buf, &idx, content) == -1) error("Must provide content as an atom");
      if(ei_decode_atom(buf, &idx, codec) == -1) error("Must provide codec as an atom");

      int decoder_config_len = 0;
      ei_get_type(buf, &idx, &t, &decoder_config_len);
      if(t != ERL_BINARY_EXT) error("decoder config must be a binary");
      uint8_t *decoder_config = av_mallocz(decoder_config_len + FF_INPUT_BUFFER_PADDING_SIZE);
      long bin_len = 0;
      ei_decode_binary(buf, &idx, decoder_config, &bin_len);

      Track *t = NULL;
      if(!strcmp(content, "video")) {
        t = &input_video;
      } else if(!strcmp(content, "audio")) {
        t = &input_audio;
      } else {
        error("Unknown media content: '%s'", content);
      }
      if(t->codec) error("Double initialization of media '%s'", content);

      t->codec = avcodec_find_decoder_by_name(codec);
      t->ctx = avcodec_alloc_context3(t->codec);
      if(!t->codec || !t->ctx) 
        error("Unknown %s decoder '%s'", content, codec);
      t->ctx->time_base = (AVRational){1, 90};
      t->ctx->extradata_size = decoder_config_len;
      t->ctx->extradata = decoder_config;
      if(avcodec_open2(t->ctx, t->codec, NULL) < 0) 
        error("failed to allocate %s decoder", content);

      reply_atom("ready");
      continue;
    }

    if(!strcmp(command, "init_output")) {
      if(arity != 4) error("Must provide 4 arguments to init_output command");
      char content[1024];
      char codec[1024];
      if(ei_decode_atom(buf, &idx, content) == -1) error("Must provide content as an atom");
      if(ei_decode_atom(buf, &idx, codec) == -1) error("Must provide codec as an atom");

      long track_id = -1;
      if(ei_decode_long(buf, &idx, &track_id) == -1) error("track_id must be integer");
      if(track_id < 1 || track_id > MAX_OUTPUT_TRACKS+1) error("track_id must be from 1 to %d", MAX_OUTPUT_TRACKS+1);
      track_id--;

      Track *t = NULL;
      if(!strcmp(content, "audio")) {
        t = &output_audio[out_audio_count++];
      } else if(!strcmp(content, "video")) {
        t = &output_video[out_video_count++];
      } else {
        error("invalid_content '%s'", content);
      }
      t->track_id = track_id;

      t->codec = avcodec_find_encoder_by_name(codec);
      t->ctx = avcodec_alloc_context3(t->codec);
      if(!t->codec || !t->ctx) error("Unknown encoder '%s'", codec);

      AVCodecContext* ctx = t->ctx;
      AVDictionary *opts = NULL;


      int options_count = 0;
      if(ei_decode_list_header(buf, &idx, &options_count) < 0) error("options must be a proplist");
      while(options_count > 0) {
        int arity1 = 0;

        int t,s;
        ei_get_type(buf, &idx, &t, &s);
        if(t == ERL_NIL_EXT) {
          ei_skip_term(buf, &idx);
          break;
        }

        if(ei_decode_tuple_header(buf, &idx, &arity1) < 0) error("options must be a proper proplist");
        if(arity1 != 2) error("tuples in options proplist must be arity 2");

        char key[MAXATOMLEN];
        if(ei_decode_atom(buf, &idx, key) == 0) {

          if(!strcmp(key, "width")) {
            long w = 0;
            if(ei_decode_long(buf, &idx, &w) < 0) error("width must be integer");
            ctx->width = w;
            continue;
          }

          if(!strcmp(key, "height")) {
            long h = 0;
            if(ei_decode_long(buf, &idx, &h) < 0) error("height must be integer");
            ctx->height = h;
            continue;
          }

          if(!strcmp(key, "bitrate")) {
            long b = 0;
            if(ei_decode_long(buf, &idx, &b) < 0) error("bitrate must be integer");
            ctx->bit_rate = b;
            continue;
          }

          if(!strcmp(key, "sample_rate")) {
            long sr = 0;
            if(ei_decode_long(buf, &idx, &sr) < 0) error("sample_rate must be integer");
            ctx->sample_rate = sr;
            continue;
          }

          if(!strcmp(key, "channels")) {
            long ch = 0;
            if(ei_decode_long(buf, &idx, &ch) < 0) error("channels must be integer");
            ctx->channels = ch;
            continue;
          }

          fprintf(stderr, "Unknown key: '%s'\r\n", key);
          ei_skip_term(buf, &idx);
          continue;
        } else if(ei_decode_string(buf, &idx, key) == 0) {
          char value[MAXATOMLEN];
          if(ei_decode_string(buf, &idx, value) < 0) error("key-value must be strings");
          av_dict_set(&opts, key, value, 0);
        } else {
          error("Invalid options proplist");
        }
      }

      if(!strcmp(content, "video")) {
        ctx->pix_fmt = AV_PIX_FMT_YUV420P;
      }
      if(!strcmp(content, "audio")) {
        ctx->sample_fmt = AV_SAMPLE_FMT_S16;
        ctx->profile = FF_PROFILE_AAC_MAIN;
      }
      ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;
      ctx->time_base = (AVRational){1,90};

      if(avcodec_open2(ctx, t->codec, &opts) < 0) error("failed to allocate video encoder");

      AVPacket config;
      config.dts = config.pts = 0;
      config.flags = CODEC_FLAG_GLOBAL_HEADER;
      config.data = ctx->extradata;
      config.size = ctx->extradata_size;
      reply_avframe(&config, t->codec);      
      continue;
    }

    if(!strcmp(command, "video_frame")) {
      idx = command_idx;
      struct video_frame *fr = read_video_frame(buf, &idx);

      AVPacket packet;
      av_new_packet(&packet, fr->body.size);
      memcpy(packet.data, fr->body.data, fr->body.size);
      packet.size = fr->body.size;
      packet.dts = fr->dts*90;
      packet.pts = fr->pts*90;
      packet.stream_index = fr->track_id;

      // if(packet_size != pkt_size) error("internal error in reading frame body");

      if(fr->content == frame_content_audio) {
        if(!input_audio.ctx) error("input audio uninitialized");

        AVFrame *decoded_frame = avcodec_alloc_frame();
        int got_output = 0;
        int ret = avcodec_decode_audio4(input_audio.ctx, decoded_frame, &got_output, &packet);
        if(got_output) {
          reply_atom("ok");
        } else {
          error("Got: %d, %d\r\n", ret, got_output);
        }
        free(fr);
        continue;
      }

      if(fr->content == frame_content_video) {
        if(!input_video.ctx) error("input video uninitialized");
        AVFrame *decoded_frame = avcodec_alloc_frame();
        int could_decode = 0;
        int ret = avcodec_decode_video2(input_video.ctx, decoded_frame, &could_decode, &packet);
        if(ret < 0) {
          error("failed to decode video");
        }
        if(could_decode) {
          decoded_frame->pts = av_frame_get_best_effort_timestamp(decoded_frame);
          int sent_config = 0;

          AVPacket pkt;
          av_init_packet(&pkt);
          pkt.data = NULL;
          pkt.size = 0;

          int could_encode = 0;

          if(out_video_count <= 0) error("trying to transcode uninitialized video");
          if(avcodec_encode_video2(output_video[0].ctx, &pkt, decoded_frame, &could_encode) != 0) 
            error("Failed to encode h264");

          if(could_encode) {
            if(dts_shift == AV_NOPTS_VALUE) {
              dts_shift = -pkt.dts;
            }
            pkt.dts += dts_shift;
            reply_avframe(&pkt, output_video[0].codec);
          } else if(!sent_config) {
            reply_atom("ok");
          }
          free(fr);
          continue;
        } else {
          reply_atom("ok");
          free(fr);
          continue;
        }
      }

      error("Unknown content");
    }

    // AVCodecContext
    // AVPacket
    // AVFrame



    char *s = (char *)malloc(1024);
    ei_s_print_term(&s, buf, &command_idx);
    error("Unknown command: %s", s);
  }
}


void pong(void) {
  reply_atom("pong");
}

