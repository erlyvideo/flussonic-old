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
#include <stdarg.h>
#include <string.h>

#include <libavcodec/avcodec.h>



typedef struct Track {
  AVCodec *codec;
  AVCodecContext *ctx;
  int track_id;
  enum AVMediaType content;
  enum AVCodecID codec_id;
  int width;
  int height;
} Track;

#define MAX_INPUT_TRACKS 2
#define MAX_OUTPUT_TRACKS MAX_INPUT_TRACKS*4

Track input_tracks[MAX_INPUT_TRACKS];
Track output_tracks[MAX_OUTPUT_TRACKS];

int in_fd = 0;
int out_fd = 1;

void loop();
void pong(void);
void debug_loop(int argc, char *argv[], void (*loop)(void));



int main(int argc, char *argv[]) {
  avcodec_register_all();
  av_log_set_level(AV_LOG_ERROR);

  argc--;
  argv++;
  if(argc >= 2  && !strcmp(argv[0], "-d")) {
    debug_loop(argc, argv, loop);
  } else {
    loop();
  }
}

void write_x(ei_x_buff *x) {
  uint32_t len = htonl(x->buffsz);
  write(out_fd, &len, 4);
  write(out_fd, x->buff, x->buffsz);
}

void reply_atom(char *a) {
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_atom(&x, a);
  write_x(&x);
  ei_x_free(&x);
}


void reply_avframe(AVPacket *pkt, enum AVCodecID codec) {
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 11);
  ei_x_encode_atom(&x, "video_frame");
  if(codec == AV_CODEC_ID_H264) {
    ei_x_encode_atom(&x, "video");
  } else {
    ei_x_encode_atom(&x, "audio");
  }
  double dts = codec == AV_CODEC_ID_H264 ? pkt->dts / 90.0 : pkt->dts;
  double pts = codec == AV_CODEC_ID_H264 ? pkt->pts / 90.0 : pkt->pts;
  ei_x_encode_double(&x, dts);
  ei_x_encode_double(&x, pts);
  ei_x_encode_long(&x, 0);
  if(codec == AV_CODEC_ID_H264) {
    ei_x_encode_atom(&x, "h264");
  } else {
    ei_x_encode_atom(&x, "aac");
  }
  if(pkt->flags & CODEC_FLAG_GLOBAL_HEADER) {
    ei_x_encode_atom(&x, "config");    
  } else if(pkt->flags & AV_PKT_FLAG_KEY) {
    ei_x_encode_atom(&x, "keyframe");
  } else {
    ei_x_encode_atom(&x, "frame");
  }
  ei_x_encode_atom(&x, "undefined");
  if(codec == AV_CODEC_ID_H264) {
    ei_x_encode_long(&x, 1);
  } else {
    ei_x_encode_long(&x, 2);
  }
  ei_x_encode_binary(&x, pkt->data, pkt->size);
  ei_x_encode_atom(&x, "undefined");
  write_x(&x);
  ei_x_free(&x);
}

void error(const char *fmt, ...) {
  va_list ap;
  if(in_fd != 0) {
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
  }

  char *msg = NULL;
  va_start(ap, fmt);
  vasprintf(&msg, fmt, ap);
  va_end(ap);


  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_format_wo_ver(&x, "{~a,~s}","error", msg);
  write_x(&x);
  ei_x_free(&x);
  _exit(15);
}


ssize_t read1(int fd, void *buf, ssize_t len) {
  ssize_t count = 0;
  ssize_t bytes = 0;
  while(len > 0 && (bytes = read(fd, buf, len)) > 0) {
    buf += bytes;
    len -= bytes;
    count += bytes;
  }
  if(bytes == 0 && len > 0) return 0;
  if(bytes == -1 && len > 0) return -1;
  return count;
}

void loop() {
  AVCodec *vencoder;
  AVCodecContext *v_enc_ctx = NULL;
  AVCodec *aencoder;
  AVCodecContext *a_enc_ctx = NULL;
  int64_t dts_shift = AV_NOPTS_VALUE;

  uint32_t buf_size = 10240;
  char *buf = (char *)malloc(buf_size);
  while(1) {
    uint32_t len;
    int idx = 0;
    int read_bytes = 0;
    if((read_bytes = read1(in_fd, &len, 4)) != 4) {
      if(read_bytes == 0) {
        fprintf(stderr, "Input eof\r\n");
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
      fprintf(stderr, "ping-pong\r\n");
      pong();
      continue;
    }
    if(!strcmp(command, "exit")) {
      fprintf(stderr, "exit command received\r\n");
      fflush(stderr);
      return;
    }
    if(!strcmp(command, "init_input")) {
      if(arity != 4) error("Must provide 4 arguments to init_input command");
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

      long track_id = -1;
      if(ei_decode_long(buf, &idx, &track_id) == -1) error("track_id must be integer");
      if(track_id != 1 && track_id != 2) error("track_id must be 1 or 2");
      track_id--;

      input_tracks[track_id].codec = avcodec_find_decoder_by_name(codec);
      input_tracks[track_id].ctx = avcodec_alloc_context3(input_tracks[track_id].codec);
      if(!input_tracks[track_id].codec || !input_tracks[track_id].ctx) 
        error("Unknown %s decoder '%s'", content, codec);
      input_tracks[track_id].ctx->time_base = (AVRational){1, 90};
      input_tracks[track_id].ctx->extradata_size = decoder_config_len;
      input_tracks[track_id].ctx->extradata = decoder_config;
      if(avcodec_open2(input_tracks[track_id].ctx, input_tracks[track_id].codec, NULL) < 0) 
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

      ei_skip_term(buf, &idx);
      long track_id = -1;
      if(ei_decode_long(buf, &idx, &track_id) == -1) error("track_id must be integer");
      if(track_id < 1 || track_id > MAX_OUTPUT_TRACKS+1) error("track_id must be 1 or 2");
      track_id--;
      


      reply_atom("not_ready_yet");
      continue;
    }

    if(!strcmp(command, "video_frame")) {
      if(arity != 10) error("#video_frame{} must have 10 fields");
      char content[1024];
      long l;
      if(ei_decode_atom(buf, &idx, content) == -1) error("content must be atom");          // content
      double pts, dts;
      if(ei_decode_double(buf, &idx, &dts) == -1) {
        if(ei_decode_long(buf, &idx, &l) == -1) error("dts must be int or float"); dts = l; // dts
      }
      if(ei_decode_double(buf, &idx, &pts) == -1) {
        if(ei_decode_long(buf, &idx, &l) == -1) error("pts must be int or float"); pts = l; // pts
      }
      ei_skip_term(buf, &idx); // stream_id
      ei_skip_term(buf, &idx); // codec
      char flavor[1024];
      if(ei_decode_atom(buf, &idx, flavor) == -1) error("flavor must be atom"); // flavor
      if(strcmp(flavor, "frame") && strcmp(flavor, "keyframe")) 
        error("flavor must be keyframe or frame,but it is '%s'", flavor);

      ei_skip_term(buf, &idx); // sound
      long track_id = 0;
      if(ei_decode_long(buf, &idx, &track_id) == -1) error("track_id must be integer"); // track_id
      track_id--;

      int pkt_size = 0;
      ei_get_type(buf, &idx, &t, &pkt_size); // binary
      if(t != ERL_BINARY_EXT) error("#video_frame.body must be binary and it is %c", t);

      AVPacket packet;
      av_new_packet(&packet, pkt_size);
      long packet_size = 0;
      ei_decode_binary(buf, &idx, packet.data, &packet_size);
      packet.size = packet_size;
      packet.dts = dts*90;
      packet.pts = pts*90;
      packet.stream_index = track_id;

      if(packet_size != pkt_size) error("internal error in reading frame body");

      if(!strcmp(content, "audio")) {
        if(!aencoder) {
          aencoder = avcodec_find_encoder_by_name("libfaac");
          a_enc_ctx = avcodec_alloc_context3(aencoder);
          if(!aencoder || !a_enc_ctx) error("Unknown encoder 'libfaac'");
          if(avcodec_open2(a_enc_ctx, aencoder, NULL) < 0) error("failed to allocate audio encoder");
        }

        AVFrame *decoded_frame = avcodec_alloc_frame();
        int got_output = 0;
        int ret = avcodec_decode_audio4(input_tracks[track_id].ctx, decoded_frame, &got_output, &packet);
        // fprintf(stderr, "Got it!\r\n");
        if(got_output) {
          reply_atom("ok");
        } else {
          error("Got: %d, %d\r\n", ret, got_output);
        }
        continue;
      }

      if(!strcmp(content, "video")) {
        AVFrame *decoded_frame = avcodec_alloc_frame();
        int could_decode = 0;
        int ret = avcodec_decode_video2(input_tracks[track_id].ctx, decoded_frame, &could_decode, &packet);
        if(ret < 0) {
          error("failed to decode video");
        }
        if(could_decode) {
          decoded_frame->pts = av_frame_get_best_effort_timestamp(decoded_frame);
          int sent_config = 0;
          if(!v_enc_ctx) {
            vencoder = avcodec_find_encoder_by_name("libx264");
            v_enc_ctx = avcodec_alloc_context3(vencoder);
            if(!vencoder || !v_enc_ctx) error("Unknown encoder 'libx264'");

            v_enc_ctx = avcodec_alloc_context3(vencoder);

            v_enc_ctx->pix_fmt = AV_PIX_FMT_YUV420P;
            v_enc_ctx->width = decoded_frame->width;
            v_enc_ctx->height = decoded_frame->height;
            v_enc_ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;
            v_enc_ctx->time_base = (AVRational){1,90};
            // v_enc_ctx->time_base = {1, 1};
            AVDictionary *opts = NULL;
            av_dict_set(&opts, "preset", "fast", 0);
            av_dict_set(&opts, "qpmin", "23", 0);
            av_dict_set(&opts, "qpmax", "50", 0);

            if(avcodec_open2(v_enc_ctx, vencoder, &opts) < 0) error("failed to allocate video encoder");

            AVPacket config;
            config.dts = config.pts = 0;
            config.flags = CODEC_FLAG_GLOBAL_HEADER;
            config.data = v_enc_ctx->extradata;
            config.size = v_enc_ctx->extradata_size;
            sent_config = 1;
            reply_avframe(&config, AV_CODEC_ID_H264);
          }

          AVPacket pkt;
          av_init_packet(&pkt);
          pkt.data = NULL;
          pkt.size = 0;

          int could_encode = 0;

          if(avcodec_encode_video2(v_enc_ctx, &pkt, decoded_frame, &could_encode) != 0) error("Failed to encode h264");

          if(could_encode) {
            if(dts_shift == AV_NOPTS_VALUE) {
              dts_shift = -pkt.dts;
            }
            pkt.dts += dts_shift;
            reply_avframe(&pkt, AV_CODEC_ID_H264);
          } else if(!sent_config) {
            reply_atom("ok");
          }

          continue;
        } else {
          reply_atom("ok");
          continue;
        }
      }

      error("Unknown content: '%s'", content);
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

