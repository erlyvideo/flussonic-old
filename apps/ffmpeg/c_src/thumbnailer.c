// @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
// @copyright  2010-2012 Max Lapshin
// @doc        multibitrate packetizer
// @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
// @end
//
//
//---------------------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/avcodec.h>
#include <libavfilter/avfilter.h>
#include <libavfilter/avfiltergraph.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
// #include <libavresample/avresample.h>
#include <assert.h>

#include "reader.h"
#include "compat.h"
#include <ei.h>

void reply_atom(char *a);
void reply_avframe(AVPacket *pkt, AVCodec *codec);
void error(const char *fmt, ...);
ssize_t read1(int fd, void *buf, ssize_t len);

extern int in_fd;
extern int out_fd;

int main(int argc, char *argv[]) {
  av_register_all();
  avcodec_register_all();
  avformat_network_init();
  avfilter_register_all();
  av_log_set_level(AV_LOG_ERROR);

  uint32_t buf_size = 10240;
  char *buf = (char *)av_malloc(buf_size);

  AVCodec *vdec;
  AVCodecContext *vdec_ctx;

  AVCodec *venc = NULL;
  AVCodecContext *venc_ctx;
  uint8_t config[1024];

  while(1) {
    uint32_t len;
    int read_bytes = 0;
    int idx = 0;
    if((read_bytes = read1(in_fd, &len, 4)) != 4) {
      if(read_bytes == 0) {
        _exit(0);
      }
      error("Can't read input length: %d", read_bytes);
    }

    len = ntohl(len);
    if(len > buf_size) {
      buf_size = len;
      buf = (char *)av_realloc(buf, buf_size);
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

    if(!strcmp(command, "config")) {
      ei_get_type(buf, &idx, &t, &size);
      if(t != ERL_BINARY_EXT) error("must pass binary as a config");
      if(size > 1000) error("too big config");


      vdec = avcodec_find_decoder_by_name("h264");
      if(!vdec) error("failed to find h264 decoder");
      vdec_ctx = avcodec_alloc_context3(vdec);
      if(!vdec_ctx) error("failed to alloc h264 decoder");

      ei_decode_binary(buf,&idx,config,&size);
      vdec_ctx->extradata = config;
      vdec_ctx->extradata_size = size;
      if(avcodec_open2(vdec_ctx, vdec, NULL) < 0) error("failed to open h264 decoder");

      reply_atom("ok");
      continue;
    }

    if(!strcmp(command, "jpeg")) {
      long n;
      if(ei_decode_long(buf,&idx, &n) < 0) error("must pass number of frame");
      ei_get_type(buf, &idx, &t, &size);
      if(t != ERL_BINARY_EXT) error("must pass binary as a frame");

      AVPacket packet;
      av_new_packet(&packet, size);
      ei_decode_binary(buf,&idx,packet.data,&size);

      AVFrame *decoded_frame = avcodec_alloc_frame();
      int could_decode = 0;
      int ret = avcodec_decode_video2(vdec_ctx, decoded_frame, &could_decode, &packet);

      if(!venc) {
        venc = avcodec_find_encoder_by_name("mjpeg");
        if(!venc) error("failed to find jpeg encoder");
        venc_ctx = avcodec_alloc_context3(venc);
        if(!venc_ctx) error("failed to alloc jpeg encoder");

        venc_ctx->width = decoded_frame->width;
        venc_ctx->height = decoded_frame->height;
        venc_ctx->pix_fmt = AV_PIX_FMT_YUVJ420P;
        venc_ctx->time_base.num = 1;
        venc_ctx->time_base.den = 50;

        if(avcodec_open2(venc_ctx, venc, NULL) < 0) error("failed to open jpeg encoder");        
      }


      AVPacket pkt;
      av_init_packet(&pkt);
      pkt.data = NULL;
      pkt.size = 0;
      int could_encode = 0;
      if(avcodec_encode_video2(venc_ctx, &pkt, decoded_frame, &could_encode) != 0) 
        error("Failed to encode jpeg");
      if(!could_encode) error("failed to make jpeg");

      ei_x_buff x;
      ei_x_new_with_version(&x);
      ei_x_encode_tuple_header(&x, 3);
      ei_x_encode_atom(&x, "jpeg");
      ei_x_encode_long(&x, n);
      ei_x_encode_binary(&x, pkt.data, pkt.size);
      write_x(&x);
      ei_x_free(&x);
      continue;
    }
    error("Unknown command: %s", command);
  }
}

