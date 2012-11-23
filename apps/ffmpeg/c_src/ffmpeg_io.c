#include <ei.h>
#include <libavcodec/avcodec.h>
#include <stdarg.h>
#include <unistd.h>


int in_fd = 0;
int out_fd = 1;



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


void reply_avframe(AVPacket *pkt, AVCodec *codec) {
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 11);
  ei_x_encode_atom(&x, "video_frame");
  if(codec->type == AVMEDIA_TYPE_VIDEO) {
    ei_x_encode_atom(&x, "video");
  } else if(codec->type == AVMEDIA_TYPE_AUDIO) {
    ei_x_encode_atom(&x, "audio");
  }
  double dts = pkt->dts / 90.0;
  double pts = pkt->pts / 90.0;
  ei_x_encode_double(&x, dts);
  ei_x_encode_double(&x, pts);
  ei_x_encode_long(&x, 0);
  ei_x_encode_atom(&x, codec->name);

  if(pkt->flags & CODEC_FLAG_GLOBAL_HEADER) {
    ei_x_encode_atom(&x, "config");    
  } else if(pkt->flags & AV_PKT_FLAG_KEY) {
    ei_x_encode_atom(&x, "keyframe");
  } else {
    ei_x_encode_atom(&x, "frame");
  }
  ei_x_encode_atom(&x, "undefined");
  if(codec->type == AVMEDIA_TYPE_VIDEO) {
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
