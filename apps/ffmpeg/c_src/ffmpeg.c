#include <ei.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/wait.h>

#include <libavcodec/avcodec.h>


int in_fd = 0;
int out_fd = 1;

void loop();
void pong(void);

uint16_t listen_port = 0;
int listen_sock() {
  int listen_fd = socket(PF_INET, SOCK_STREAM, 0);
  int true_ = 1;
  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &true_, sizeof(true_));
  struct sockaddr_in bind_addr;
  bzero(&bind_addr, sizeof(bind_addr));
  bind_addr.sin_family = AF_INET;
  bind_addr.sin_addr.s_addr = INADDR_ANY;
  bind_addr.sin_port = htons(listen_port);
  if(bind(listen_fd, (const struct sockaddr *)&bind_addr, sizeof(bind_addr)) == -1) {
    fprintf(stderr, "Can't bind to listen socket\r\n");
    exit(14);
  }
  listen(listen_fd, 10);
  return listen_fd;
}

int main(int argc, char *argv[]) {
  avcodec_register_all();
  av_log_set_level(AV_LOG_DEBUG);

  argc--;
  argv++;
  if(argc >= 2  && !strcmp(argv[0], "-d")) {
    listen_port = atoi(argv[1]);
    argc-=2;
    argv++; argv++;


    if(argc >= 1 && !strcmp(argv[0], "-f")) {
      argc--;
      argv++;
      fprintf(stderr, "Start fork-pool\r\n");

      while(1) {
        pid_t child;
        if((child = fork()) == 0) {
          int listen_fd = listen_sock();    
          struct sockaddr_in cli_addr;
          socklen_t cli_addr_len;

          int cli_sock = accept(listen_fd, (struct sockaddr *)&cli_addr, &cli_addr_len);
          in_fd = out_fd = cli_sock;
          // setsockopt(in_fd, SOL_SOCKET, SO_LINGER, &true_, sizeof(true_));
          loop();
          shutdown(cli_sock, SHUT_RDWR);
          _exit(0);
        } else {
          int stat_loc;
          struct rusage rusage;
          wait4(child, &stat_loc, 0, &rusage);
          if(WIFSIGNALED(stat_loc)) {
            fprintf(stderr, "Child process was signalled by %d\r\n", WTERMSIG(stat_loc));
          } else if(WIFEXITED(stat_loc)) {
            if(WEXITSTATUS(stat_loc) != 0)
              fprintf(stderr, "Child process exited with code %d\r\n", WEXITSTATUS(stat_loc));
          } else {
            fprintf(stderr, "Child process exited due to unknown reason\r\n");
          }
          fprintf(stderr, "\r\n");
        }
      }      
    } else {
      int listen_fd = listen_sock();    
      struct sockaddr_in cli_addr;
      socklen_t cli_addr_len;

      int cli_sock = accept(listen_fd, (struct sockaddr *)&cli_addr, &cli_addr_len);
      in_fd = out_fd = cli_sock;
      loop();
      _exit(0);
    }
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


void reply_avframe(AVFrame *frame) {

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
  AVCodec *vdecoder, *adecoder;
  AVCodecContext *v_dec_ctx, *a_dec_ctx;
  AVCodec *vencoder, *aencoder;
  AVCodecContext *v_env_ctx, *a_enc_ctx;

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
      if(arity != 4) error("Must provide 4 arguments to init command");
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

      AVCodecContext *dec_ctx = NULL;
      if(!strcmp(content, "audio")) {
        adecoder = avcodec_find_decoder_by_name(codec);
        dec_ctx = a_dec_ctx = avcodec_alloc_context3(adecoder);
        a_dec_ctx->extradata_size = decoder_config_len;
        a_dec_ctx->extradata = decoder_config;
        if(!adecoder || !a_dec_ctx) error("Unknown audio decoder '%s'", codec);
        if(avcodec_open2(a_dec_ctx, adecoder, NULL) < 0) error("failed to allocate audio decoder");
      }

      if(!strcmp(content, "video")) {
        vdecoder = avcodec_find_decoder_by_name(codec);
        dec_ctx = v_dec_ctx = avcodec_alloc_context3(vdecoder);
        // v_dec_ctx->flags2 |= CODEC_FLAG2_CHUNKS;

        v_dec_ctx->debug |= FF_DEBUG_STARTCODE;
        v_dec_ctx->extradata_size = decoder_config_len;
        v_dec_ctx->extradata = decoder_config;

        if(!vdecoder || !v_dec_ctx) error("Unknown video decoder '%s'", codec);
        if(avcodec_open2(v_dec_ctx, vdecoder, NULL) < 0) error("failed to allocate video decoder");
      }
      if(!dec_ctx) error("content must be video or audio");



      vencoder = avcodec_find_encoder_by_name("libx264");
      v_env_ctx = avcodec_alloc_context3(vencoder);
      if(!vencoder || !v_env_ctx) error("Unknown encoder 'libx264'");
      aencoder = avcodec_find_encoder_by_name("aac");
      a_enc_ctx = avcodec_alloc_context3(aencoder);
      if(!aencoder || !a_enc_ctx) error("Unknown encoder 'aac'");
      reply_atom("ready");
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

      int pkt_size = 0;
      ei_get_type(buf, &idx, &t, &pkt_size); // binary
      if(t != ERL_BINARY_EXT) error("#video_frame.body must be binary and it is %c", t);

      AVPacket packet;
      av_new_packet(&packet, pkt_size);
      long packet_size = 0;
      ei_decode_binary(buf, &idx, packet.data, &packet_size);
      packet.size = packet_size;
      packet.dts = dts;
      packet.pts = pts;
      packet.stream_index = track_id;

      if(packet_size != pkt_size) error("internal error in reading frame body");

      fprintf(stderr, "Going to decode %d bytes\r\n", packet.size);
      if(!strcmp(content, "audio")) {
        AVFrame *decoded_frame = avcodec_alloc_frame();
        int got_output = 0;
        int ret = avcodec_decode_audio4(a_dec_ctx, decoded_frame, &got_output, &packet);
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
        int got_output = 0;
        int ret = avcodec_decode_video2(v_dec_ctx, decoded_frame, &got_output, &packet);
        if(ret >= 0) {
          if(got_output) {
            // reply_avframe(decoded_frame);
            reply_atom("ok");
          } else {
            reply_atom("ok");
          }
          continue;
        }
        error("Got it: %d, %d\r\n", ret, got_output);
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

