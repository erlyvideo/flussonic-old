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
// #include <libavresample/avresample.h>
#include <assert.h>

#include "compat.h"

typedef struct Track {
  AVCodec *codec;
  AVCodecContext *ctx;
  int track_id;
  enum AVMediaType content;
  enum AVCodecID codec_id;
  int width;
  int height;
  AVRational time_base;
} Track;

void error(const char *fmt, ...);

int main(int argc, char *argv[]) {
  av_register_all();
  avcodec_register_all();
  avformat_network_init();
  av_log_set_level(AV_LOG_ERROR);

  if(argc < 2) {
    error("%s input-ts", argv[0]);
  }

  AVFormatContext* input_ctx = avformat_alloc_context();

  if(avformat_open_input(&input_ctx, argv[1],NULL,NULL) != 0){
    error("failed input");
  }

  if(avformat_find_stream_info(input_ctx, NULL) < 0) {
    error("no_stream_info");
  }


  int video_stream_index = -1;
  int vdts_step = -1;
  int audio_stream_index = -1;
  int adts_step = -1;

  int sample_rate;

  Track tracks[input_ctx->nb_streams];

  // if((video_stream_id = av_find_best_stream(input_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, NULL, 0)) < 0) {
  //   printf("no video input\n");
  //   exit(4);
  // } else {
  //   tracks[video_stream_id].codec = vdec;
  // }

  // if((audio_stream_id = av_find_best_stream(input_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, NULL, 0)) < 0) {
  //   printf("no audio input\n");
  //   exit(5);
  // }

  int i;
  for(i =0;i<input_ctx->nb_streams;i++){

    tracks[i].codec = avcodec_find_decoder(input_ctx->streams[i]->codec->codec_id);
    fprintf(stderr, "stream %d: %s (%d)\r\n", i, tracks[i].codec->name, input_ctx->streams[i]->codec->sample_rate);
    tracks[i].ctx = avcodec_alloc_context3(tracks[i].codec);
    if(avcodec_open2(tracks[i].ctx, tracks[i].codec, NULL) < 0) {
      error("Failed to open decoder %s", tracks[i].codec->name);
    }
    tracks[i].time_base = input_ctx->streams[i]->time_base;
    if(input_ctx->streams[i]->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
      video_stream_index = i;
      vdts_step = 3600;
    } else if(input_ctx->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
      audio_stream_index = i;
      sample_rate = input_ctx->streams[i]->codec->sample_rate;
      adts_step = 90000*1152 / sample_rate;
    }
      
  }


  AVCodec *venc = NULL, *aenc = NULL;
  AVCodecContext *venc_ctx = NULL, *aenc_ctx = NULL;
  // AVAudioResampleContext *avr = avresample_alloc_context();

  // av_opt_set_int(avr, "in_channel_layout",  AV_CH_LAYOUT_STEREO, 0);
  // av_opt_set_int(avr, "out_channel_layout", AV_CH_LAYOUT_STEREO, 0);
  // av_opt_set_int(avr, "in_sample_rate",     input_ctx->streams[audio_stream_index]->codec->sample_rate,                0);
  // av_opt_set_int(avr, "out_sample_rate",    input_ctx->streams[audio_stream_index]->codec->sample_rate,                0);
  // av_opt_set_int(avr, "in_sample_fmt",      AV_SAMPLE_FMT_S16,   0);
  // av_opt_set_int(avr, "out_sample_fmt",     AV_SAMPLE_FMT_S16,    0);


  // AVPacket out_pkt;
  // av_init_packet(&out_pkt);
  // out_pkt.data = NULL;
  // out_pkt.size = 0;
  int out_size = 1024*1024;
  uint8_t *out_buf = av_malloc(out_size);




  int64_t first_dts = -1;
  int64_t first_vdts = -1;
  uint64_t vcount = 0;
  int64_t first_adts = -1;
  uint64_t acount = 0;
  AVFrame *raw_video, *raw_audio;
  raw_video = avcodec_alloc_frame();
  raw_audio = avcodec_alloc_frame();

  while(1) {
    AVPacket packet;
    av_init_packet(&packet);
    packet.data = NULL;
    packet.size = 0;


    if(av_read_frame(input_ctx, &packet) < 0) 
      error("eof");

    if(first_dts == -1) first_dts = packet.dts;
    int decoded = 0;
    int encoded = 0;
    if(packet.stream_index == video_stream_index) {
      if(first_vdts == -1) first_vdts = (packet.dts / vdts_step)*vdts_step;
      while(packet.dts >= first_vdts + vcount*vdts_step + vdts_step) {
        printf("missed video frame %llu\n", (unsigned long long)vcount);
        vcount++;
      }
      avcodec_decode_video2(tracks[packet.stream_index].ctx, raw_video, &decoded, &packet);
      raw_video->pts = first_vdts + vcount*vdts_step;

      if(decoded && !venc) {
        venc = avcodec_find_encoder_by_name("libx264");
        if(!venc) error("failed to open video encoder");
        venc_ctx = avcodec_alloc_context3(venc);
        if(!venc_ctx) error("failed to allocate video encoding context");
        venc_ctx->width = raw_video->width;
        venc_ctx->height = raw_video->height;
        venc_ctx->bit_rate = 300000;
        venc_ctx->pix_fmt = AV_PIX_FMT_YUV420P;
        venc_ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;
        venc_ctx->time_base = (AVRational){1,90};
        venc_ctx->thread_count = 0;
        AVDictionary *opts = NULL;
        av_dict_set(&opts, "preset", "fast", 0);
        av_dict_set(&opts, "profile", "main", 0);
        av_dict_set(&opts, "qmin", "26", 0);
        av_dict_set(&opts, "qmax", "50", 0);

        if(avcodec_open2(venc_ctx, venc, &opts) < 0)
          error("failed to open video encoder");

        printf("Open libx264: %dx%d, %d bytes config, %d bps\r\n", venc_ctx->width, venc_ctx->height, venc_ctx->extradata_size, venc_ctx->bit_rate);
      }

      if(decoded) {
        encoded = avcodec_encode_video(venc_ctx, out_buf, out_size, raw_video);
        if(encoded < 0)
          error("Failed to encode h264");
        
        //if(encoded)
        //  printf("h264 dts: %10llu, bytes: %d\n", venc_ctx->coded_frame->pts, encoded);
      }

      vcount++;
    } else {
      if(first_adts == -1) first_adts = (packet.dts / adts_step)*adts_step;
      while(packet.dts >= first_adts + acount*adts_step + adts_step) {
        printf("missed audio frame %llu\n", (unsigned long long)acount);
        acount++;
      }
      avcodec_decode_audio4(tracks[packet.stream_index].ctx, raw_audio, &decoded, &packet);
      raw_audio->pts = first_adts + acount*adts_step;

      if(decoded && !aenc) {
        aenc = avcodec_find_decoder_by_name("aac");
        assert(aenc);
        aenc_ctx = avcodec_alloc_context3(aenc);
        assert(aenc_ctx);

        aenc_ctx->channels = input_ctx->streams[audio_stream_index]->codec->channels;
        aenc_ctx->sample_rate = input_ctx->streams[audio_stream_index]->codec->sample_rate;
        aenc_ctx->bit_rate = 64;

        if(avcodec_open2(aenc_ctx, aenc, NULL) < 0)
          error("failed to allocate audio encoder");
      }
 
      if(decoded) {
        // if(avcodec_encode_audio2(aenc_ctx, &out_pkt, raw_audio, &encoded) < 0)
        //   error("Failed to encode aac");

        // if(encoded)
        //   printf(" aac dts: %10llu, bytes: %d\n", out_pkt.dts, out_pkt.size);
      }

      acount++;
    }
    // if(!got_picture) {
    //   printf("%10s %8lld failed to decode frame\n", tracks[packet.stream_index].codec->name, (long long int)(packet.dts - first_dts));
    // } else {
    //   printf("%10s %8lld %d, %d\n", tracks[packet.stream_index].codec->name, (long long int)(packet.dts - first_dts), got_picture, decoded);
    // }
    av_free_packet(&packet);
    av_init_packet(&packet);
  }
}









void error(const char *fmt, ...) {
  va_list ap;
  // if(in_fd != 0) {
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
  // }

  // char *msg = NULL;
  // va_start(ap, fmt);
  // vasprintf(&msg, fmt, ap);
  // va_end(ap);


  // ei_x_buff x;
  // ei_x_new_with_version(&x);
  // ei_x_format_wo_ver(&x, "{~a,~s}","error", msg);
  // write_x(&x);
  // ei_x_free(&x);
  _exit(15);
}


