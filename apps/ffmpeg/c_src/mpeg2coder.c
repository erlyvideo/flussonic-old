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


int main(int argc, char *argv[]) {
  av_register_all();
  avcodec_register_all();
  avformat_network_init();
  av_log_set_level(AV_LOG_ERROR);

  if(argc < 2) {
    printf("%s input-ts\n", argv[0]);
    exit(1);
  }

  AVFormatContext* input_ctx = avformat_alloc_context();

  if(avformat_open_input(&input_ctx, argv[1],NULL,NULL) != 0){
    printf("failed input\n");
    exit(2);
  }

  if(avformat_find_stream_info(input_ctx, NULL) < 0) {
    printf("no_stream_info\n");
    exit(3);
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
      fprintf(stderr, "Failed to open decoder %s", tracks[i].codec->name);
      exit(2);
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

  AVPacket packet;
  av_init_packet(&packet);

  int64_t first_dts = -1;
  int64_t first_vdts = -1;
  uint64_t vcount = 0;
  int64_t first_adts = -1;
  uint64_t acount = 0;
  AVFrame *raw_video, *raw_audio;
  raw_video = avcodec_alloc_frame();
  raw_audio = avcodec_alloc_frame();

  while(av_read_frame(input_ctx, &packet) >= 0) {
    if(first_dts == -1) first_dts = packet.dts;
    int decoded = 0;
    int got_picture = 0;
    if(packet.stream_index == video_stream_index) {
      if(first_vdts == -1) first_vdts = packet.dts;
      while(packet.dts >= first_vdts + vcount*vdts_step + vdts_step) {
        printf("missed video frame %llu", (unsigned long long)acount);
        vcount++;
      }
      decoded = avcodec_decode_video2(tracks[packet.stream_index].ctx, raw_video, &got_picture, &packet);
      if(!decoded) {
        printf("%10s undecoded: %llu", tracks[packet.stream_index].codec->name, (unsigned long long) vcount);
      } else if(packet.dts != first_vdts + vcount*vdts_step) {
        printf("%10s  bad vdts: %llu %d\n", tracks[packet.stream_index].codec->name, (unsigned long long)vcount, (int)(packet.dts - (first_vdts + vcount*vdts_step)));
      } else {
        printf("%10s good vdts: %llu\n", tracks[packet.stream_index].codec->name, (unsigned long long)vcount);
      }
      vcount++;
    } else {
      if(first_adts == -1) first_adts = packet.dts;
      while(packet.dts >= first_adts + acount*adts_step + adts_step) {
        printf("missed audio frame %llu", (unsigned long long)acount);
        acount++;
      }
      decoded = avcodec_decode_audio4(tracks[packet.stream_index].ctx, raw_audio, &got_picture, &packet);
      if(!decoded) {
        printf("%10s undecoded: %llu", tracks[packet.stream_index].codec->name, (unsigned long long) acount);
      } else if(packet.dts != first_adts + acount*adts_step) {
        printf("%10s  bad adts: %llu %d\n", tracks[packet.stream_index].codec->name, (unsigned long long)acount, (int)(packet.dts - (first_adts + acount*adts_step)));
      } else {
        printf("%10s good adts: %llu", tracks[packet.stream_index].codec->name, (unsigned long long)acount);
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
