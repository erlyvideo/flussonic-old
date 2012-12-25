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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

typedef struct Track {
  AVCodec *codec;
  AVCodecContext *ctx;
  int track_id;
  enum AVMediaType content;
  enum AVCodecID codec_id;
  int width;
  int height;
} Track;


int main(int argc, char *argv[]) {
  av_register_all();
  avcodec_register_all();
  avformat_network_init();
  av_log_set_level(AV_LOG_ERROR);

  if(argc < 1) {
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


  int video_stream_index;
  AVCodecContext *vdec_ctx;

  Track tracks[input_ctx->nb_streams];

  for(int i =0;i<input_ctx->nb_streams;i++){
    tracks[i].codec = input_ctx->streams[i]->codec;
    tracks[i].ctx = avcodec_alloc_context3(tracks[i]->codec);
    tracks[i].time_base = input_ctx->streams[i]->time_base;
    if(input_ctx->streams[i]->codec->codec_type == AVMEDIA_TYPE_VIDEO)
      video_stream_index = i;
  }

  AVPacket packet;
  av_init_packet(&packet);

  while(av_read_frame(input_ctx, &packet) >= 0) {
    printf("frame %d %8llu %8llu\n", packet.stream_index, packet.dts, packet.pts);
    av_free_packet(&packet);
    av_init_packet(&packet);
  }
}
