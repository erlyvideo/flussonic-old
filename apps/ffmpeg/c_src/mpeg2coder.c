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

#include "compat.h"

struct AudioQueue;
typedef struct AudioQueue {
  AVPacket packet;
  struct AudioQueue *next;
} AudioQueue;

typedef struct AudioGraph {
  AVFilterContext *buffersink_ctx;
  AVFilterContext *buffersrc_ctx;
  AVFilterGraph *filter_graph;  
} AudioGraph;

typedef struct Track {
  int track_id;
  enum AVMediaType content;
  enum AVCodecID codec_id;
  int width;
  int height;
  AVRational time_base;
  AVCodecContext *venc_ctx;
  AVFormatContext* output_ctx;
  AVStream *vout_st;
  AVStream *aout_st;
} Track;

void error(const char *fmt, ...);
AudioGraph *init_audio_graph(AVStream *in_stream);
void initialize_audio(AVCodec **adec, AVCodecContext **adec_ctx, AVCodec **aenc, AVCodecContext **aenc_ctx, AVCodecContext *in_stream_ctx);
void initialize_video(Track *track, AVCodecContext *in_ctx, char *path);



int main(int argc, char *argv[]) {
  av_register_all();
  avcodec_register_all();
  avformat_network_init();
  avfilter_register_all();
  av_log_set_level(AV_LOG_ERROR);

  if(argc < 3) {
    error("%s input-ts output-ts", argv[0]);
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

  int sample_rate;



  Track tracks[input_ctx->nb_streams];
  int nb_tracks = input_ctx->nb_streams;



  AVCodecContext *aenc_ctx = NULL, *adec_ctx = NULL;
  AudioQueue *audio_queue = NULL;
  AVCodec *aenc = NULL, *adec = NULL;

  AVCodec *vdec = NULL;
  AVCodecContext *vdec_ctx = NULL;

  int i;
  for(i =0;i<input_ctx->nb_streams;i++){

    tracks[i].content = input_ctx->streams[i]->codec->codec_type;
    tracks[i].time_base = input_ctx->streams[i]->time_base;
    if(tracks[i].content == AVMEDIA_TYPE_VIDEO) {
      video_stream_index = i;
      vdts_step = 3600;

      vdec = avcodec_find_decoder(input_ctx->streams[i]->codec->codec_id);
      vdec_ctx = avcodec_alloc_context3(vdec);
      if(avcodec_open2(vdec_ctx, vdec, NULL) < 0) {
        error("Failed to open decoder %s", vdec->name);
      }
      initialize_video(&tracks[i], input_ctx->streams[i]->codec, argv[2]);
    } else if(tracks[i].content == AVMEDIA_TYPE_AUDIO) {
      audio_stream_index = i;
      tracks[i].output_ctx = NULL;
      sample_rate = input_ctx->streams[i]->codec->sample_rate;
      initialize_audio(&adec, &adec_ctx, &aenc, &aenc_ctx, input_ctx->streams[i]->codec);
    }
      
  }

  for(i = 0; i < nb_tracks; i++) {
    if(tracks[i].content == AVMEDIA_TYPE_VIDEO) {
      tracks[i].aout_st = avformat_new_stream(tracks[i].output_ctx, aenc);
      tracks[i].aout_st->codec = aenc_ctx;
      if(avformat_write_header(tracks[i].output_ctx, NULL) < 0) 
        error("Failed to start output format");
    }
  }


  AudioGraph *audio = init_audio_graph(input_ctx->streams[audio_stream_index]);

  if(aenc_ctx) {
    av_buffersink_set_frame_size(audio->buffersink_ctx, aenc_ctx->frame_size);
  }



  int64_t first_dts = -1;
  int64_t first_vdts = -1;
  uint64_t vcount = 0;
  int64_t first_adts = -1;
  uint64_t acount = 0;

  AVFrame *raw_video, *raw_audio;
  raw_video = avcodec_alloc_frame();
  raw_audio = avcodec_alloc_frame();

  int in_buffer_size = 1024*1024;
  uint8_t *in_buffer = av_malloc(in_buffer_size);

  while(1) {
    AVPacket in_pkt;
    av_init_packet(&in_pkt);
    in_pkt.data = in_buffer;
    in_pkt.size = in_buffer_size;

    AVPacket out_pkt;

    if(av_read_frame(input_ctx, &in_pkt) < 0) 
      error("eof");

    if(first_dts == -1) first_dts = in_pkt.dts;
    int decoded = 0;
    int encoded = 0;
    if(in_pkt.stream_index == video_stream_index) {
      // if(first_vdts == -1) first_vdts = (packet.dts / vdts_step)*vdts_step;
      if(first_vdts == -1) first_vdts = in_pkt.dts - first_dts;

      // while(packet.dts >= first_vdts + vcount*vdts_step + vdts_step) {
      //   printf("missed video frame %llu\n", (unsigned long long)vcount);
      //   vcount++;
      // }
      avcodec_decode_video2(vdec_ctx, raw_video, &decoded, &in_pkt);
      // raw_video->pts = first_vdts + vcount*vdts_step;
      raw_video->pts = vcount*vdts_step;
      // printf("video %llu\n", raw_video->pts);

      if(decoded) {
        // deinterlace!

        AVPicture picture2;
        AVCodecContext *dec = vdec_ctx;
        uint8_t *deinterlaced = av_malloc(avpicture_get_size(dec->pix_fmt, dec->width, dec->height));
        avpicture_fill(&picture2, deinterlaced, dec->pix_fmt, dec->width, dec->height);
        avpicture_deinterlace(&picture2, (AVPicture *)raw_video, dec->pix_fmt, dec->width, dec->height);
        *(AVPicture *)raw_video = picture2;


        int i = in_pkt.stream_index;
        encoded = 0;
        av_new_packet(&out_pkt, 1024*1024);

        if(avcodec_encode_video2(tracks[i].venc_ctx, &out_pkt, raw_video, &encoded) < 0)
          error("Failed to encode h264");

        av_free(deinterlaced);
        
        if(encoded) {

          while(audio_queue && audio_queue->packet.dts < out_pkt.dts) {
            audio_queue->packet.dts += 2;
            audio_queue->packet.pts += 2;
            // printf(" aac dts: %10lld, bytes: %d\n", audio_queue->packet.dts, audio_queue->packet.size);

          // int k;
          // printf("PKT %p: ", audio_queue->packet.data);
          // for(k = 0; k < 16 && k < audio_queue->packet.size; k++) printf("%d,", audio_queue->packet.data[k]);
          // printf("\n");

            audio_queue->packet.stream_index = tracks[i].aout_st->index;
            av_write_frame(tracks[i].output_ctx, &audio_queue->packet);
            av_free_packet(&audio_queue->packet);
            AudioQueue *q = audio_queue->next;
            av_free(audio_queue);
            audio_queue = q;
          }

          // WTF!!! libx264 generates AnnexB but with short startcode 0,0,1. mpegts.c required long startcode 0,0,0,1
          if(out_pkt.data[0] == 0 && out_pkt.data[1] == 0 && out_pkt.data[2] == 1) {
            memmove(out_pkt.data + 1, out_pkt.data, out_pkt.size);
            out_pkt.data[0] = 0;
          }
          // int k;
          // for(k = 0; k < 16; k++) printf("%x,", out_pkt.data[k]);
          // printf("\n");


          // printf("h264 dts: %10llu, bytes: %d\n", out_pkt.dts, out_pkt.size);
          out_pkt.stream_index = tracks[i].vout_st->index;
          if(av_write_frame(tracks[i].output_ctx, &out_pkt) < 0)
            error("failed to write output video frame");

          av_free_packet(&out_pkt);
        }
      }

      vcount++;
    } else if(in_pkt.stream_index == audio_stream_index) {
      // if(first_adts == -1) first_adts = (packet.dts / adts_step)*adts_step;
      if(first_dts == -1) first_adts = in_pkt.dts - first_dts;
      // while(packet.dts >= first_adts + acount*adts_step + adts_step) {
      //   printf("missed audio frame %llu\n", (unsigned long long)acount);
      //   acount++;
      // }
      decoded = 0;
      if(avcodec_decode_audio4(adec_ctx, raw_audio, &decoded, &in_pkt) < 0) {
        printf("failed to decode\n");
      }
      // *1152 / sample_rate
      // raw_audio->pts = first_adts + acount*adts_step;

      if(!decoded) {
        printf("filling audio with silence\n");
      }
 
      if(decoded) {
        if (av_buffersrc_add_frame(audio->buffersrc_ctx, raw_audio, AV_BUFFERSRC_FLAG_PUSH) < 0) {
          error("Error while feeding the audio filtergraph");
        }
        AVFilterBufferRef *samplesref;
        int ret = 0;
        while (1) {
          ret = av_buffersink_get_buffer_ref(audio->buffersink_ctx, &samplesref, AV_BUFFERSINK_FLAG_NO_REQUEST);
          if(ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
            break;
          if(ret < 0)
            error("failed to pass audio through filter");
          if (samplesref) {
            const AVFilterBufferRefAudioProps *props = samplesref->audio;
            AVFrame *filtered_frame = avcodec_alloc_frame();
            avcodec_get_frame_defaults(filtered_frame);
            avfilter_copy_buf_props(filtered_frame, samplesref);
            // filtered_frame->pts = first_adts + acount*90000 / sample_rate;
            filtered_frame->pts = acount*90000 / sample_rate;
            acount += props->nb_samples;

            encoded = 0;
            av_new_packet(&out_pkt, 4096);


            if(avcodec_encode_audio2(aenc_ctx, &out_pkt, filtered_frame, &encoded) < 0)
              error("Failed to encode aac");

            // printf("out_pkt size %d\n", out_pkt.size);

            if(encoded) {
              AudioQueue *q = av_malloc(sizeof(AudioQueue));
              q->packet = out_pkt;
              q->next = NULL;
              AudioQueue **head = &audio_queue;
              while(*head) {
                head = &((*head)->next);
              }
              *head = q;
            }

            avcodec_free_frame(&filtered_frame);
            avfilter_unref_bufferp(&samplesref);
          }
        }

      }

    }
    av_free_packet(&in_pkt);
  }
}












/*************************************************************************************************************/
//
//                       Initialization
//
/*************************************************************************************************************/





void initialize_video(Track *track, AVCodecContext *in_ctx, char *path) {

  track->output_ctx = avformat_alloc_context();
  track->output_ctx->oformat = av_guess_format("mpegts", NULL, NULL);

  AVCodec *venc = avcodec_find_encoder_by_name("libx264");
  if(!venc) error("failed to find libx264 encoder");


  AVCodecContext* venc_ctx = avcodec_alloc_context3(venc);
  if(!venc_ctx) error("failed to allocate video encoding context");
  venc_ctx->width = in_ctx->width;
  venc_ctx->height = in_ctx->height;
  venc_ctx->bit_rate = 300000;
  venc_ctx->pix_fmt = AV_PIX_FMT_YUV420P;
  // venc_ctx->flags |= CODEC_FLAG_GLOBAL_HEADER;
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

  if(avformat_alloc_output_context2(&track->output_ctx, track->output_ctx->oformat, NULL, path) < 0)
    error("Failed to open output context %s", path);


  if(avio_open2(&track->output_ctx->pb, path, AVIO_FLAG_WRITE,
    &track->output_ctx->interrupt_callback, NULL) < 0)
    error("failed to open %s for writing", path);

  track->venc_ctx = venc_ctx;
  track->vout_st = avformat_new_stream(track->output_ctx, venc);
  track->vout_st->codec = venc_ctx;
}






AudioGraph *init_audio_graph(AVStream *in_stream) {
  const char *filters_descr = "aresample=48000,aconvert=s16:stereo";

  AudioGraph *audio = av_malloc(sizeof(AudioGraph));
  AVCodecContext *dec_ctx = in_stream->codec;
  char args[512];
  AVFilter *abuffersrc  = avfilter_get_by_name("abuffer");
  AVFilter *abuffersink = avfilter_get_by_name("ffabuffersink");
  AVFilterInOut *outputs = avfilter_inout_alloc();
  AVFilterInOut *inputs  = avfilter_inout_alloc();
  const enum AVSampleFormat sample_fmts[] = { AV_SAMPLE_FMT_FLTP, -1 };
  AVABufferSinkParams *abuffersink_params;
  const AVFilterLink *outlink;
  AVRational time_base = in_stream->time_base;

  audio->filter_graph = avfilter_graph_alloc();

  /* buffer audio source: the decoded frames from the decoder will be inserted here. */
  snprintf(args, sizeof(args),
          "time_base=%d/%d:sample_rate=%d:sample_fmt=%s:channel_layout=0x%"PRIx64,
           time_base.num, time_base.den, dec_ctx->sample_rate,
           av_get_sample_fmt_name(dec_ctx->sample_fmt), dec_ctx->channel_layout);
  if(avfilter_graph_create_filter(&audio->buffersrc_ctx, abuffersrc, "in",
                                     args, NULL, audio->filter_graph) < 0)
      error("Cannot create audio buffer source");

  /* buffer audio sink: to terminate the filter chain. */
  abuffersink_params = av_abuffersink_params_alloc();
  abuffersink_params->sample_fmts     = sample_fmts;
  if(avfilter_graph_create_filter(&audio->buffersink_ctx, abuffersink, "out",
                                     NULL, abuffersink_params, audio->filter_graph) < 0)
    error("Cannot create audio buffer sink");
  av_free(abuffersink_params);

  /* Endpoints for the filter graph. */
  outputs->name       = av_strdup("in");
  outputs->filter_ctx = audio->buffersrc_ctx;
  outputs->pad_idx    = 0;
  outputs->next       = NULL;

  inputs->name       = av_strdup("out");
  inputs->filter_ctx = audio->buffersink_ctx;
  inputs->pad_idx    = 0;
  inputs->next       = NULL;

  if (avfilter_graph_parse(audio->filter_graph, filters_descr,
                                  &inputs, &outputs, NULL) < 0)
    error("Failed to parse audio graph config: %s", filters_descr);

  if (avfilter_graph_config(audio->filter_graph, NULL) < 0)
    error("Failed to configure audio graph");

  /* Print summary of the sink buffer
   * Note: args buffer is reused to store channel layout string */
  outlink = audio->buffersink_ctx->inputs[0];
  av_get_channel_layout_string(args, sizeof(args), -1, outlink->channel_layout);
  av_log(NULL, AV_LOG_INFO, "Output: srate:%dHz fmt:%s chlayout:%s\n",
         (int)outlink->sample_rate,
         (char *)av_x_if_null(av_get_sample_fmt_name(outlink->format), "?"),
         args);

  return audio;
}



void initialize_audio(AVCodec **adec_, AVCodecContext **adec_ctx_, AVCodec **aenc_, AVCodecContext **aenc_ctx_, AVCodecContext *in_stream_ctx) {
  AVCodec *adec = avcodec_find_decoder(in_stream_ctx->codec_id);
  AVCodecContext* adec_ctx = avcodec_alloc_context3(adec);
  if(avcodec_open2(adec_ctx, adec, NULL) < 0)
    error("Failed to open decoder %s", adec->name);
  *adec_ = adec;
  *adec_ctx_ = adec_ctx;


  AVCodec *aenc = avcodec_find_encoder_by_name("aac");
  if(!aenc) error("failed to find aac encoder");


  AVCodecContext *aenc_ctx = avcodec_alloc_context3(aenc);
  if(!aenc_ctx) error("failed to alloc audio context");



  aenc_ctx->channels = in_stream_ctx->channels;
  aenc_ctx->sample_rate = in_stream_ctx->sample_rate;
  aenc_ctx->bit_rate = 64000;
  aenc_ctx->sample_fmt = AV_SAMPLE_FMT_FLTP;
  aenc_ctx->channel_layout = AV_CH_LAYOUT_STEREO;
  aenc_ctx->strict_std_compliance = FF_COMPLIANCE_EXPERIMENTAL;
  aenc_ctx->time_base = (AVRational){1,90};

  if(avcodec_open2(aenc_ctx, aenc, NULL) < 0)
    error("failed to initialize audio encoder");

  *aenc_ = aenc;
  *aenc_ctx_ = aenc_ctx;
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


