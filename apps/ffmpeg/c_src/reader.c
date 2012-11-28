#include "reader.h"

enum frame_content read_frame_content(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"audio")) return frame_content_audio;
  if(!strcmp(atom,"video")) return frame_content_video;
  if(!strcmp(atom,"metadata")) return frame_content_metadata;
  return -1;
}
int write_frame_content(ei_x_buff *x, enum frame_content t) {
  switch(t) {
    case frame_content_audio: ei_x_encode_atom(x,"audio"); break;
    case frame_content_video: ei_x_encode_atom(x,"video"); break;
    case frame_content_metadata: ei_x_encode_atom(x,"metadata"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

enum frame_flavor read_frame_flavor(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"frame")) return frame_flavor_frame;
  if(!strcmp(atom,"keyframe")) return frame_flavor_keyframe;
  if(!strcmp(atom,"config")) return frame_flavor_config;
  if(!strcmp(atom,"disposable")) return frame_flavor_disposable;
  return -1;
}
int write_frame_flavor(ei_x_buff *x, enum frame_flavor t) {
  switch(t) {
    case frame_flavor_frame: ei_x_encode_atom(x,"frame"); break;
    case frame_flavor_keyframe: ei_x_encode_atom(x,"keyframe"); break;
    case frame_flavor_config: ei_x_encode_atom(x,"config"); break;
    case frame_flavor_disposable: ei_x_encode_atom(x,"disposable"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

enum frame_codec read_frame_codec(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"h264")) return frame_codec_h264;
  if(!strcmp(atom,"sorenson")) return frame_codec_sorenson;
  if(!strcmp(atom,"vp6")) return frame_codec_vp6;
  if(!strcmp(atom,"vp6f")) return frame_codec_vp6f;
  if(!strcmp(atom,"mpeg4")) return frame_codec_mpeg4;
  if(!strcmp(atom,"mpeg2")) return frame_codec_mpeg2;
  if(!strcmp(atom,"aac")) return frame_codec_aac;
  if(!strcmp(atom,"mp3")) return frame_codec_mp3;
  if(!strcmp(atom,"pcma")) return frame_codec_pcma;
  if(!strcmp(atom,"pcmu")) return frame_codec_pcmu;
  if(!strcmp(atom,"pcm")) return frame_codec_pcm;
  if(!strcmp(atom,"pcm_le")) return frame_codec_pcm_le;
  if(!strcmp(atom,"g726_16")) return frame_codec_g726_16;
  if(!strcmp(atom,"speex")) return frame_codec_speex;
  if(!strcmp(atom,"nellymoser")) return frame_codec_nellymoser;
  if(!strcmp(atom,"nellymoser8")) return frame_codec_nellymoser8;
  return -1;
}
int write_frame_codec(ei_x_buff *x, enum frame_codec t) {
  switch(t) {
    case frame_codec_h264: ei_x_encode_atom(x,"h264"); break;
    case frame_codec_sorenson: ei_x_encode_atom(x,"sorenson"); break;
    case frame_codec_vp6: ei_x_encode_atom(x,"vp6"); break;
    case frame_codec_vp6f: ei_x_encode_atom(x,"vp6f"); break;
    case frame_codec_mpeg4: ei_x_encode_atom(x,"mpeg4"); break;
    case frame_codec_mpeg2: ei_x_encode_atom(x,"mpeg2"); break;
    case frame_codec_aac: ei_x_encode_atom(x,"aac"); break;
    case frame_codec_mp3: ei_x_encode_atom(x,"mp3"); break;
    case frame_codec_pcma: ei_x_encode_atom(x,"pcma"); break;
    case frame_codec_pcmu: ei_x_encode_atom(x,"pcmu"); break;
    case frame_codec_pcm: ei_x_encode_atom(x,"pcm"); break;
    case frame_codec_pcm_le: ei_x_encode_atom(x,"pcm_le"); break;
    case frame_codec_g726_16: ei_x_encode_atom(x,"g726_16"); break;
    case frame_codec_speex: ei_x_encode_atom(x,"speex"); break;
    case frame_codec_nellymoser: ei_x_encode_atom(x,"nellymoser"); break;
    case frame_codec_nellymoser8: ei_x_encode_atom(x,"nellymoser8"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

enum frame_sound_channels read_frame_sound_channels(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"mono")) return frame_sound_channels_mono;
  if(!strcmp(atom,"stereo")) return frame_sound_channels_stereo;
  return -1;
}
int write_frame_sound_channels(ei_x_buff *x, enum frame_sound_channels t) {
  switch(t) {
    case frame_sound_channels_mono: ei_x_encode_atom(x,"mono"); break;
    case frame_sound_channels_stereo: ei_x_encode_atom(x,"stereo"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

enum frame_sound_size read_frame_sound_size(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"bit8")) return frame_sound_size_bit8;
  if(!strcmp(atom,"bit16")) return frame_sound_size_bit16;
  return -1;
}
int write_frame_sound_size(ei_x_buff *x, enum frame_sound_size t) {
  switch(t) {
    case frame_sound_size_bit8: ei_x_encode_atom(x,"bit8"); break;
    case frame_sound_size_bit16: ei_x_encode_atom(x,"bit16"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

enum frame_sound_rate read_frame_sound_rate(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"rate5")) return frame_sound_rate_rate5;
  if(!strcmp(atom,"rate11")) return frame_sound_rate_rate11;
  if(!strcmp(atom,"rate22")) return frame_sound_rate_rate22;
  if(!strcmp(atom,"rate44")) return frame_sound_rate_rate44;
  return -1;
}
int write_frame_sound_rate(ei_x_buff *x, enum frame_sound_rate t) {
  switch(t) {
    case frame_sound_rate_rate5: ei_x_encode_atom(x,"rate5"); break;
    case frame_sound_rate_rate11: ei_x_encode_atom(x,"rate11"); break;
    case frame_sound_rate_rate22: ei_x_encode_atom(x,"rate22"); break;
    case frame_sound_rate_rate44: ei_x_encode_atom(x,"rate44"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

struct video_frame* read_video_frame(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct video_frame);
  struct video_frame* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"video_frame")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  {int t,s;
  ei_get_type(buf,&idx1,&t,&s);
  if(t != ERL_BINARY_EXT) return NULL;
  out_size += s;}
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct video_frame*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct video_frame);

  // Unpack all fields
  r->content = read_frame_content(buf,idx);
  if(!r->content) {free(r); return NULL;}
  if(ei_decode_double(buf,idx,&r->dts) < 0) {
     ei_skip_term(buf,idx);
     r->dts = 0.000;
  }
  if(ei_decode_double(buf,idx,&r->pts) < 0) {
     ei_skip_term(buf,idx);
     r->pts = 0.000;
  }
  if(ei_decode_long(buf,idx,&r->stream_id) < 0) {
     ei_skip_term(buf,idx);
     r->stream_id = 0;
  }
  r->codec = read_frame_codec(buf,idx);
  if(!r->codec) {free(r); return NULL;}
  r->flavor = read_frame_flavor(buf,idx);
  if(!r->flavor) {free(r); return NULL;}
  if(ei_decode_long(buf,idx,&r->track_id) < 0) {
     ei_skip_term(buf,idx);
     r->track_id = 0;
  }
  {int t,s;
  ei_get_type(buf,idx,&t,&s);
  if(t != ERL_BINARY_EXT) {free(r); return NULL;}
  r->body.size = s;}
  r->body.data = endptr;
  endptr += r->body.size;
  ei_decode_binary(buf,idx,r->body.data,&r->body.size);
  ei_skip_term(buf,idx); // any next_id

  return r;
}

int write_video_frame(ei_x_buff* x, struct video_frame r) {
  ei_x_encode_tuple_header(x, 10);
  ei_x_encode_atom(x, "video_frame");
  write_frame_content(x,r.content);
  ei_x_encode_double(x, r.dts);
  ei_x_encode_double(x, r.pts);
  ei_x_encode_double(x, r.stream_id);
  write_frame_codec(x,r.codec);
  write_frame_flavor(x,r.flavor);
  ei_x_encode_double(x, r.track_id);
  ei_x_encode_binary(x, r.body.data, r.body.size);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

struct video_params* read_video_params(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct video_params);
  struct video_params* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"video_params")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct video_params*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct video_params);

  // Unpack all fields
  if(ei_decode_long(buf,idx,&r->width) < 0) {
     ei_skip_term(buf,idx);
     r->width = 0;
  }
  if(ei_decode_long(buf,idx,&r->height) < 0) {
     ei_skip_term(buf,idx);
     r->height = 0;
  }
  if(ei_decode_long(buf,idx,&r->fps) < 0) {
     ei_skip_term(buf,idx);
     r->fps = 0;
  }
  if(ei_decode_long(buf,idx,&r->length_size) < 0) {
     ei_skip_term(buf,idx);
     r->length_size = 4;
  }
  ei_skip_term(buf,idx); // any nals
  return r;
}

int write_video_params(ei_x_buff* x, struct video_params r) {
  ei_x_encode_tuple_header(x, 6);
  ei_x_encode_atom(x, "video_params");
  ei_x_encode_double(x, r.width);
  ei_x_encode_double(x, r.height);
  ei_x_encode_double(x, r.fps);
  ei_x_encode_double(x, r.length_size);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

struct audio_params* read_audio_params(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct audio_params);
  struct audio_params* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"audio_params")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct audio_params*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct audio_params);

  // Unpack all fields
  if(ei_decode_long(buf,idx,&r->channels) < 0) {
     ei_skip_term(buf,idx);
     r->channels = 0;
  }
  if(ei_decode_long(buf,idx,&r->sample_rate) < 0) {
     ei_skip_term(buf,idx);
     r->sample_rate = 0;
  }
  if(ei_decode_long(buf,idx,&r->samples) < 0) {
     ei_skip_term(buf,idx);
     r->samples = 0;
  }
  ei_skip_term(buf,idx); // any config
  return r;
}

int write_audio_params(ei_x_buff* x, struct audio_params r) {
  ei_x_encode_tuple_header(x, 5);
  ei_x_encode_atom(x, "audio_params");
  ei_x_encode_double(x, r.channels);
  ei_x_encode_double(x, r.sample_rate);
  ei_x_encode_double(x, r.samples);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

struct stream_info* read_stream_info(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct stream_info);
  struct stream_info* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"stream_info")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  {int t,s;
  ei_get_type(buf,&idx1,&t,&s);
  if(t != ERL_BINARY_EXT) return NULL;
  out_size += s;}
  ei_skip_term(buf,&idx1);
  {int t,s;
  ei_get_type(buf,&idx1,&t,&s);
  if(t != ERL_BINARY_EXT) return NULL;
  out_size += s;}
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct stream_info*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct stream_info);

  // Unpack all fields
  r->content = read_frame_content(buf,idx);
  if(!r->content) {free(r); return NULL;}
  if(ei_decode_long(buf,idx,&r->track_id) < 0) {
     ei_skip_term(buf,idx);
     r->track_id = 0;
  }
  r->codec = read_frame_codec(buf,idx);
  if(!r->codec) {free(r); return NULL;}
  {int t,s;
  ei_get_type(buf,idx,&t,&s);
  if(t != ERL_BINARY_EXT) {free(r); return NULL;}
  r->config.size = s;}
  r->config.data = endptr;
  endptr += r->config.size;
  ei_decode_binary(buf,idx,r->config.data,&r->config.size);
  if(ei_decode_long(buf,idx,&r->bitrate) < 0) {
     ei_skip_term(buf,idx);
     r->bitrate = 0;
  }
  {int t,s;
  ei_get_type(buf,idx,&t,&s);
  if(t != ERL_BINARY_EXT) {free(r); return NULL;}
  r->language.size = s;}
  r->language.data = endptr;
  endptr += r->language.size;
  ei_decode_binary(buf,idx,r->language.data,&r->language.size);
  ei_skip_term(buf,idx); // atom params
  ei_skip_term(buf,idx); // non_neg_integer timescale
  ei_skip_term(buf,idx); // any options
  return r;
}

int write_stream_info(ei_x_buff* x, struct stream_info r) {
  ei_x_encode_tuple_header(x, 10);
  ei_x_encode_atom(x, "stream_info");
  write_frame_content(x,r.content);
  ei_x_encode_double(x, r.track_id);
  write_frame_codec(x,r.codec);
  ei_x_encode_binary(x, r.config.data, r.config.size);
  ei_x_encode_double(x, r.bitrate);
  ei_x_encode_binary(x, r.language.data, r.language.size);
  ei_x_encode_atom(x, "undefined");
  ei_x_encode_double(x, r.timescale);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

enum flow_type read_flow_type(char *buf, int *idx) {
  char atom[MAXATOMLEN+1];
  if(ei_decode_atom(buf,idx,atom) < 0) return 0;
  if(!strcmp(atom,"file")) return flow_type_file;
  if(!strcmp(atom,"stream")) return flow_type_stream;
  return -1;
}
int write_flow_type(ei_x_buff *x, enum flow_type t) {
  switch(t) {
    case flow_type_file: ei_x_encode_atom(x,"file"); break;
    case flow_type_stream: ei_x_encode_atom(x,"stream"); break;
    default: ei_x_encode_atom(x,"undefined");
  }
  return 0;
}

struct media_info* read_media_info(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct media_info);
  struct media_info* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"media_info")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct media_info*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct media_info);

  // Unpack all fields
  r->flow_type = read_flow_type(buf,idx);
  if(!r->flow_type) {free(r); return NULL;}
  ei_skip_term(buf,idx); // any streams
  if(ei_decode_long(buf,idx,&r->duration) < 0) {
     ei_skip_term(buf,idx);
     r->duration = 0;
  }
  ei_skip_term(buf,idx); // any options
  return r;
}

int write_media_info(ei_x_buff* x, struct media_info r) {
  ei_x_encode_tuple_header(x, 5);
  ei_x_encode_atom(x, "media_info");
  write_flow_type(x,r.flow_type);
  ei_x_encode_atom(x, "undefined");
  ei_x_encode_double(x, r.duration);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

struct init_output* read_init_output(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct init_output);
  struct init_output* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"init_output")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  // Now allocate structure
  r = (struct init_output*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct init_output);

  // Unpack all fields
  r->content = read_frame_content(buf,idx);
  if(!r->content) {free(r); return NULL;}
  r->codec = read_frame_codec(buf,idx);
  if(!r->codec) {free(r); return NULL;}
  if(ei_decode_long(buf,idx,&r->track_id) < 0) {
     ei_skip_term(buf,idx);
     r->track_id = 0;
  }
  ei_skip_term(buf,idx); // any options
  return r;
}

int write_init_output(ei_x_buff* x, struct init_output r) {
  ei_x_encode_tuple_header(x, 5);
  ei_x_encode_atom(x, "init_output");
  write_frame_content(x,r.content);
  write_frame_codec(x,r.codec);
  ei_x_encode_double(x, r.track_id);
  ei_x_encode_atom(x, "undefined");
  return 0;
}

struct init_input* read_init_input(char *buf, int *idx) {
  // Declare all fields and unpack tuple header
  ssize_t out_size = sizeof(struct init_input);
  struct init_input* r;
  char header[MAXATOMLEN+1];
  int arity=-1;
  if(ei_decode_tuple_header(buf, idx, &arity) < 0) return NULL;
  if(ei_decode_atom(buf, idx, header) < 0) return NULL;
  if(strcmp(header,"init_input")) return NULL;
  // Now need to jump through the message to calculate total memory
  int idx1 = *idx;
  ei_skip_term(buf,&idx1);
  ei_skip_term(buf,&idx1);
  {int t,s;
  ei_get_type(buf,&idx1,&t,&s);
  if(t != ERL_BINARY_EXT) return NULL;
  out_size += s;}
  // Now allocate structure
  r = (struct init_input*)calloc(out_size,1);
  void *endptr = (void *)r + sizeof(struct init_input);

  // Unpack all fields
  r->content = read_frame_content(buf,idx);
  if(!r->content) {free(r); return NULL;}
  r->codec = read_frame_codec(buf,idx);
  if(!r->codec) {free(r); return NULL;}
  {int t,s;
  ei_get_type(buf,idx,&t,&s);
  if(t != ERL_BINARY_EXT) {free(r); return NULL;}
  r->config.size = s;}
  r->config.data = endptr;
  endptr += r->config.size;
  ei_decode_binary(buf,idx,r->config.data,&r->config.size);
  return r;
}

int write_init_input(ei_x_buff* x, struct init_input r) {
  ei_x_encode_tuple_header(x, 4);
  ei_x_encode_atom(x, "init_input");
  write_frame_content(x,r.content);
  write_frame_codec(x,r.codec);
  ei_x_encode_binary(x, r.config.data, r.config.size);
  return 0;
}

