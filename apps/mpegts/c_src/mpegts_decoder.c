#include <stdint.h>
#include "erl_nif.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

ErlNifResourceType *MpegtsResource;

#define TS_BUFFERED 188 //4*188

typedef struct {
  char *desc;
  uint8_t last_ts_size;
  uint8_t last_ts[TS_BUFFERED];
  uint16_t pmt;
} Mpegts;

static void 
mpegts_destructor(ErlNifEnv* env, void *obj)
{
  // Mpegts *desc = (Mpegts *)obj;
}


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  MpegtsResource = enif_open_resource_type(env, NULL, "mpegts_resource", mpegts_destructor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
  return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


void hexdump(uint8_t *p, uint32_t size) {
  int i = 1;
  while(size > 0) {
    fprintf(stderr,"%d,", p[0]);
    if(i % 8 == 0) fprintf(stderr, "\r\n");
    p++;
    size--;
    i++;
  }
  fprintf(stderr, "\r\n");
}


static ERL_NIF_TERM
mpegts_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Mpegts *ts;
  ERL_NIF_TERM reply;
  char info[] = "MPEG-TS decoder";
  ts = (Mpegts *)enif_alloc_resource(MpegtsResource, sizeof(Mpegts));
  ts->desc = (char *)malloc(sizeof(info)+1);
  memcpy(ts->desc, info, strlen(info) + 1);

  ts->last_ts_size = 0;
  ts->pmt = 0xFFFF;

  reply = enif_make_resource_binary(env, (void *)ts, ts->desc, strlen(ts->desc));
  enif_release_resource(ts);

  return reply;
}

static void handle_ts(Mpegts *ts, uint8_t *pkt);
static void handle_pat(Mpegts *ts, uint8_t *pkt);
static void handle_pmt(Mpegts *ts, uint8_t *pkt);
static uint8_t *payload(uint8_t *pkt);
static uint8_t payload_size(uint8_t *payload, uint8_t *pkt);

static ERL_NIF_TERM
mpegts_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  Mpegts *ts;
  int eof = 0;
  if(argc != 2) return enif_make_badarg(env);

  char eof_str[10];


  if(!enif_inspect_binary(env, argv[0], &bin)) {
    if(!enif_get_atom(env, argv[0], eof_str, sizeof(eof_str), ERL_NIF_LATIN1)) return enif_make_badarg(env);
    if(!strcmp(eof_str, "eof")) eof = 1;
  }

  if(eof) return enif_make_tuple3(env, enif_make_atom(env, "ok"), argv[1], enif_make_list(env, 0));


  if(!enif_get_resource(env, argv[1], MpegtsResource, (void **)&ts)) {
    return enif_make_badarg(env); 
  }


  // fprintf(stderr, "Decode %d bytes, %d in buffer, %d going to leave \r\n", (int)bin.size, ts->last_ts_size, (bin.size - ts->last_ts_size) % 188);

  uint8_t *pkt = bin.data;
  uint32_t size = bin.size;
  if(ts->last_ts_size > 0) {
    if(ts->last_ts_size + size < TS_BUFFERED) {
      memcpy(ts->last_ts + ts->last_ts_size, pkt, size);
      ts->last_ts_size += bin.size;
      size = 0;
    } else {
      uint32_t size1 = TS_BUFFERED - ts->last_ts_size;
      memcpy(ts->last_ts + ts->last_ts_size, pkt, size1);
      pkt += size1;
      size -= size1;
      ts->last_ts_size += size1;
      size1 = 0;
      while(size1 <= ts->last_ts_size - 188) {
        handle_ts(ts, ts->last_ts+size1);
        size1 += 188;
      }
      ts->last_ts_size = 0;
    }
  }


  while(size >= TS_BUFFERED) {
    while(size >= TS_BUFFERED && (pkt[0] != 0x47 || pkt[188] != 0x47 || pkt[2*188] != 0x47)) {
      pkt++;
      size--;
    }
    if(size >= TS_BUFFERED) {
      handle_ts(ts, pkt);
      pkt+=188;
      size-=188;
    }
  }

  if(size > 0) {
    assert(size <= TS_BUFFERED);
    ts->last_ts_size = size;
    memcpy(ts->last_ts, pkt, ts->last_ts_size);
  }
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), argv[1], enif_make_list(env, 0));
}


static void 
handle_ts(Mpegts *ts, uint8_t *pkt) {
  uint16_t pid = ((pkt[1] & 0x1F) << 8) | pkt[2];
  fprintf(stderr, "%02x,%02x,%02x  pid %d (pmt = %d)\r\n", pkt[0], pkt[1], pkt[2], pid, ts->pmt);
  if(pid == 0) handle_pat(ts, pkt);
  if(pid == ts->pmt) handle_pmt(ts, pkt);
}

// psi(<<_Pointer, TableId, _SectionInd:1, _:3, SectionLength:12, TransportStreamId:16, _:2, Version:5, CurrentNext:1, 
//             SectionNumber, LastSectionNumber, PSIPayload/binary>> = PSIRaw) ->


static void
handle_pat(Mpegts *ts, uint8_t *pkt) {
  uint8_t *psi = payload(pkt);
  uint16_t size = payload_size(psi,pkt);
  if(size < 9+4) return;
  if(!psi) return;
  if(psi[1] != 0) return;
  uint32_t length = ((psi[2] & 0xF) << 8) | psi[3];
  if(length < 4 || length + 9 > size) return;
  psi += 9;
  // extract_pat(<<ProgramNum:16, _:3, Pid:13, PAT/binary>>, Descriptors) ->
  ts->pmt = ((psi[2] & 0x1F) << 8) | psi[3];
  fprintf(stderr, "PMT pid = %d\r\n", ts->pmt);
}



static void
handle_pmt(Mpegts *ts, uint8_t *pkt) {
  uint8_t *psi = payload(pkt);
  uint16_t size = payload_size(psi,pkt);
  if(size < 9+4) return;
  if(!psi) return;
  if(psi[1] != 0) return;
  uint32_t length = ((psi[2] & 0xF) << 8) | psi[3];
  if(length < 4 || length + 9 > size) return;
  psi += 9;
  fprintf(stderr, "PMT %d bytes\r\n", length);
}


// 16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, HasAdaptationField:1, HasPayload:1, Counter:4, Payload:184

static uint8_t *
payload(uint8_t *pkt) {
  if(pkt[0] != 0x47) return NULL;
  uint8_t flags = pkt[3] >> 4;
  uint8_t has_payload = flags & 1;
  uint8_t has_adaptation = flags & 2;
  if(!has_payload) return NULL;
  if(has_adaptation) return pkt + 4 + 1 + pkt[4];
  return pkt + 4;
}

static uint8_t
payload_size(uint8_t *payload, uint8_t *pkt) {
  if(!payload) return 0;
  return 188 - (payload - pkt);
}



























static ErlNifFunc mpegts_funcs[] =
{
  {"init", 0, mpegts_init},
  {"decode", 2, mpegts_decode}
};

ERL_NIF_INIT(mpegts_decoder_c, mpegts_funcs, load, reload, upgrade, unload)





