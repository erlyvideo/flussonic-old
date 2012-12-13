#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>


const int MPEGTS_SIZE = 188;
#define PID_COUNT 8192

ErlDrvTermData atom_udp;


static int mpegts_init(void) {
  atom_udp = driver_mk_atom("mpegts_udp");
  return 0;
}

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner;
  int socket;
  uint8_t *buf;
  ssize_t size;
  ssize_t len;
  uint8_t counters[PID_COUNT];
  uint32_t error_count;
  uint32_t scrambled;
  uint32_t packet_count;
  ssize_t limit;
  unsigned long timeout;
} mpegts;

static ErlDrvData mpegts_drv_start(ErlDrvPort port, char *buff)
{
  mpegts* d = (mpegts *)driver_alloc(sizeof(mpegts));
  bzero(d, sizeof(mpegts));
  memset(d->counters, 0xFF, PID_COUNT);
  d->port = port;
  d->owner = driver_caller(port);
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  d->size = 2*65536;
  d->limit = 2*50000;
  d->timeout = 500;
  d->buf = (uint8_t *)driver_alloc(d->size);
  d->len = 0;
  d->socket = -1;
  return (ErlDrvData)d;
}


static void mpegts_drv_stop(ErlDrvData handle)
{
  mpegts* d = (mpegts *)handle;
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
  close(d->socket);
  // driver_free(d->buf);
  // driver_free(handle);
}



static ErlDrvSSizeT mpegts_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  mpegts* d = (mpegts*) handle;
  
  switch(command) {
    case 1: {
      int sock;
      int flags;
      struct sockaddr_in si;
      uint16_t port;
      if(len < 2) return 0;
      memcpy(&port, buf, 2);
      // fprintf(stderr, "Connecting to port %d\r\n", port);
      sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = port;
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(len >= 6) {
        memcpy(&si.sin_addr.s_addr, buf+2, 4);
      }
      if(bind(sock, (struct sockaddr *)&si, sizeof(si)) == -1) {
        fprintf(stderr, "Invalid bind to %d\r\n", ntohs(port));
        driver_failure_posix(d->port, errno);
        return 0;
        // memcpy(*rbuf, "error", 5);
        // return 5;
      }
      
      if(len >= 6) {
        struct ip_mreq mreq;
        memcpy(&mreq.imr_multiaddr.s_addr, buf+2, 4);
        mreq.imr_interface.s_addr = htonl(INADDR_ANY);

        if (setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq)) < 0) {
          perror("multicast join error\n");
          driver_failure_posix(d->port, errno);
          return 0;
        }
      }
      
      int n = 2*1024 * 1024; // 10 seconds buffer
      if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &n, sizeof(n)) == -1) {
        // deal with failure, or ignore if you can live with the default size
        driver_failure_posix(d->port, errno);
      }

      d->socket = sock;
      flags = fcntl(d->socket, F_GETFL);
      assert(flags >= 0);
      assert(!fcntl(d->socket, F_SETFL, flags | O_NONBLOCK));
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    case 2: {
      memcpy(*rbuf, &d->error_count, 4);
      d->error_count = 0;
      return 4;
    }
    case 3: {
      memcpy(*rbuf, &d->scrambled, 4);
      d->scrambled = 0;
      return 4;
    }
    case 4: {
      memcpy(*rbuf, &d->packet_count, 4);
      d->packet_count = 0;
      return 4;
    }
    case 5: {
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
      memcpy(*rbuf, "ok", 2);
      return 2;      
    }
    default:
    return 0;
  }
  return 0;
}


static void check_errors(mpegts *d) 
{
  uint8_t *packet;
  return;
  for(packet = d->buf; packet < d->buf + d->len; packet += 188) {
    if(packet[0] == 0x47) {
      d->packet_count++;
      uint16_t pid = ((packet[1] & 0x1F) << 8) | packet[2];
      uint8_t counter = packet[3] & 0x0F;
      if(packet[3] >> 7) d->scrambled++;
      // fprintf(stderr, "%d,%d,%d,%d:  %5d %2d\r\n", packet[0], packet[1], packet[2], packet[3], pid, counter);
      // fprintf(stderr, "Pid: %5d %2d\r\n", pid, counter);
      if(d->counters[pid] != 0xFF && d->counters[pid] != counter) {
        // fprintf(stderr, "Pid: %5d %2d %2d\r\n", pid, d->counters[pid], counter);
        d->error_count++;
      }
      d->counters[pid] = (counter + 1) % 0x10;
    }
  }
}

static void flush(mpegts* d) {
  if(d->len == 0) return;
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, atom_udp,
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_BUF2BINARY, (ErlDrvTermData)d->buf, (ErlDrvTermData)d->len,
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  d->len = 0;
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
}

static void mpegts_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
  mpegts* d = (mpegts*) handle;
  struct sockaddr_in peer;
  socklen_t peer_len;
  ssize_t s;


  while((s = recvfrom(d->socket, d->buf + d->len, d->size - d->len, 0, (struct sockaddr *)&peer, &peer_len)) > 0) {
    d->len += s;
    if((d->len > d->limit && (d->len % 188 == 0)) || d->len > (d->limit + d->size) / 2) {
      check_errors(d);
      flush(d);
    } else {
      driver_set_timer(d->port, d->timeout);
    }
  }
  if(errno != EAGAIN) {
    driver_failure_posix(d->port, errno);    
  }
  
}


static void mpegts_drv_timeout(ErlDrvData handle) {
  mpegts* d = (mpegts*) handle;
  flush(d);
}


ErlDrvEntry mpegts_driver_entry = {
    mpegts_init,			/* F_PTR init, N/A */
    mpegts_drv_start,		/* L_PTR start, called when port is opened */
    mpegts_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	    	/* F_PTR output, called when erlang has sent */
    mpegts_drv_input,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "mpegts_udp",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    mpegts_drv_command,			/* F_PTR control, port_command callback */
    mpegts_drv_timeout,			/* F_PTR timeout, reserved */
    NULL,			/* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(mpegts_udp) /* must match name in driver_entry */
{
    return &mpegts_driver_entry;
}
