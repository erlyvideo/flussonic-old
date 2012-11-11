#include <sys/socket.h>
#include <arpa/inet.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>


extern int in_fd;
extern int out_fd;

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


void debug_loop(int argc, char *argv[], void (*loop)(void)) {

  listen_port = atoi(argv[1]);
  argc-=2;
  argv++; argv++;
  int listen_fd = listen_sock();
  struct sockaddr_in cli_addr;
  socklen_t cli_addr_len;


  if(argc >= 1 && !strcmp(argv[0], "-f")) {
    argc--;
    argv++;
    fprintf(stderr, "Start fork-pool\r\n");

    while(1) {
      pid_t child;
      if((child = fork()) == 0) {

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
    int cli_sock = accept(listen_fd, (struct sockaddr *)&cli_addr, &cli_addr_len);
    in_fd = out_fd = cli_sock;
    loop();
    _exit(0);
  }  
}