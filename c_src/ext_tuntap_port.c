#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/epoll.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <arpa/inet.h>

#define BUFSIZE 2048


char *executable = NULL;
short mode = 0;
short flags = IFF_NO_PI;


struct option options[] = {
  {"mode",         required_argument, NULL, 'm'},
  {"multi_queue",  optional_argument, NULL, 'q'},
  {"help",         no_argument,       NULL, 'h'},
  {NULL,           no_argument,       NULL, 0}
};


void show_usage() {
  fprintf(stderr, "Usage: %s [options] IFNAME\n", executable);
  fprintf(stderr, "\n"
          "  -m, --mode=tun|tap\n"
          "  -q, --multi_queue          multiple queue\n"
          "  -h, --help                 print help message and exit\n");
}


int epoll_set(int poll_fd, int op, int fd, uint32_t events) {
  struct epoll_event event = {
    .events = events,
    .data = { .fd = fd }
  };
  return epoll_ctl(poll_fd, op, fd, &event);
}


int main(int argc, char **argv) {
  executable = argv[0];

  int opt, index;
  while((opt = getopt_long(argc, argv, "+m:qh", options, &index)) != -1) {
    switch (opt) {
    case 'm':
      if (!strcmp(optarg, "tun")) {
        mode = IFF_TUN;
      } else if (!strcmp(optarg, "tap")) {
        mode = IFF_TAP;
      }
      break;
    case 'q':
      flags |= IFF_MULTI_QUEUE;
      break;
    case 'h':
    default:
      show_usage();
      exit(EXIT_FAILURE);
    }
  }

  if (optind >= argc) {
    fprintf(stderr, "missing interface name\n");
    show_usage();
    exit(EXIT_FAILURE);
  }

  if (!mode) {
    fprintf(stderr, "missing mode\n");
    show_usage();
    exit(EXIT_FAILURE);
  }

  int tun_fd = open("/dev/net/tun", O_RDWR);
  if (tun_fd < 0) {
    perror("ERROR: failed to open tun device");
    exit(EXIT_FAILURE);
  }

  struct ifreq ifr;
  ifr.ifr_flags = flags | mode;
  strncpy(ifr.ifr_name, argv[optind], IFNAMSIZ);

  if (ioctl(tun_fd, TUNSETIFF, &ifr) < 0) {
    perror("ERROR: ioctl");
    exit(EXIT_FAILURE);
  }

  int poll_fd = epoll_create(1);
  struct epoll_event event = {0};
  uint32_t tun_events = EPOLLIN;

  epoll_set(poll_fd, EPOLL_CTL_ADD, STDIN_FILENO, EPOLLIN);
  epoll_set(poll_fd, EPOLL_CTL_ADD, STDOUT_FILENO, 0);
  epoll_set(poll_fd, EPOLL_CTL_ADD, tun_fd, tun_events);

  /*
   * buffer
   * |<-             2048 bytes              ->|
   * +-------------+------------------+--------+
   * |   length    |      payload     | unused |
   * +-------------+------------------+--------+
   * |<- 2 bytes ->|<- length bytes ->|
   */

  /* STDIN -> from_port_buffer -> tun */
  ssize_t from_port_length = 0;
  char from_port_buffer[BUFSIZE];
  /* tun -> from_tun_buffer -> STDOUT */
  ssize_t from_tun_length = 0;
  char from_tun_buffer[BUFSIZE];

  for (;;) {
    int nfds = epoll_wait(poll_fd, &event, 1, -1);

    if (nfds == -1) {
      if (errno == EINTR)
        continue;
      perror("ERROR: epoll_wait");
      exit(EXIT_FAILURE);
    }

    if (event.data.fd == tun_fd) {
      if (event.events == EPOLLIN) {
        /* tun is ready to read */
        from_tun_length = read(tun_fd, from_tun_buffer+2, BUFSIZE-2);
        *(uint16_t *)from_tun_buffer = htons(from_tun_length);
        epoll_set(poll_fd, EPOLL_CTL_MOD, STDOUT_FILENO, EPOLLOUT);
        tun_events &= ~EPOLLIN;
      } else if (event.events == EPOLLOUT) {
        /* tun is ready to write */
        write(tun_fd, from_port_buffer+2, from_port_length);
        epoll_set(poll_fd, EPOLL_CTL_MOD, STDIN_FILENO, EPOLLIN);
        tun_events &= ~EPOLLOUT;
      }
    } else if (event.data.fd == STDIN_FILENO) {
      /* port is ready to read */
      if (read(STDIN_FILENO, from_port_buffer, 2) <= 0)
        break;

      from_port_length = ntohs(*(uint16_t *)from_port_buffer);
      read(STDIN_FILENO, from_port_buffer+2, from_port_length);
      epoll_set(poll_fd, EPOLL_CTL_MOD, STDIN_FILENO, 0);
      tun_events |= EPOLLOUT;
    } else if (event.data.fd == STDOUT_FILENO) {
      /* port is ready to write */
      write(STDOUT_FILENO, from_tun_buffer, from_tun_length+2);
      epoll_set(poll_fd, EPOLL_CTL_MOD, STDOUT_FILENO, 0);
      tun_events |= EPOLLIN;
    }

    epoll_set(poll_fd, EPOLL_CTL_MOD, tun_fd, tun_events);
  }

  return 0;
}
