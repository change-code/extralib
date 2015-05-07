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

  int fd = open("/dev/net/tun", O_RDWR);
  if (fd < 0) {
    perror("ERROR: failed to open tun device");
    exit(EXIT_FAILURE);
  }

  struct ifreq ifr;
  ifr.ifr_flags = flags | mode;
  strncpy(ifr.ifr_name, argv[optind], IFNAMSIZ);

  if (ioctl(fd, TUNSETIFF, &ifr) < 0) {
    perror("ERRRO: ioctl");
    return 1;
  }

  int poll = epoll_create(1);


  struct epoll_event event = {0};
  uint32_t tun_events = EPOLLIN;

  event.events = EPOLLIN;
  event.data.fd = STDIN_FILENO;
  epoll_ctl(poll, EPOLL_CTL_ADD, STDIN_FILENO, &event);

  event.events = 0;
  event.data.fd = STDOUT_FILENO;
  epoll_ctl(poll, EPOLL_CTL_ADD, STDOUT_FILENO, &event);

  event.events = tun_events;
  event.data.fd = fd;
  epoll_ctl(poll, EPOLL_CTL_ADD, fd, &event);

  ssize_t from_port_length = 0;
  char from_port_buffer[2048];
  ssize_t to_port_length = 0;
  char to_port_buffer[2048];

  for (;;) {
    int nfds = epoll_wait(poll, &event, 1, -1);

    if (nfds == -1) {
      if (errno == EINTR)
        continue;
      perror("epoll_wait");
      exit(EXIT_FAILURE);
    }

    int event_fd = event.data.fd;
    if (event_fd == fd) {

      if (event.events == EPOLLIN) {
        to_port_length = read(fd, to_port_buffer+2, 2046);
        *(unsigned char *)to_port_buffer = (to_port_length >> 8) & 0xFF;       
        *(unsigned char *)(to_port_buffer+1) = to_port_length & 0xFF;

        event.events = EPOLLOUT;
        event.data.fd = STDOUT_FILENO;
        epoll_ctl(poll, EPOLL_CTL_MOD, STDOUT_FILENO, &event);

        tun_events &= ~EPOLLIN;
      } else if (event.events == EPOLLOUT) {
        write(fd, from_port_buffer+2, from_port_length);

        event.events = EPOLLIN;
        event.data.fd = STDIN_FILENO;
        epoll_ctl(poll, EPOLL_CTL_MOD, STDIN_FILENO, &event);

        tun_events &= ~EPOLLOUT;
      }

    } else if (event_fd == STDIN_FILENO) {
      if (read(STDIN_FILENO, from_port_buffer, 2) <= 0)
        break;

      from_port_length = (((unsigned char)from_port_buffer[0]) << 8) | ((unsigned char)from_port_buffer[1]);
      read(STDIN_FILENO, from_port_buffer+2, from_port_length);

      event.events = 0;
      event.data.fd = event_fd;
      epoll_ctl(poll, EPOLL_CTL_MOD, event_fd, &event);

      tun_events |= EPOLLOUT;
    } else if (event_fd == STDOUT_FILENO) {
      write(STDOUT_FILENO, to_port_buffer, to_port_length+2);

      event.events = 0;
      event.data.fd = event_fd;
      epoll_ctl(poll, EPOLL_CTL_MOD, event_fd, &event);

      tun_events |= EPOLLIN;
    }

    event.events = tun_events;
    event.data.fd = fd;
    epoll_ctl(poll, EPOLL_CTL_MOD, fd, &event);
  }

  return 0;
}
