#include <assert.h>
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

int main (int argc, char **argv) {
	int open_device() {
		void cmd_options (struct ifreq * ifr) {
			void show_usage () {
				fprintf(stderr, "Usage: %s [options] IFNAME\n", argv[0]);
				fprintf(stderr, "\n"
					            "  -m, --mode=tun|tap\n"
					            "  -q, --multi_queue          multiple queue\n"
					            "  -h, --help                 print help message and exit\n");
			}

			struct option options[] = {
				{"mode",         required_argument, NULL, 'm'},
				{"multi_queue",  optional_argument, NULL, 'q'},
				{"help",         no_argument,       NULL, 'h'},
				{NULL,           no_argument,       NULL, 0}
			};

			if (optind >= argc) {
				fprintf(stderr, "missing interface name\n");
				show_usage();
				exit(EXIT_FAILURE);
			}

			int opt, index;
			while ((opt = getopt_long(argc, argv, "+m:qh", options, &index)) != -1) {
				switch (opt) {
				case 'm':
					if (!strcmp(optarg, "tun")) {
						ifr->ifr_flags |= IFF_TUN;
					} else if (!strcmp(optarg, "tap")) {
						ifr->ifr_flags |= IFF_TAP;
					} else {
						fprintf(stderr, "unknown mode\n");
						show_usage();
						exit(EXIT_FAILURE);
					}
					break;
				case 'q':
					ifr->ifr_flags |= IFF_MULTI_QUEUE;
					break;
				case 'h':
				default:
					show_usage();
					exit(EXIT_FAILURE);
				}
			}

			if(!((ifr->ifr_flags & IFF_TUN) | (ifr->ifr_flags & IFF_TAP))) {
				fprintf(stderr, "missing mode\n");
				show_usage();
				exit(EXIT_FAILURE);
			}

			ifr->ifr_flags |= IFF_NO_PI;
			strncpy(ifr->ifr_name, argv[optind], IFNAMSIZ);
		}

		struct ifreq ifr;
		memset(&ifr, 0, sizeof(ifr));

		int fd = open("/dev/net/tun", O_RDWR);
		if (fd < 0) {
			perror("ERROR: failed to open tun device");
			exit(EXIT_FAILURE);
		}

		cmd_options(&ifr);

		if (ioctl(fd, TUNSETIFF, &ifr) < 0) {
			perror("ERROR: ioctl");
			exit(EXIT_FAILURE);
		}
		return(fd);
	}

	void epoll_add(uint32_t events, int poll, int fd) {
		struct epoll_event ev = {0};
		ev.events = events;
		ev.data.fd = fd;
		if (epoll_ctl(poll, EPOLL_CTL_ADD, fd, &ev) < -1) {
			perror("epoll_ctl: ADD");
			exit(EXIT_FAILURE);
		}
	}

	void epoll_mod(uint32_t events, int poll, int fd) {
		struct epoll_event ev = {0};
		ev.events = events;
		ev.data.fd = fd;
		if (epoll_ctl(poll, EPOLL_CTL_MOD, fd, &ev) < -1) {
			perror("epoll_ctl: MOD");
			exit(EXIT_FAILURE);
		}
	}

	int const packet_len_byte_No = 2;
	int const BUFSIZE = 65536 + packet_len_byte_No;
	ssize_t from_port_length;
	char from_port_buffer[BUFSIZE];
	ssize_t to_port_length;
	char to_port_buffer[BUFSIZE];
	struct epoll_event event;
	uint32_t tun_event = 0;

	int device_fd = open_device();
	int poll = epoll_create(1);
	/* one event once */

	/*
	 * PORT_OUT(STDIN)  -> from_port_buffer -> DEV
	 * IN
	 * PORT_IN(STDOUT)  <- to_port_buffer   <- DEV
	 *                                         IN
	 */
	epoll_add(EPOLLIN, poll, STDIN_FILENO);
	epoll_add(0, poll, STDOUT_FILENO);
	epoll_add(EPOLLIN, poll, device_fd);

	for (;;) {
		int nfds = epoll_wait(poll, &event, 1, -1);
		/* one event once */

		if (nfds == -1) {
			if (errno == EINTR)
				continue;

			perror("epoll_wait");
			exit(EXIT_FAILURE);
		}

		if (event.data.fd == STDIN_FILENO) {
			/* port is ready to read */
			/*
			 * PORT_OUT(STDIN)  -> from_port_buffer -> DEV
			 * *IN*
			 * ===>
			 * PORT_OUT(STDIN)  -> from_port_buffer -> DEV
			 *                     DATA                OUT
			 */
			epoll_mod(0, poll, STDIN_FILENO);
			tun_event |= EPOLLOUT;
			epoll_mod(tun_event, poll, device_fd);

			if (read(STDIN_FILENO, from_port_buffer, packet_len_byte_No) == -1) {
				perror("read STDIN");
				exit(EXIT_FAILURE);
			}
			assert(packet_len_byte_No == 2);
			from_port_length = ntohs(*((uint16_t*)from_port_buffer));
			if (read(STDIN_FILENO, from_port_buffer + packet_len_byte_No, from_port_length) == -1) {
				perror("read STDIN");
				exit(EXIT_FAILURE);
			}
		} else
		if (event.data.fd == STDOUT_FILENO) {
			/* port is ready to write */
			/*
			 * PORT_IN(STDOUT)  <- to_port_buffer   <- DEV
			 * *OUT*               DATA
			 * ===>
			 * PORT_IN(STDOUT)  <- to_port_buffer   <- DEV
			 * DATA                                    IN
			 */
			epoll_mod(0, poll, STDOUT_FILENO);
			tun_event |= EPOLLIN;
			epoll_mod(tun_event, poll, device_fd);

			if (write(STDOUT_FILENO, to_port_buffer, to_port_length + packet_len_byte_No) == -1) {
				perror("write STDOUT");
				exit(EXIT_FAILURE);
			}
		} else
		if (event.data.fd == device_fd) {
			if (event.events == EPOLLIN) {
				/* device is ready to read */
				/*
				 * PORT_IN(STDOUT)  <- to_port_buffer   <- DEV
				 *                                         *IN*
				 * ==>
				 * PORT_IN(STDOUT)  <- to_port_buffer   <- DEV
				 * OUT                 DATA
				*/
				epoll_mod(EPOLLOUT, poll, STDOUT_FILENO);
				tun_event &= ~EPOLLIN;
				epoll_mod(tun_event, poll, device_fd);

				to_port_length = read(device_fd, to_port_buffer + packet_len_byte_No, BUFSIZE - packet_len_byte_No);
				if (to_port_length == -1) {
					perror("read tun device");
					exit(EXIT_FAILURE);
				}
				uint16_t length = htons(to_port_length);
				memcpy(to_port_buffer, &length, packet_len_byte_No);
			} else
			if (event.events == EPOLLOUT) {
				/* device is ready to write */
				/*
				 * PORT_OUT(STDIN)  -> from_port_buffer -> DEV
				 *                     DATA                *OUT*
				 * ==>
				 * PORT_OUT(STDIN)  -> from_port_buffer -> DEV
				 * IN                                      DATA
				 */
				epoll_mod(EPOLLIN, poll, STDIN_FILENO);
				tun_event &= ~EPOLLOUT;
				epoll_mod(tun_event, poll, device_fd);

				if (write(device_fd, from_port_buffer + packet_len_byte_No, from_port_length) == -1) {
					perror("write tun device");
					exit(EXIT_FAILURE);
				}
			}
		}
	}

  return(EXIT_SUCCESS);
}
