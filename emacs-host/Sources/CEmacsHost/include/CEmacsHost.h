#ifndef C_EMACS_HOST_H
#define C_EMACS_HOST_H

#include <sys/types.h>

pid_t eh_spawn_pty(int *master_fd, char *const argv[], int rows, int cols);
int eh_resize_pty(int master_fd, int rows, int cols);
int eh_set_nonblocking(int fd);

#endif
