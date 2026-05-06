#include "CEmacsHost.h"

#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <util.h>
#else
#include <pty.h>
#endif

pid_t eh_spawn_pty(int *master_fd, char *const argv[], int rows, int cols) {
    struct winsize winsize;
    winsize.ws_row = (unsigned short)rows;
    winsize.ws_col = (unsigned short)cols;
    winsize.ws_xpixel = 0;
    winsize.ws_ypixel = 0;

    pid_t pid = forkpty(master_fd, NULL, NULL, &winsize);
    if (pid == 0) {
        execvp(argv[0], argv);
        _exit(127);
    }
    return pid;
}

int eh_resize_pty(int master_fd, int rows, int cols) {
    struct winsize winsize;
    winsize.ws_row = (unsigned short)rows;
    winsize.ws_col = (unsigned short)cols;
    winsize.ws_xpixel = 0;
    winsize.ws_ypixel = 0;
    return ioctl(master_fd, TIOCSWINSZ, &winsize);
}

int eh_set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) {
        return -1;
    }
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}
