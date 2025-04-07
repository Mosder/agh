#include <unistd.h>

int main (int argc, char *argv[]) {
    pid_t pid;
    int fd[2], w;
    char buf[256];

    pipe(fd);
    pid=fork();
    if (pid == 0) {
        close(fd[0]);
        w = write(fd[1], "1234567890", 10);
        sleep(5);
        close(fd[1]);
        return 0;
    }

    close(fd[1]);
    w = read(fd[0], buf, 10);
    close(fd[0]);
    buf[w] = 0;
    printf("%s\n", buf);

    return 0;
}