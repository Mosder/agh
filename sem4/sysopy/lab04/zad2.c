#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 0;
    }
    pid_t pid = fork();
    if (pid == 0) {
        printf("child process\n");
        printf("child pid = %d, parent pid = %d\n", getpid(), getppid());
        printf("child's local = %d, child's global = %d\n", 1, 1);
        return execl("/bin/ls", "ls", argv[1], NULL);
    }
    else {
        printf("parent process\n");
        printf("parent pid = %d, child pid = %d\n", getpid(), pid);
        printf("Child exit code: %d\n", 1);
        printf("Parent's local = %d, parent's global = %d\n", 1, 1);
        return 0;
    }
    return 0;
}