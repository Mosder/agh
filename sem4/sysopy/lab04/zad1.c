#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 1;
    }
    int processes_count = atoi(argv[1]);
    if (processes_count <= 0) {
        printf("Liczba procesów musi być liczbą całkowitą większą od 0\n");
        return 1;
    }
    for (int i = 0; i < processes_count; i++) {
        pid_t pid = fork();
        // check if it's a child process
        if (pid == 0) {
            printf("pid macierzysty - %d, pid własny - %d\n", (int) getppid(), (int) getpid());
            return 0;
        }
    }
    
    // wait for all child prcesses to finish
    while(wait(0) > 0);
    printf("argv[1] - %d\n", processes_count);
    return 0;
}