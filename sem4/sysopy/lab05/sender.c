#include <signal.h>

int main(void) {
    union sigval war;
    int pid = atoi(tab[1]);

    war.sival_int = 123;
    sigqueue(pid, SIGUSR1, war);
    return 0;
}