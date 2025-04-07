#include <unistd.h>
#include <stdio.h>
#include <signal.h>

#define PRINT_SIG_COUNT 1
#define PRINT_EVERY_SEC 2
#define IGNORE_CTRL_C 3
#define PRINT_CTRL_C 4
#define KILL 5

int received_signals = 0;
int number = 0;

void handle_ctrl_c() {
    printf("Wciśnięto CTRL+C\n");
    return;
}

void handler(int sign, siginfo_t *info, void *p3) {
    received_signals++;
    kill(info->si_pid, SIGUSR1);
    int mode = info->si_value.sival_int;
    if (mode != PRINT_EVERY_SEC) number = 0;

    switch(mode) {
        case PRINT_SIG_COUNT: {
            printf("Całkowita liczba otrzymanych sygnałów: %d\n", received_signals);
            break;
        }
        case PRINT_EVERY_SEC: {
            sigset_t newmask, oldmask, set;
            sigemptyset(&newmask);
            sigaddset(&newmask, SIGUSR1);
            sigprocmask(SIG_BLOCK, &newmask, &oldmask);
            while(1) {
                printf("Number: %d\n", number++);
                sleep(1);
                sigpending(&set);
                if (sigismember(&set, SIGUSR1)) {
                    sigprocmask(SIG_UNBLOCK, &newmask, &oldmask);
                    break;
                }
            }
            break;
        }
        case IGNORE_CTRL_C: {
            signal(SIGINT, SIG_IGN);
            break;
        }
        case PRINT_CTRL_C: {
            signal(SIGINT, handle_ctrl_c);
            break;
        }
        case KILL: {
            raise(SIGKILL);
            break;
        }
    }
    return;
}

int main(void) {
    printf("pid: %d\n", getpid());

    struct sigaction action;
    action.sa_sigaction = handler;
    action.sa_flags = SA_SIGINFO;
    sigaction(SIGUSR1, &action, NULL);

    while(1) pause();

    return 0;
}