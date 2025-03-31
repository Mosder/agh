#include <signal.h>
#include <stdio.h>
#include <string.h>
#define NONE 0
#define IGNORE 1
#define HANDLER 2
#define MASK 3

int hash_arg(char *arg) {
    if (strcmp(arg, "none") == 0) return NONE;
    else if (strcmp(arg, "ignore") == 0) return IGNORE;
    else if (strcmp(arg, "handler") == 0) return HANDLER;
    else if (strcmp(arg, "mask") == 0) return MASK;
    else return -1;
}

void signal_handler() {
    printf("Otrzymano sygnał SIGUSR1\n");
}

void mask_sginal() {
    sigset_t newmask, oldmask;
    sigemptyset(&newmask);
    sigaddset(&newmask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &newmask, &oldmask);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 1;
    }
    int hashed_arg = hash_arg(argv[1]);
    switch (hashed_arg) {
        case NONE:
            break; 
        case IGNORE:
            signal(SIGUSR1, SIG_IGN);
            break; 
        case HANDLER:
            signal(SIGUSR1, signal_handler);
            break; 
        case MASK:
            mask_sginal();
            break; 
        default:
            printf("Nie rozpoznano argumentu.\nPoprawne argumenty: none, ignore, handler, mask\n");
            return 1;
    }
    raise(SIGUSR1);
    if (hashed_arg == MASK) {
        sigset_t set;
        sigpending(&set);
        if (sigismember(&set, SIGUSR1) == 1)
            printf("SIGUSR1 oczekuje\n");
    }
    return 0;
}