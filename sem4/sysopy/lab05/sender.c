#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

void handle_response() {
    printf("Potwierdzono odebranie sygnału\n");
    return;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 1;
    }

    int catcher_pid = atoi(argv[1]);
    int mode = atoi(argv[2]);

    if (catcher_pid == 0 || mode < 1 || mode > 5) {
        printf("Otrzymano nieprawidłowe argumenty\n");
        return 1;
    }

    signal(SIGUSR1, handle_response);

    union sigval message;
    message.sival_int = mode;
    sigqueue(catcher_pid, SIGUSR1, message);

    pause();
    return 0;
}