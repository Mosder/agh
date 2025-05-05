#include <stdio.h>
#include <stdlib.h>

#include "printer_info.h"

char* generate_message() {
    char message[MESSAGE_LENGTH];
    for (int i = 0; i < MESSAGE_LENGTH; i++) {
        message[i] = rand() % ALPHABET_LENGTH + ALPHABET_START;
    }
    return message;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    int client_count = atoi(client_count);
    if (client_count < 1) {
        printf("Client count must be at least 1");
        return 1;
    }
}