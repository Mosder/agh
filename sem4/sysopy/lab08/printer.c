#include <stdio.h>
#include <string.h>

#include "printer_info.h"

void print_message(char *message) {
    for (int i = 0; i < MESSAGE_LENGTH; i++) {
        printf("%c", message[i]);
        fflush(stdout);
        sleep(1);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    int printer_count = atoi(printer_count);
    if (printer_count < 1) {
        printf("Printer count must be at least 1");
        return 1;
    }
}