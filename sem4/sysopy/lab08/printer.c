#include <stdio.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "info.h"

// read from buffer
void read_shift_buffer(sem_t *sem_buffer, char *buffer, char *dest) {
    sem_wait(sem_buffer);
    strncpy(dest, buffer, MESSAGE_LENGTH);
    memmove(buffer, buffer + MESSAGE_LENGTH, strlen(buffer) - MESSAGE_LENGTH + 1);
    sem_post(sem_buffer);
}

// print message one character by one
void print_message(char *message) {
    for (int i = 0; i < MESSAGE_LENGTH; i++) {
        for (int j = 0; j <= i; j++) {
            printf("%c", message[j]);
        }
        printf("\n");
        sleep(1);
    }
}

int main(void) {
    // open needed semaphores
    sem_t *sem_queue = sem_open(SEM_QUEUE_SPACE, O_RDWR, 0644, QUEUE_LENGTH);
    sem_t *sem_requests = sem_open(SEM_REQUEST_COUNT, O_RDWR, 0644, 0);
    sem_t *sem_buffer = sem_open(SEM_BUFFER_CHANGE, O_RDWR, 0644, 1);
    // open shared memory
    int memory_fd = shm_open(MEMORY_NAME, O_RDWR, 0644);
    char *buffer = (char*) mmap(NULL, MEMORY_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, memory_fd, 0);
    while(1) {
        // wait for requests
        sem_wait(sem_requests);
        // read and remove request from shared memory
        char message[MESSAGE_LENGTH];
        read_shift_buffer(sem_buffer, buffer, message);
        // add a queue space
        sem_post(sem_queue);
        // print given message
        print_message(message);
    }
    return 0;
}