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
void print_message(int printer_id, char *message) {
    for (int i = 0; i < MESSAGE_LENGTH; i++) {
        printf("printer %d: ", printer_id);
        for (int j = 0; j <= i; j++) {
            printf("%c", message[j]);
        }
        printf("\n");
        sleep(1);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    int printer_count = atoi(argv[1]);
    if (printer_count < 1) {
        printf("Printer count must be at least 1");
        return 1;
    }

    // create needed semaphores
    sem_unlink(SEM_QUEUE_SPACE);
    sem_unlink(SEM_REQUEST_COUNT);
    sem_unlink(SEM_BUFFER_CHANGE);
    sem_t *sem_queue = sem_open(SEM_QUEUE_SPACE, O_CREAT | O_RDWR, 0644, QUEUE_LENGTH);
    sem_t *sem_requests = sem_open(SEM_REQUEST_COUNT, O_CREAT | O_RDWR, 0644, 0);
    sem_t *sem_buffer = sem_open(SEM_BUFFER_CHANGE, O_CREAT | O_RDWR, 0644, 1);
    sem_close(sem_queue);
    sem_close(sem_requests);
    sem_close(sem_buffer);

    // create shared memory
    shm_unlink(MEMORY_NAME);
    int memory_fd = shm_open(MEMORY_NAME, O_CREAT | O_RDWR, 0644);
    ftruncate(memory_fd, MEMORY_SIZE);

    for (int i = 0; i < printer_count; i++) {
        if (fork() == 0) {
            int printer_id = i;
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
                print_message(printer_id, message);
            }
        }
    }

    while(1);
    return 0;
}