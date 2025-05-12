#include <stdio.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include "info.h"

// generate random message of random characters
void generate_message(char *message) {
    for (int i = 0; i < MESSAGE_LENGTH; i++) {
        message[i] = rand() % ALPHABET_LENGTH + ALPHABET_START;
    }
}

// write to buffer
void write_to_buffer(sem_t *sem_buffer, char *buffer, char *source) {
    sem_wait(sem_buffer);
    strcat(buffer, source);
    sem_post(sem_buffer);
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
        // set random seed
        srand(time(NULL));
        // wait until queue is not full
        sem_wait(sem_queue);
        // generate message and write it to buffer
        char message[MESSAGE_LENGTH];
        generate_message(message);
        write_to_buffer(sem_buffer, buffer, message);
        // add a request and announce it
        sem_post(sem_requests);
        printf("request: %s\n", message);
        // wait to send another request
        sleep(SLEEP_BASE_LENGTH + rand() % SLEEP_INCREASE_MAX);
    }
    return 0;
}