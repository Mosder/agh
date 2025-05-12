#include <stdio.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "info.h"

int main(void) {
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
}