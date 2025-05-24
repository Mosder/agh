#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

// time stuff
#define TIME_PATTERN "[%d:%d:%d]"
#define TM_TO_STRING(tmp) (tmp->tm_hour), (tmp->tm_min), (tmp->tm_sec)

// hospital parameters
#define FIRST_AID_KIT_CAPACITY 6
#define PATIENT_CAPACITY 3
#define PHARMACIST_DELIVERY_SIZE (FIRST_AID_KIT_CAPACITY - (FIRST_AID_KIT_CAPACITY % PATIENT_CAPACITY))

// define doctor task
#define DOCTOR_ASLEEP 0
#define DOCTOR_CURE 1
#define DOCTOR_REFILL 2

// common args struct
typedef struct {
    int *patients_waiting;
    int patients_waiting_indices[PATIENT_CAPACITY];
    pthread_mutex_t *patients_waiting_mutex;
    int *patients_remaining;
    pthread_mutex_t *patients_remaining_mutex;
    pthread_cond_t *patients_remaining_cond;
    int *available_cures;
    pthread_mutex_t *available_cures_mutex;
    pthread_cond_t *available_cures_cond;
    int *pharmacist_waiting;
    pthread_mutex_t *pharmacist_waiting_mutex;
    int *doctor_task;
    pthread_mutex_t *doctor_task_mutex;
    pthread_cond_t *doctor_task_cond;
} common_args;

// common args with id struct
typedef struct {
    int id;
    common_args *args;
} common_args_with_id;
