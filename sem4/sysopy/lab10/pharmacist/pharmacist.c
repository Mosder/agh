#include "../common.h"
#include "pharmacist.h"

// start delivery
int start_delivery(
    int pharmacist_id,
    int *pharmacist_waiting, pthread_mutex_t *pharmacist_waiting_mutex,
    int *available_cures, pthread_mutex_t *available_cures_mutex
) {
    // determine arrival eta
    int wait_time = PHARMACIST_ARRIVAL_TIME_RANGE_START + rand() % PHARMACIST_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PHARMACIST_ARRIVAL_STRING, TM_TO_STRING(tmp), pharmacist_id, wait_time);
    // wait
    sleep(wait_time);
    pthread_mutex_lock(available_cures_mutex);
    // if space in fak, check if other pharmacist waiting
    if (*available_cures < PATIENT_CAPACITY) {
        pthread_mutex_lock(pharmacist_waiting_mutex);
        int waiting = *pharmacist_waiting;
        // if nobody was waiting, this one is now waiting
        if (waiting == 0) {
            *pharmacist_waiting = 1;
        }
        pthread_mutex_unlock(pharmacist_waiting_mutex);
        // if nobody was waiting, then treat fak as empty
        return waiting == 0 ? 1 : 0;
    }
    return 0;
}

void wait_for_empty_fak(
    int pharmacist_id, int *available_cures,
    pthread_mutex_t *available_cures_mutex, pthread_cond_t *available_cures_cond,
    int *pharmacist_waiting, pthread_mutex_t *pharmacist_waiting_mutex
) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PHARMACIST_WAITING_STRING, TM_TO_STRING(tmp), pharmacist_id);
    // wait for empty fak and other pharmacists
    pthread_mutex_lock(pharmacist_waiting_mutex);
    while(*available_cures >= PATIENT_CAPACITY || *pharmacist_waiting == 1) {
        pthread_mutex_unlock(pharmacist_waiting_mutex);
        pthread_cond_wait(available_cures_cond, available_cures_mutex);
        pthread_mutex_lock(pharmacist_waiting_mutex);
    }
    // set the waiting variable
    *pharmacist_waiting = 1;
    pthread_mutex_unlock(pharmacist_waiting_mutex);
}

void pharmacist_wake_up_doctor(
    int pharmacist_id, int *doctor_task,
    pthread_mutex_t *doctor_task_mutex, pthread_cond_t *doctor_task_cond
) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PHARMACIST_WAKE_DOCTOR_STRING, TM_TO_STRING(tmp), pharmacist_id);
    // wake doctor
    *doctor_task = DOCTOR_REFILL;
    pthread_cond_broadcast(doctor_task_cond);
    pthread_mutex_unlock(doctor_task_mutex);
    // print information
    time_t timestamp2 = time(NULL);
    struct tm *tmp2 = localtime(&timestamp2);
    printf(PHARMACIST_DELIVER_STRING, TM_TO_STRING(tmp2), pharmacist_id);
}

// go home after visit
void go_home_pharmacist(int pharmacist_id) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PHARMACIST_FINISHED_STRING, TM_TO_STRING(tmp), pharmacist_id);
}

void* init_pharmacist(void *init_args) {
    int id = ((common_args_with_id*)init_args)->id;
    common_args *args = ((common_args_with_id*)init_args)->args;
    // delivery
    int is_fak_empty = start_delivery(id,
        args->pharmacist_waiting, args->pharmacist_waiting_mutex,
        args->available_cures, args->available_cures_mutex
    );
    // if fak not empty, wait until empty
    if (!is_fak_empty) {
        wait_for_empty_fak(
            id, args->available_cures,
            args->available_cures_mutex, args->available_cures_cond,
            args->pharmacist_waiting, args->pharmacist_waiting_mutex
        );
    }
    // unlock cures variable
    pthread_mutex_unlock(args->available_cures_mutex);
    // wait untill doctor is asleep
    pthread_mutex_lock(args->doctor_task_mutex);
    while(*args->doctor_task != DOCTOR_ASLEEP) {
        pthread_cond_wait(args->doctor_task_cond, args->doctor_task_mutex);
    }
    // remember cures before (to check if delivery is done)
    pthread_mutex_lock(args->available_cures_mutex);
    int available_cures_before = *args->available_cures;
    pthread_mutex_unlock(args->available_cures_mutex);
    // empty fak - wake up the doctor
    pharmacist_wake_up_doctor(id, args->doctor_task, args->doctor_task_mutex, args->doctor_task_cond);
    // wait untill delivery is over
    pthread_mutex_lock(args->available_cures_mutex);
    while (available_cures_before == *args->available_cures) {
        pthread_cond_wait(args->available_cures_cond, args->available_cures_mutex);
    }
    pthread_mutex_unlock(args->available_cures_mutex);
    // change waiting variable
    pthread_mutex_lock(args->pharmacist_waiting_mutex);
    *args->pharmacist_waiting = 0;
    pthread_mutex_unlock(args->pharmacist_waiting_mutex);
    // go home
    go_home_pharmacist(id);
    return NULL;
}