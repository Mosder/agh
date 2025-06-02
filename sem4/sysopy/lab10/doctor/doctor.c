#include "../common.h"
#include "doctor.h"

void wake_up() {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(DOCTOR_WAKE_UP_STRING, TM_TO_STRING(tmp));
}

int cure(
    int *patients_waiting, int *patients_waiting_indices, pthread_mutex_t *patients_waiting_mutex,
    int *available_cures, pthread_mutex_t *available_cures_mutex, pthread_cond_t *available_cures_cond,
    int *patients_remaining, pthread_mutex_t *patients_remaining_mutex, pthread_cond_t *patients_remaining_cond
) {
    pthread_mutex_lock(patients_waiting_mutex);
    int patients_to_cure = *patients_waiting;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(DOCTOR_VISIT_STRING, TM_TO_STRING(tmp), patients_waiting_indices[0]);
    for (int i = 1; i < patients_to_cure; i++) {
        printf(", %d", patients_waiting_indices[i]);
    }
    printf("\n");
    pthread_mutex_unlock(patients_waiting_mutex);
    // wait (do consultation)
    sleep(DOCTOR_VISIT_TIME_RANGE_START + rand() % DOCTOR_VISIT_TIME_RANGE_LENGTH);
    // use cures
    pthread_mutex_lock(available_cures_mutex);
    *available_cures -= patients_to_cure;
    // if cures running low - signal pharmacist
    if (*available_cures < PATIENT_CAPACITY) {
        pthread_cond_signal(available_cures_cond);
    }
    pthread_mutex_unlock(available_cures_mutex);
    // reduce remaining patients
    pthread_mutex_lock(patients_remaining_mutex);
    *patients_remaining -= patients_to_cure;
    // check if all cured
    int all_cured = *patients_remaining == 0 ? 1 : 0;
    // broadcast change
    pthread_cond_broadcast(patients_remaining_cond);
    pthread_mutex_unlock(patients_remaining_mutex);
    // clear patients queue
    pthread_mutex_lock(patients_waiting_mutex);
    *patients_waiting = 0;
    pthread_mutex_unlock(patients_waiting_mutex);
    // return information if all cured
    return all_cured;
}

void refill(int *available_cures, pthread_mutex_t *available_cures_mutex, pthread_cond_t *available_cures_cond) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(DOCTOR_DELIVERY_STRING, TM_TO_STRING(tmp));
    // wait (accept delivery)
    sleep(DOCTOR_DELIVERY_TIME_RANGE_START + rand() % DOCTOR_DELIVERY_TIME_RANGE_LENGTH);
    // fill fak
    pthread_mutex_lock(available_cures_mutex);
    *available_cures = FIRST_AID_KIT_CAPACITY;
    // broadcast the fill information
    pthread_cond_broadcast(available_cures_cond);
    pthread_mutex_unlock(available_cures_mutex);
}

void fall_asleep(int *doctor_task, pthread_mutex_t *doctor_task_mutex, pthread_cond_t *doctor_task_cond) {
    pthread_mutex_lock(doctor_task_mutex);
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(DOCTOR_SLEEP_STRING, TM_TO_STRING(tmp));
    // set task
    *doctor_task = DOCTOR_ASLEEP;
    pthread_cond_broadcast(doctor_task_cond);
}

void* init_doctor(void *init_args) {
    common_args *args = (common_args*)init_args;
    pthread_mutex_lock(args->doctor_task_mutex);
    while(1) {
        // sleep untill woken up
        while (*args->doctor_task == DOCTOR_ASLEEP) {
            pthread_cond_wait(args->doctor_task_cond, args->doctor_task_mutex);
        }
        int task = *args->doctor_task;
        pthread_mutex_unlock(args->doctor_task_mutex);
        // wake up
        wake_up();
        // do given task
        if (task == DOCTOR_CURE) {
            // end work when all cured
            int all_cured = cure(
                args->patients_waiting, args->patients_waiting_indices, args->patients_waiting_mutex,
                args->available_cures, args->available_cures_mutex, args->available_cures_cond,
                args->patients_remaining, args->patients_remaining_mutex, args->patients_remaining_cond
            );
            if(all_cured) return NULL;
        }
        else if (task == DOCTOR_REFILL) {
            refill(args->available_cures, args->available_cures_mutex, args->available_cures_cond);
        }
        // go back to sleep
        fall_asleep(args->doctor_task, args->doctor_task_mutex, args->doctor_task_cond);
    }
    return NULL;
}