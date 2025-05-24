#include "../common.h"
#include "patient.h"

// go to hospital (1st time)
int go_to_hospital(int patient_id, int *patients_waiting, pthread_mutex_t *patients_waiting_mutex) {
    // determine arrival eta
    int wait_time = PATIENT_ARRIVAL_TIME_RANGE_START + rand() % PATIENT_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_ARRIVAL_STRING, TM_TO_STRING(tmp), patient_id, wait_time);
    // wait
    sleep(wait_time);
    // lock variable
    pthread_mutex_lock(patients_waiting_mutex);
    // determine if there is space in hospital
    //  return 0 if not
    //  return which patient this is elsewise
    if (*patients_waiting == PATIENT_CAPACITY) return 0;
    return *patients_waiting + 1;
}

// go to hospital (2nd+ time)
int take_a_stroll(int patient_id, int *patients_waiting, pthread_mutex_t *patients_waiting_mutex) {
    // determine arrival eta
    int wait_time = PATIENT_ARRIVAL_TIME_RANGE_START + rand() % PATIENT_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_STROLL_STRING, TM_TO_STRING(tmp), patient_id, wait_time);
    // wait
    sleep(wait_time);
    // lock variable
    pthread_mutex_lock(patients_waiting_mutex);
    // determine if there is space in hospital
    //  return 0 if not
    //  return which patient this is elsewise
    if (*patients_waiting == PATIENT_CAPACITY) return 0;
    return *patients_waiting + 1;
}

// sit in queue
void sit_in_queue(int patient_id, int *patients_waiting, int *patients_waiting_indices, pthread_mutex_t *patients_waiting_mutex) {
    // set patient id
    patients_waiting_indices[*patients_waiting] = patient_id;
    // increment patients waiting
    *patients_waiting += 1;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_WAITING_STRING, TM_TO_STRING(tmp), patient_id, *patients_waiting);
    // unlock variable
    pthread_mutex_unlock(patients_waiting_mutex);
}

// wake up doctor
void patient_wake_up_doctor(
    int patient_id, int patients_to_cure,
    int *available_cures, pthread_mutex_t *available_cures_mutex, pthread_cond_t *available_cures_cond,
    int *doctor_task, pthread_mutex_t *doctor_task_mutex, pthread_cond_t *doctor_task_cond
) {
    // wait until enough cures in first aid kit
    pthread_mutex_lock(available_cures_mutex);
    while (*available_cures < patients_to_cure) {
        pthread_cond_wait(available_cures_cond, available_cures_mutex);
    }
    pthread_mutex_unlock(available_cures_mutex);
    // wait until doctor is asleep (not busy)
    pthread_mutex_lock(doctor_task_mutex);
    while (*doctor_task != DOCTOR_ASLEEP) {
        pthread_cond_wait(doctor_task_cond, doctor_task_mutex);
    }
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_WAKE_DOCTOR_STRING, TM_TO_STRING(tmp), patient_id);
    // wake doctor
    *doctor_task = DOCTOR_CURE;
    pthread_cond_broadcast(doctor_task_cond);
    pthread_mutex_unlock(doctor_task_mutex);
}

// go home after visit
void go_home_patient(int patient_id) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_VISIT_END_STRING, TM_TO_STRING(tmp), patient_id);
}

void* init_patient(void *init_args) {
    int id = ((common_args_with_id*)init_args)->id;
    common_args *args = ((common_args_with_id*)init_args)->args;
    // go to hospital
    int result = go_to_hospital(id, args->patients_waiting, args->patients_waiting_mutex);
    // while there is no space, take a stroll
    while(result == 0) {
        // unlock variable
        pthread_mutex_unlock(args->patients_waiting_mutex);
        // take a stroll
        result = take_a_stroll(id, args->patients_waiting, args->patients_waiting_mutex);
    }
    // if there is space, sit in queue
    sit_in_queue(id, args->patients_waiting, args->patients_waiting_indices, args->patients_waiting_mutex);
    // remember patients_remaining before visit (to know when visit ends)
    pthread_mutex_lock(args->patients_remaining_mutex);
    int patients_remaining_before = *args->patients_remaining;
    pthread_mutex_unlock(args->patients_remaining_mutex);
    // if max hospital capacity or last patient for the day, wake up doctor
    if (result == PATIENT_CAPACITY) {
        patient_wake_up_doctor(
            id, result,
            args->available_cures, args->available_cures_mutex, args->available_cures_cond,
            args->doctor_task, args->doctor_task_mutex, args->doctor_task_cond
        );
    }
    // split conditions to not lock mutex for no reason
    else {
        // check number of remaining patients
        pthread_mutex_lock(args->patients_remaining_mutex);
        int can_wake_up = result == *args->patients_remaining ? 1 : 0;
        pthread_mutex_unlock(args->patients_remaining_mutex);
        if (can_wake_up) {
            patient_wake_up_doctor(
                id, result,
                args->available_cures, args->available_cures_mutex, args->available_cures_cond,
                args->doctor_task, args->doctor_task_mutex, args->doctor_task_cond
            );
        }
    }
    // wait till the end of the visit
    pthread_mutex_lock(args->patients_remaining_mutex);
    while (patients_remaining_before == *args->patients_remaining) {
        pthread_cond_wait(args->patients_remaining_cond, args->patients_remaining_mutex);
    }
    pthread_mutex_unlock(args->patients_remaining_mutex);
    // go home after visit
    go_home_patient(id);
    return NULL;
}