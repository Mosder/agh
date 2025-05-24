#include "patient.h"

// go to hospital (1st time)
int go_to_hospital(int patient_id, int *patients_waiting) {
    // determine arrival eta
    int wait_time = PATIENT_ARRIVAL_TIME_RANGE_START + rand() % PATIENT_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_ARRIVAL_STRING, TM_TO_STRING(tmp), patient_id, wait_time);
    // wait
    sleep(wait_time);
    // lock variable
    // determine if there is space in hospital
    if (*patients_waiting == PATIENT_CAPACITY) return 0;
    return *patients_wating + 1;
}

// go to hospital (2nd+ time)
int take_a_stroll(int patient_id, int *patients_waiting) {
    // determine arrival eta
    int wait_time = PATIENT_ARRIVAL_TIME_RANGE_START + rand() % PATIENT_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_STROLL_STRING, TM_TO_STRING(tmp), patient_id, wait_time);
    // wait
    sleep(wait_time);
    // lock variable
    // determine if there is space in hospital
    if (*patients_waiting == PATIENT_CAPACITY) return 0;
    return *patients_wating + 1;
}

// sit in queue
void sit_in_queue(int patient_id, int *patients_waiting) {
    // increment patients waiting
    *patients_waiting += 1;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_WAITING_STRING, TM_TO_STRING(tmp), patient_id, *patients_waiting);
    // unlock variable
}

// wake up doctor
void wake_up_doctor(int patient_id) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_WAKE_DOCTOR_STRING, TM_TO_STRING(tmp), patient_id);
    // wake doctor
}

// go home after visit
void end_visit(int patient_id) {
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PATIENT_VISIT_END_STRING, TM_TO_STRING(tmp), patient_id);
}

void* init_patient(void *init_args) {
    patient_args *args = (patient_args*) init_args;
    // go to hospital (2-5)
    // if full take a stroll (2-5)
    // else if is not 3rd w8 for visit in queue
    // else (is 3rd) wake up the motherfucker
    // go home after visit
    return NULL;
}