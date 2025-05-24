#include "pharmacist.h"

// start delivery
int start_delivery(int pharmacist_id, int available_cures) {
    // determine arrival eta
    int wait_time = PHARMACIST_ARRIVAL_TIME_RANGE_START + rand() % PHARMACIST_ARRIVAL_TIME_RANGE_LENGTH;
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(PHARMACIST_ARRIVAL_STRING, TM_TO_STRING(tmp), patient_id, wait_time);
    // wait
    sleep(wait_time);
    // lock variable
    // determine if there is space in fak
    if (*available_cures >= PATIENT_CAPACITY) return 0;
    return 1;
}

void* init_pharmacist(void *init_args) {
    pharmacist_args *args = (pharmacist_args*) init_args;
    // deliver (5-15s)
    // full fak - w8 till empty (or below 3)
    // empty (or below 3) fak - wake up the wicked
    // finish the delivery
    // fuck off
    return NULL;
}