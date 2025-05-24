#include "doctor.h"

// time_t timestamp = time(NULL);
// struct tm *tmp = localtime(&timestamp);
// printf(DOCTOR_WAKE_UP_STRING, TM_TO_STRING(tmp));
void* init_doctor(void *init_args) {
    doctor_args *args = (doctor_args*) init_args;
    // sleep until 3 patients and cures (or less when last patients or no more pharmacists)
    //      or when pharmacist is w8ing and less than 3 cures
    // wake up
    // patients: consult, use 3 cures (2-4s)
    // pharmacist: fill fak (1-3s)
    // after task - go to sleep (honk shoo/mimimi)
    return NULL;
}