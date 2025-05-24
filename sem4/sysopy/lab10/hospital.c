#include <stdio.h>

#include "common.h"
#include "patient/patient.h"
#include "pharmacist/pharmacist.h"
#include "doctor/doctor.h"

int main(int argc, char *argvp[]) {
    if (argc != 3) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 1;
    }
    patient_count = atoi(argv[1]);
    pharmacist_count = atoi(argv[2]);
    if (patient_count < 1 || pharmacist_count < 1) {
        printf("Liczba pacjentów i farmaceutów musi być dodatnia\n");
        return 2;
    }

    // start doctor thread
    pthread_t doctor_thread;
    doctor_args init_doctor_args;
    pthread_create(&doctor_thread, NULL, init_doctor, (void*) (&init_doctor_args));

    // start patient threads
    pthread_t patient_threads[patient_count];
    patient_args init_patient_args[patient_count];
    for (int i = 0; i < patient_count; i++) {
        pthread_create(patient_threads + i, NULL, init_patient, (void*) (init_patient_args + i));
    }

    // start patient threads
    pthread_t pharmacist_threads[pharmacist_count];
    pharmacist_args init_pharmacist_args[pharmacist_count];
    for (int i = 0; i < pharmacist_count; i++) {
        pthread_create(pharmacist_threads + i, NULL, init_pharmacist, (void*) (init_pharmacist_args + i));
    }

    // wait until finished
    while(1);
    return 0;
}