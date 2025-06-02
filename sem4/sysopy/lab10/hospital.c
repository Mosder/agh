#include "common.h"
#include "patient/patient.h"
#include "pharmacist/pharmacist.h"
#include "doctor/doctor.h"

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Nieprawidłowa liczba argumentów\n");
        return 1;
    }
    int patient_count = atoi(argv[1]);
    int pharmacist_count = atoi(argv[2]);
    if (patient_count < 1 || pharmacist_count < 1) {
        printf("Liczba pacjentów i farmaceutów musi być dodatnia\n");
        return 2;
    }

    // if not all patients can be cured, cancel visits for excess
    int excess_patients = patient_count - (FIRST_AID_KIT_CAPACITY + pharmacist_count*PHARMACIST_DELIVERY_SIZE);
    if (excess_patients > 0) {
        printf("Zbyt mało dostaw leków, by obsłużyć wszystkich %d pacjentów.\n", patient_count);
        printf("Odwołuję wizytę %d pacjentom.\n", excess_patients);
        patient_count -= excess_patients;
    }

    // initialize variables
    int patients_waiting = 0;
    pthread_mutex_t patients_waiting_mutex = PTHREAD_MUTEX_INITIALIZER;
    int patients_remaining = patient_count;
    pthread_mutex_t patients_remaining_mutex = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t patients_remaining_cond = PTHREAD_COND_INITIALIZER;
    int available_cures = FIRST_AID_KIT_CAPACITY;
    pthread_mutex_t available_cures_mutex = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t available_cures_cond = PTHREAD_COND_INITIALIZER;
    int pharmacist_waiting = 0;
    pthread_mutex_t pharmacist_waiting_mutex = PTHREAD_MUTEX_INITIALIZER;
    int doctor_task = DOCTOR_ASLEEP;
    pthread_mutex_t doctor_task_mutex = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t doctor_task_cond = PTHREAD_COND_INITIALIZER;

    // define common args
    common_args args;
    args.patients_waiting = &patients_waiting;
    args.patients_waiting_mutex = &patients_waiting_mutex;
    args.patients_remaining = &patients_remaining;
    args.patients_remaining_mutex = &patients_remaining_mutex;
    args.patients_remaining_cond = &patients_remaining_cond;
    args.available_cures = &available_cures;
    args.available_cures_mutex = &available_cures_mutex;
    args.available_cures_cond = &available_cures_cond;
    args.pharmacist_waiting = &pharmacist_waiting;
    args.pharmacist_waiting_mutex = &pharmacist_waiting_mutex;
    args.doctor_task = &doctor_task;
    args.doctor_task_mutex = &doctor_task_mutex;
    args.doctor_task_cond = &doctor_task_cond;

    // start doctor thread
    pthread_t doctor_thread;
    pthread_create(&doctor_thread, NULL, init_doctor, (void*) (&args));

    // start patient threads
    pthread_t patient_threads[patient_count];
    common_args_with_id patient_args[patient_count];
    for (int i = 0; i < patient_count; i++) {
        patient_args[i].id = i+1;
        patient_args[i].args = &args;
        pthread_create(patient_threads + i, NULL, init_patient, (void*) (patient_args + i));
    }

    // start patient threads
    pthread_t pharmacist_threads[pharmacist_count];
    common_args_with_id pharmacist_args[pharmacist_count];
    for (int i = 0; i < pharmacist_count; i++) {
        pharmacist_args[i].id = i+1;
        pharmacist_args[i].args = &args;
        pthread_create(pharmacist_threads + i, NULL, init_pharmacist, (void*) (pharmacist_args + i));
    }

    // wait until finished
    pthread_mutex_lock(&patients_remaining_mutex);
    while(patients_remaining > 0) {
        pthread_cond_wait(&patients_remaining_cond, &patients_remaining_mutex);
    }
    pthread_mutex_unlock(&patients_remaining_mutex);
    // print information
    time_t timestamp = time(NULL);
    struct tm *tmp = localtime(&timestamp);
    printf(TIME_PATTERN" - Wszyscy pacjenci zostali uleczeni\n", TM_TO_STRING(tmp));
    return 0;
}