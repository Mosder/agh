#include "../common.h"

// patient strings
#define PATIENT_ARRIVAL_STRING TIME_PATTERN" - Pacjent(%d): idę do szpitala, będę za %d s\n"
#define PATIENT_STROLL_STRING TIME_PATTERN" - Pacjent(%d): za dużo pacjentów, wracam później za %d s\n"
#define PATIENT_WAITING_STRING TIME_PATTERN" - Pacjent(%d): czeka %d pacjentów na lekarza\n"
#define PATIENT_WAKE_DOCTOR_STRING TIME_PATTERN" - Pacjent(%d): budzę lekarza\n"
#define PATIENT_VISIT_END_STRING TIME_PATTERN" - Pacjent(%d): kończę wizytę\n"

// patient times
#define PATIENT_ARRIVAL_TIME_RANGE_START 2
#define PATIENT_ARRIVAL_TIME_RANGE_END 5
#define PATIENT_ARRIVAL_TIME_RANGE_LENGTH (PATIENT_ARRIVAL_TIME_RANGE_END - PATIENT_ARRIVAL_TIME_RANGE_START + 1)

extern void* init_patient(void *init_args);
