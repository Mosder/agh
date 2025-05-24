#include "../common.h"

// doctor strings
#define DOCTOR_WAKE_UP_STRING TIME_PATTERN" - Lekarz: budzę się\n"
#define DOCTOR_VISIT_STRING TIME_PATTERN" - Lekarz: konsultuję pacjentów %d, %d, %d\n"
#define DOCTOR_DELIVERY_STRING TIME_PATTERN" - Lekarz: przyjmuję dostawę leków\n"
#define DOCTOR_SLEEP_STRING TIME_PATTERN" - Lekarz: zasypiam\n"

// doctor times
#define DOCTOR_VISIT_TIME_RANGE_START 2
#define DOCTOR_VISIT_TIME_RANGE_END 4
#define DOCTOR_VISIT_TIME_RANGE_LENGTH (DOCTOR_VISIT_TIME_RANGE_END - DOCTOR_VISIT_TIME_RANGE_START + 1)
#define DOCTOR_DELIVERY_TIME_RANGE_START 1
#define DOCTOR_DELIVERY_TIME_RANGE_END 3
#define DOCTOR_DELIVERY_TIME_RANGE_LENGTH (DOCTOR_DELIVERY_TIME_RANGE_END - DOCTOR_DELIVERY_TIME_RANGE_START + 1)

extern void* init_doctor(void *init_args);
