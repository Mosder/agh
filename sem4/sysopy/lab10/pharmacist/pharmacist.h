// pharmacist strings
#define PHARMACIST_ARRIVAL_STRING TIME_PATTERN" - Farmaceuta(%d): idę do szpitala, będę za %d s\n"
#define PHARMACIST_WAITING_STRING TIME_PATTERN" - Farmaceuta(%d): czekam na opróżnienie apteczki\n"
#define PHARMACIST_WAKE_DOCTOR_STRING TIME_PATTERN" - Farmaceuta(%d): budzę lekarza\n"
#define PHARMACIST_DELIVER_STRING TIME_PATTERN" - Farmaceuta(%d): dostarczam leki\n"
#define PHARMACIST_FINISHED_STRING TIME_PATTERN" - Farmaceuta(%d): zakończyłem dostawę\n"

// pharmacist times
#define PHARMACIST_ARRIVAL_TIME_RANGE_START 5
#define PHARMACIST_ARRIVAL_TIME_RANGE_END 15
#define PHARMACIST_ARRIVAL_TIME_RANGE_LENGTH (PHARMACIST_ARRIVAL_TIME_RANGE_END - PHARMACIST_ARRIVAL_TIME_RANGE_START + 1)

extern void* init_pharmacist(void *init_args);
