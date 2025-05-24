#include <time.h>
#include <stdlib.h>
#include <stdio.h>

// time stuff
#define TIME_PATTERN "[%d:%d:%d]"
#define TM_TO_STRING(tmp) (tmp->tm_hour), (tmp->tm_min), (tmp->tm_sec)

// hospital parameters
#define FIRST_AID_KIT_CAPACITY 6
#define PATIENT_CAPACITY 3

// init doctor struct args
typedef struct {
    int something;
} doctor_args;

// init patient struct args
typedef struct {
    int patient_id;
} patient_args;

// init pharmacist struct args
typedef struct {
    int pharmacist_id;
} pharmacist_args;
