#include "doctor.h"
#include <stdio.h>

int doctor() {
    struct tm *tmp = get_tm();
    printf(DOCTOR_WAKE_UP_STRING, TM_TO_STRING(tmp));
    return 1;
}