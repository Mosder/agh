#include <stdio.h>

#include "patient/patient.h"
#include "pharmacist/pharmacist.h"
#include "doctor/doctor.h"

int main(void) {
    printf("%d %d %d\n", patient(), pharmacist(), doctor());
}