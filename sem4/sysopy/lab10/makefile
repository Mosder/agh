CC=gcc
CFLAGS=-Wall

all: zad

lib-patient: patient/patient.c
	$(CC) $(CFLAGS) -c patient/patient.c
	ar rcs patient/libpatient.a patient.o
	rm patient.o

lib-pharmacist: pharmacist/pharmacist.c
	$(CC) $(CFLAGS) -c pharmacist/pharmacist.c
	ar rcs pharmacist/libpharmacist.a pharmacist.o
	rm pharmacist.o

lib-doctor: doctor/doctor.c
	$(CC) $(CFLAGS) -c doctor/doctor.c
	ar rcs doctor/libdoctor.a doctor.o
	rm doctor.o

zad: hospital.c lib-patient lib-pharmacist lib-doctor
	$(CC) $(CFLAGS) hospital.c patient/libpatient.a pharmacist/libpharmacist.a doctor/libdoctor.a -o hospital

.PHONY: clean
clean:
	rm hospital patient/libpatient.a pharmacist/libpharmacist.a doctor/libdoctor.a