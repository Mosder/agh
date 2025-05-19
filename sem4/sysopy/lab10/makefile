CC=gcc
CFLAGS=-Wall

all: zad

lib-time: time/time.c
	$(CC) $(CFLAGS) -c time/time.c
	ar rcs time/libtime.a time.o
	rm time.o

lib-patient: lib-time patient/patient.c
	$(CC) $(CFLAGS) -c time/libtime.a patient/patient.c
	ar rcs patient/libpatient.a patient.o
	rm patient.o

lib-pharmacist: lib-time pharmacist/pharmacist.c
	$(CC) $(CFLAGS) -c time/libtime.a pharmacist/pharmacist.c
	ar rcs pharmacist/libpharmacist.a pharmacist.o
	rm pharmacist.o

lib-doctor: lib-time doctor/doctor.c
	$(CC) $(CFLAGS) -c time/libtime.a doctor/doctor.c
	ar rcs doctor/libdoctor.a doctor.o
	rm doctor.o

zad: main.c lib-patient lib-pharmacist lib-doctor
	$(CC) $(CFLAGS) main.c patient/libpatient.a pharmacist/libpharmacist.a doctor/libdoctor.a -o main

.PHONY: clean
clean:
	rm main patient/libpatient.a pharmacist/libpharmacist.a doctor/libdoctor.a time/libtime.a