CC=gcc
CFLAGS=-Wall -std=c17

all: countdown

countdown: countdown.c
	$(CC) $(CFLAGS) -g countdown.c -o countdown

.PHONY: clean
clean:
	rm *.o countdown