CC=gcc
CFLAGS=-Wall

all: flipper

flipper: flipper.c
	$(CC) $(CFLAGS) flipper.c -o flipper

.PHONY: clean
clean:
	rm flipper
	ls -d */ | grep -v ^art/$ | xargs rm -rf