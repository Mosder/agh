CC=gcc
CFLAGS=-Wall

all: zad

zad: printer.c client.c
	$(CC) $(CFLAGS) printer.c -o printer
	$(CC) $(CFLAGS) client.c -o client

.PHONY: clean
clean:
	rm printer client