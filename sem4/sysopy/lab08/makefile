CC=gcc
CFLAGS=-Wall

all: zad

zad: printer.c client.c init.c
	$(CC) $(CFLAGS) printer.c -o printer
	$(CC) $(CFLAGS) client.c -o client
	$(CC) $(CFLAGS) init.c -o init

.PHONY: clean
clean:
	rm printer client init