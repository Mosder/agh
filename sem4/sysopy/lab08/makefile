CC=gcc
CFLAGS=-Wall

all: zad

zad: server.c printer.c client.c
	$(CC) $(CFLAGS) printer.c -o preinter
	$(CC) $(CFLAGS) client.c -o client

.PHONY: clean
clean:
	rm printer client