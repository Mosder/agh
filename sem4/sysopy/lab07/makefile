CC=gcc
CFLAGS=-Wall

all: zad

zad: server.c client.c
	$(CC) $(CFLAGS) server.c -o server
	$(CC) $(CFLAGS) client.c -o client

.PHONY: clean
clean:
	rm server client