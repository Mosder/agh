CC=gcc
CFLAGS=-Wall

all: example

example: ex_client.c ex_server.c
	$(CC) $(CFLAGS) ex_server.c -o ex_server
	$(CC) $(CFLAGS) ex_client.c -o ex_client

.PHONY: clean
clean:
	rm ex_client ex_server