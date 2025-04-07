CC=gcc
CFLAGS=-Wall

all: zad1 zad2

zad1: zad1.c
	$(CC) $(CFLAGS) zad1.c -o zad1

zad2: catcher.c sender.c
	$(CC) $(CFLAGS) catcher.c -o catcher
	$(CC) $(CFLAGS) sender.c -o sender

.PHONY: clean
clean:
	rm zad1 catcher sender