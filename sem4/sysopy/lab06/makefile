CC=gcc
CFLAGS=-Wall

all: zad1 zad2

zad1: zad1.c
	$(CC) $(CFLAGS) zad1.c -o zad1

zad2: main.c calc.c
	$(CC) $(CFLAGS) main.c -o main
	$(CC) $(CFLAGS) calc.c -o calc

.PHONY: clean
clean:
	rm zad1 main calc pipe-*