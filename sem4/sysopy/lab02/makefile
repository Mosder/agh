CC=gcc
CFLAGS=-Wall -std=c17

all: static shared dll

lib-static: collatz.c
	$(CC) $(CFLAGS) -c collatz.c
	ar rcs libcollatz.a collatz.o

lib-shared: collatz.c
	$(CC) $(CFLAGS) -fPIC -c collatz.c
	$(CC) -shared collatz.o -o libcollatz.so

static: lib-static main.c
	$(CC) $(CFLAGS) main.c libcollatz.a -o main-static

shared: lib-shared main.c
	$(CC) $(CFLAGS) main.c -lcollatz -L./ -Wl,-rpath,. -o main-shared

dll: lib-shared main.c
	$(CC) $(CFLAGS) main.c -D DLL -o main-dll

.PHONY: clean
clean:
	rm *.o *.a *.so main-*