all: server client

.PHONY: .FORCE

server: server.c .FORCE
	gcc server.c -o server

client: client.c .FORCE
	gcc client.c -o client

.PHONY: clean
clean:
	rm client server
