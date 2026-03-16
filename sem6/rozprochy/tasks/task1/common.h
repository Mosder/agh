#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>

#define DEFAULT_ADDRESS "127.0.0.1"
#define DEFAULT_PORT 8000

#define MAX_NAME_LENGTH 50
#define MAX_MESSAGE_LENGTH 10000

#define PING_MESSAGE "PING"
#define PONG_MESSAGE "ALIVE"
#define UDP_ADDRESS_REQUEST "UDPREQ"
#define UDP_ADDRESS_RESPONSE "UDPRESP"
#define SERVER_DEAD_MESSAGE "SERVER_DEAD"

#define COMMAND_LIST "LIST"
#define COMMAND_2ALL "2ALL"
#define COMMAND_2ONE "2ONE"
#define COMMAND_STOP "STOP"

