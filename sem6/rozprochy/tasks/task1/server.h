#include "common.h"

#define MAX_CLIENTS 1024

typedef struct {
    struct sockaddr_in address;
    int socket;
    struct sockaddr_in udp_address;
	int id;
    char name[MAX_NAME_LENGTH];
    int alive;
} client_info;

typedef struct {
	char ip[1000];
	int port;
} address_info;

#define SERVER_NAME "[SERVER]"
#define CLIENT_NAME_FORMAT "[%s (%d)]"

#define INIT_MESSAGE_FORMAT "Server started on %s:%d\n"
#define CLIENT_CONNECTED_MESSAGE_FORMAT "Client %s (%d) connected\n"
#define CLIENT_DISCONNECTED_MESSAGE_FORMAT "Client %s (%d) disconnected\n"
#define FULL_CHAT_MESSAGE "Couldn't accept client - chat is full\n"

#define USERS_LIST_LINE_FORMAT " - %s (%d)\n"
#define PV_INDICATOR "(whisper)"

