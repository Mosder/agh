#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_NAME_LENGTH 50
#define MAX_MESSAGE_LENGTH 1000
#define MAX_CLIENTS 20

typedef struct {
    struct sockaddr_in address;
    char name[MAX_NAME_LENGTH];
    int alive;
} client_info;

int server_socket;
client_info *clients[MAX_CLIENTS];
pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;

// ping client to check if they're alive
void ping_clients() {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            sendto(server_socket, "PING", 5, 0, (struct sockaddr*)&clients[i]->address, sizeof(clients[i]->address));
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// check if clients are alive
void check_responses() {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (clients[i]->alive) clients[i]->alive = 0;
            else clients[i] = NULL;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// ping clients every 3s to check if they're alive
void* handle_pinging(void *args) {
    while (1) {
        ping_clients();
        sleep(3);
        check_responses();
    }
    return NULL;
}

// add new client info to the clients array
void add_client(client_info *info) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (!clients[i]) {
            clients[i] = info;
            break;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// get a list of users
void get_users(char *str) {
    strcpy(str, "Users:\n");
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            strcat(str, " - ");
            strcat(str, clients[i]->name);
            strcat(str, "\n");
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// send message to all users
void send_to_all(char *message, client_info *sender) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] && clients[i] != sender) {
            sendto(server_socket, message, strlen(message), 0, (struct sockaddr*)&clients[i]->address, sizeof(clients[i]->address));
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// send message to one user
void send_to_one(char *message, char *recipient) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (strcmp(clients[i]->name, recipient) == 0) {
                sendto(server_socket, message, strlen(message), 0, (struct sockaddr*)&clients[i]->address, sizeof(clients[i]->address));
                break;
            }
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// remove client from clients array
void remove_client(client_info *info) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] && clients[i] == info) {
            printf("Client %s removed\n", clients[i]->name);
            clients[i] = NULL;
            break;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// confirm ping response from client
void confirm_response(client_info *info) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] && clients[i] == info) {
            clients[i]->alive = 1;
            break;
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// handle client requests
void handle_request(client_info *info, char* buffer) {
    // LIST
    if (strncmp(buffer, "LIST", 4) == 0) {
        char users[MAX_MESSAGE_LENGTH];
        get_users(users);
        sendto(server_socket, users, strlen(users), 0, (struct sockaddr*)&info->address, sizeof(info->address));
    }
    // 2ALL
    else if(strncmp(buffer, "2ALL", 4) == 0) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        sprintf(message, "[%s]: %s", info->name, buffer + 5);
        send_to_all(message, info);
    }
    // 2ONE
    else if(strncmp(buffer, "2ONE", 4) == 0) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        char *recipient = strtok(buffer+5, " ");
        char *content = strtok(NULL, " ");
        sprintf(message, "(whisper) [%s]: %s", info->name, content);
        send_to_one(message, recipient);
    }
    // STOP
    else if(strncmp(buffer, "STOP", 4) == 0) {
        remove_client(info);
        free(info);
    }
    // ALIVE
    else if(strncmp(buffer, "ALIVE", 5) == 0) {
        confirm_response(info);
    }
}

// count clients
int client_count() {
    int count = 0;
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) count++;
    }
    pthread_mutex_unlock(&clients_mutex);
    return count;
}

// check if client exists, if so, return info, else NULL
client_info* get_client_info(const struct sockaddr_in *address) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (memcmp(&clients[i]->address, address, sizeof(struct sockaddr_in)) == 0) {
                pthread_mutex_unlock(&clients_mutex);
                return clients[i];
            }
        }
    }
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    char *ip = argv[1];
    int port = atoi(argv[2]);

    struct sockaddr_in server_address, client_address;

    // server socket
    server_socket = socket(AF_INET, SOCK_DGRAM, 0);
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    server_address.sin_addr.s_addr = inet_addr(ip);
    // reuse address option
    int option = 1;
    setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
    bind(server_socket, (struct sockaddr *) &server_address, sizeof(server_address));
    printf("Server started\n");

    // pinging thread
    pthread_t pinging_thread;
    pthread_create(&pinging_thread, NULL, &handle_pinging, NULL);

    while(1) {
        char buffer[MAX_MESSAGE_LENGTH];
        memset(buffer, 0, MAX_MESSAGE_LENGTH);
        int client_len = sizeof(client_address);
        recvfrom(server_socket, buffer, MAX_MESSAGE_LENGTH, 0, (struct sockaddr*)&client_address, (socklen_t*)&client_len);

        // if client exists, handle request
        client_info *info = get_client_info(&client_address);
        if (info) {
            handle_request(info, buffer);
            continue;
        }

        // if no space, don't add the new client
        if (client_count() >= MAX_CLIENTS) {
            printf("Couldn't accept client - max count reached.\n");
            continue;
        }

        // create new client
        client_info *new_info = (client_info*)malloc(sizeof(client_info));
        new_info->address = client_address;
        new_info->alive = 1;
        strcpy(new_info->name, buffer);
        add_client(new_info);
        sendto(server_socket, "ACCEPT", 7, 0, (struct sockaddr*)&new_info->address, sizeof(new_info->address));
        printf("Added new client - %s\n", new_info->name);
    }

    return 0;
}