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
    int socket;
    char name[MAX_NAME_LENGTH];
    int alive;
} client_info;

client_info *clients[MAX_CLIENTS];
pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;

// ping client to check if they're alive
void ping_clients() {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            write(clients[i]->socket, "PING", 5);
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
            write(clients[i]->socket, message, strlen(message));
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
                write(clients[i]->socket, message, strlen(message));
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
void* handle_client(void *args) {
    client_info *info = (client_info*)args;
    while(1) {
        char buffer[MAX_MESSAGE_LENGTH];
        memset(buffer, 0, sizeof(buffer));
        read(info->socket, buffer, MAX_MESSAGE_LENGTH);
        // LIST
        if (strncmp(buffer, "LIST", 4) == 0) {
            char users[MAX_MESSAGE_LENGTH];
            get_users(users);
            write(info->socket, users, strlen(users));
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
            close(info->socket);
            free(info);
            break;
        }
        // ALIVE
        else if(strncmp(buffer, "ALIVE", 5) == 0) {
            confirm_response(info);
        }
    }
    return NULL;
}

int client_count() {
    int count = 0;
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) count++;
    }
    pthread_mutex_unlock(&clients_mutex);
    return count;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    char *ip = argv[1];
    int port = atoi(argv[2]);

    int server_socket, client_socket;
    struct sockaddr_in server_address, client_address;

    // server socket
    server_socket = socket(AF_INET, SOCK_STREAM, 0);
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    server_address.sin_addr.s_addr = inet_addr(ip);
    // reuse address option
    int option = 1;
    setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
    bind(server_socket, (struct sockaddr *) &server_address, sizeof(server_address));
    listen(server_socket, MAX_CLIENTS);
    printf("Server started\n");

    // pinging thread
    pthread_t pinging_thread;
    pthread_create(&pinging_thread, NULL, &handle_pinging, NULL);

    while(1) {
        int client_len = sizeof(client_address);
        client_socket = accept(server_socket, (struct sockaddr*)&client_address, (socklen_t*)&client_len);

        // if no space, don't add the new client
        if (client_count() >= MAX_CLIENTS) {
            printf("Couldn't accept client - max count reached.\n");
            close(client_socket);
            continue;
        }

        // create new client info
        client_info *info = (client_info*)malloc(sizeof(client_info));
        info->address = client_address;
        info->socket = client_socket;
        info->alive = 1;
        read(client_socket, info->name, MAX_NAME_LENGTH);

        // add client and create handling thread
        add_client(info);
        printf("Added new client - %s\n", info->name);
        pthread_t client_thread;
        pthread_create(&client_thread, NULL, &handle_client, (void*)info);
    }

    return 0;
}