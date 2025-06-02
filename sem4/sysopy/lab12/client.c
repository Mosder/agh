#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>

#define MAX_NAME_LENGTH 50
#define MAX_MESSAGE_LENGTH 1000

int sock;

// leave the server
void client_leave() {
    send(sock, "STOP\n", 5, MSG_DONTWAIT);
    close(sock);
    raise(SIGKILL);
}

// handle receiving messages
void* receive_handler(void *args) {
    while (1) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(message, 0, sizeof(message));
        read(sock, message, MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH);
        // handle ping
        if (strcmp(message, "PING") == 0) send(sock, "ALIVE", 6, MSG_DONTWAIT);
        // else, print received message
        else printf("%s", message);
    }
    return NULL;
}

// handle sending messages
void* send_handler(void *args) {
    while(1) {
        char buffer[MAX_MESSAGE_LENGTH];
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(buffer, 0, sizeof(buffer));
        memset(message, 0, sizeof(message));

        fgets(buffer, MAX_MESSAGE_LENGTH, stdin);

        // LIST
        if (strncmp(buffer, "/list", 5) == 0) {
            sprintf(message, "LIST\n");
        }
        // 2ONE
        else if (strncmp(buffer, "/msg", 4) == 0) {
            sprintf(message, "2ONE %s", buffer + 5);
        }
        // STOP
        else if (strncmp(buffer, "/stop", 5) == 0) {
            client_leave();
            break;
        }
        // 2ALL
        else {
            sprintf(message, "2ALL %s", buffer);
        }

        send(sock, message, strlen(message), MSG_DONTWAIT);
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("Incorrect number of arguments\n");
        return 1;
    }
    char *name = argv[1];
    char *ip = argv[2];
    int port = atoi(argv[3]);

    // handle ctrl-c
    signal(SIGINT, client_leave);

    struct sockaddr_in server_address;

    // socket
    sock = socket(AF_INET, SOCK_STREAM, 0);
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    server_address.sin_addr.s_addr = inet_addr(ip);
    connect(sock, (struct sockaddr *) &server_address, sizeof(server_address));

    // send name
    send(sock, name, MAX_NAME_LENGTH, MSG_DONTWAIT);

    printf("Connected to server\n");
    printf("Possible actions:\n");
    printf(" - LIST: /list\n");
    printf(" - 2ALL: <message>\n");
    printf(" - 2ONE: /msg <name> <message>\n");
    printf(" - STOP: /stop\n");

    // start receiving thread
    pthread_t receive_thread;
    pthread_create(&receive_thread, NULL, receive_handler, NULL);

    // start sending thread
    pthread_t send_thread;
    pthread_create(&send_thread, NULL, send_handler, NULL);

    // wait until all threads terminate
    pthread_join(receive_thread, NULL);
    pthread_join(send_thread, NULL);

    close(sock);
    return 0;
}