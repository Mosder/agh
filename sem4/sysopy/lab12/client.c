#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <sys/select.h>

#define MAX_NAME_LENGTH 50
#define MAX_MESSAGE_LENGTH 1000

int sock;
struct sockaddr_in server_address;
int server_len;

// leave the server
void client_leave() {
    sendto(sock, "STOP\n", 5, MSG_DONTWAIT, (struct sockaddr*)&server_address, server_len);
    close(sock);
    raise(SIGKILL);
}

// handle receiving messages
void* receive_handler(void *args) {
    while (1) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(message, 0, sizeof(message));
        recvfrom(sock, message, MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH, 0, (struct sockaddr*)&server_address, (socklen_t*)&server_len);
        // handle ping
        if (strcmp(message, "PING") == 0) sendto(sock, "ALIVE", 6, MSG_DONTWAIT, (struct sockaddr*)&server_address, server_len);
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

        sendto(sock, message, strlen(message), MSG_DONTWAIT, (struct sockaddr*)&server_address, server_len);
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

    // socket
    sock = socket(AF_INET, SOCK_DGRAM, 0);
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    server_address.sin_addr.s_addr = inet_addr(ip);
    server_len = sizeof(server_address);

    // send name
    sendto(sock, name, strlen(name), 0, (struct sockaddr*)&server_address, server_len);

    // accept set
    fd_set fdaccept;
    FD_ZERO(&fdaccept);
    FD_SET(sock, &fdaccept);

    // wait 2 secs for response
    struct timeval timeout;
    timeout.tv_sec = 2;
    timeout.tv_usec = 0;
    int ready = select(sock+1, &fdaccept, NULL, NULL, &timeout);
    if (ready <= 0) {
        printf("Couldn't connect to server\n");
        return 2;
    }

    // receive accept
    char buffer[7];
    memset(buffer, 0, sizeof(buffer));
    recvfrom(sock, buffer, 7, 0, (struct sockaddr*)&server_address, (socklen_t*)&server_len);
    memset(buffer, 0, sizeof(buffer));

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