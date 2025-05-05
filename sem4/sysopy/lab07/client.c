#include <mqueue.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include "mq_info.h"

int main(void) {
    // create client queue
    char client_queue_name[64];
    snprintf(client_queue_name, sizeof(client_queue_name), CLIENT_QUEUE_NAME"%d", getpid());
    struct mq_attr mqat;
    mqat.mq_maxmsg = 10;
    mqat.mq_msgsize = sizeof(message);
    mqd_t client_queue_id = mq_open(client_queue_name, O_RDONLY | O_CREAT, 0644, &mqat);

    // initialize client
    message request, response;
    request.type = INIT;
    request.client_id = getpid();
    mqd_t server_queue_id = mq_open(SERVER_QUEUE_NAME, O_WRONLY);
    mq_send(server_queue_id, (const char *) &request, sizeof(message), 0);
    mq_receive(client_queue_id, (char *) &response, MESSAGE_SIZE, NULL);
    if (response.client_id == -1) {
        printf("Couldn't connect to chat ;(\n");
        return 1;
    }
    request.type = MESSAGE;
    request.client_id = response.client_id;
    printf("Connected to chat - your id: %d. Behave yourself ;)\n", request.client_id);

    if (fork() == 0) {
        // child receives messages
        while(1) {
            mq_receive(client_queue_id, (char *) &response, MESSAGE_SIZE, NULL);
            printf("client %d: %s", response.client_id, response.message);
        }
    }
    else {
        // parent sends messages
        while(1) {
            fgets(request.message, MAX_MESSAGE_LENGTH, stdin);
            mq_send(server_queue_id, (const char *) &request, sizeof(message), 0);
        }
    }
    return 0;
}