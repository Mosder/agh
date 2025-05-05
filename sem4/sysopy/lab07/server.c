#include <mqueue.h>
#include <stdio.h>
#include <string.h>
#include "mq_info.h"
#include <errno.h>

#define CLIENT_LIMIT 10

int main(void) {
    // clients info
    mqd_t clients_queues[CLIENT_LIMIT];
    int clients_connected = 0;

    // create server queue
    struct mq_attr mqat;
    mqat.mq_maxmsg = 10;
    mqat.mq_msgsize = sizeof(message);
    mqd_t server_queue_id = mq_open(SERVER_QUEUE_NAME, O_RDONLY | O_CREAT, 0644, &mqat);

    // initialize message and response structs
    message response, request;

    while(1) {
        mq_receive(server_queue_id, (char *) &request, MESSAGE_SIZE, NULL);
        switch (request.type) {
            // handle initialization
            case INIT:
                response.type = INIT;
                if (clients_connected >= CLIENT_LIMIT) {
                    response.client_id = -1;
                }
                else {
                    char client_queue_name[64];
                    snprintf(client_queue_name, sizeof(client_queue_name), CLIENT_QUEUE_NAME"%d", request.client_id);
                    clients_queues[clients_connected] = mq_open(client_queue_name, O_WRONLY);
                    response.client_id = clients_connected;
                }
                mq_send(clients_queues[clients_connected++], (const char *) &response, sizeof(message), 0);
                break;
            // handle message send
            case MESSAGE:
                response.type = MESSAGE;
                response.client_id = request.client_id;
                strcpy(response.message, request.message);
                for (int client = 0; client < clients_connected; client++) {
                    if (client != request.client_id)
                        mq_send(clients_queues[client], (const char *) &response, sizeof(message), 0);
                }
                break;
        }
    }
    return 0;
}