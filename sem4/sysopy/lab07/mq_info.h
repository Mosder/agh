// queue names
#define SERVER_QUEUE_NAME "/server_queue"
#define CLIENT_QUEUE_NAME "/client_queue-"
// message types
#define INIT 0
#define MESSAGE 1
// max message length
#define MAX_MESSAGE_LENGTH 100
// message struct
typedef struct {
    int type;
    int client_id;
    char message[MAX_MESSAGE_LENGTH];
} message;
// message size
#define MESSAGE_SIZE (2*sizeof(message))