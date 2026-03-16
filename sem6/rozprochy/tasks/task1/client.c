#include "client.h"

int sock, sock_udp, sock_udp_multi;
struct sockaddr_in server_address, multi_address, own_address;
int server_len, multi_len, own_len;

int id;
char name[MAX_NAME_LENGTH];

// leave the server
void client_leave(int signo) {
    send(sock, COMMAND_STOP, strlen(COMMAND_STOP)+1, MSG_DONTWAIT);
    close(sock);
    close(sock_udp);
	close(sock_udp_multi);
    raise(SIGKILL);
}

// handle receiving messages
void* receive_handler(void *args) {
    while (1) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(message, 0, sizeof(message));
        read(sock, message, MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH);
		// handle server dead message
        if (strcmp(message, SERVER_DEAD_MESSAGE) == 0) {
			printf(SERVER_DEAD_DISPLAY_MESSAGE);
			close(sock);
			close(sock_udp);
			close(sock_udp_multi);
			raise(SIGKILL);
		}
		// handle UDP address request
		else if (strncmp(message, UDP_ADDRESS_REQUEST, strlen(UDP_ADDRESS_REQUEST)) == 0) {
			char response[MAX_MESSAGE_LENGTH];
			id = atoi(message+strlen(UDP_ADDRESS_REQUEST)+1);
			sprintf(response, UDP_ADDRESS_RESPONSE" %d", id);
			sendto(sock_udp, response, strlen(response)+1, MSG_DONTWAIT, (struct sockaddr*)&server_address, server_len);
		}
        // handle ping
		else if (strcmp(message, PING_MESSAGE) == 0) send(sock, PONG_MESSAGE, strlen(PONG_MESSAGE)+1, MSG_DONTWAIT);
        // else, print received message
        else printf("%s", message);
    }
    return NULL;
}

// handle udp
void* udp_handler(void *args) {
    while (1) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(message, 0, sizeof(message));
        recvfrom(sock_udp, message, MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH, 0, (struct sockaddr*)&server_address, (socklen_t*)&server_len);
        printf("%s", message);
    }
    return NULL;
}

// handle udp multicast
void* udp_multi_handler(void *args) {
    while (1) {
        char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
        memset(message, 0, sizeof(message));
        recvfrom(sock_udp_multi, message, MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH, 0, (struct sockaddr*)&own_address, (socklen_t*)&own_len);
		
		// filter out own message
		char own_name[MAX_NAME_LENGTH];
		sprintf(own_name, MULTICAST_NAME_FORMAT, name, id);
		if (strncmp(own_name, message, strlen(own_name)))
			printf("%s", message);
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
        if (strncmp(buffer, SLASH_COMMAND_LIST, strlen(SLASH_COMMAND_LIST)) == 0) {
            sprintf(message, COMMAND_LIST);
        }
        // 2ONE
        else if (strncmp(buffer, SLASH_COMMAND_2ONE, strlen(SLASH_COMMAND_2ONE)) == 0) {
            sprintf(message, COMMAND_2ONE" %s", buffer+strlen(SLASH_COMMAND_2ONE)+1);
        }
        // U
        else if (strncmp(buffer, SLASH_COMMAND_U, strlen(SLASH_COMMAND_U)) == 0) {
			sprintf(message, COMMAND_2ALL" "ASCII_ART);
			sendto(sock_udp, message, strlen(message), MSG_DONTWAIT, (struct sockaddr*)&server_address, server_len);
			continue;
        }
        // M
        else if (strncmp(buffer, SLASH_COMMAND_M, strlen(SLASH_COMMAND_M)) == 0) {
			sprintf(message, MULTICAST_NAME_FORMAT": "ASCII_ART, name, id);
			sendto(sock_udp_multi, message, strlen(message), MSG_DONTWAIT, (struct sockaddr*)&multi_address, multi_len);
			continue;
        }
        // STOP
        else if (strncmp(buffer, SLASH_COMMAND_STOP, strlen(SLASH_COMMAND_STOP)) == 0) {
            client_leave(0);
            break;
        }
        // 2ALL
        else if (strlen(buffer) > 1) {
            sprintf(message, COMMAND_2ALL" %s", buffer);
        }

        send(sock, message, strlen(message), MSG_DONTWAIT);
    }
    return NULL;
}

int main(int argc, char *argv[]) {
	char *ip = DEFAULT_ADDRESS;
	int port = DEFAULT_PORT;

	if (argc == 4) {
		ip = argv[2];
		port = atoi(argv[3]);
	}
	else if (argc < 2) {
		printf(NO_NAME_GIVEN_ERROR_MESSAGE);
		return 1;
	}

    strcpy(name, argv[1]);

    // handle ctrl-c
    signal(SIGINT, client_leave);

    // socket tcp
    sock = socket(AF_INET, SOCK_STREAM, 0);
    // socket udp
    sock_udp = socket(AF_INET, SOCK_DGRAM, 0);
    // socket udp multicast
    sock_udp_multi = socket(AF_INET, SOCK_DGRAM, 0);
	int option_reuse = 1;
    setsockopt(sock_udp_multi, SOL_SOCKET, SO_REUSEADDR, &option_reuse, sizeof(option_reuse));

	// server address
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);
    server_address.sin_addr.s_addr = inet_addr(ip);
    server_len = sizeof(server_address);
    connect(sock, (struct sockaddr *) &server_address, sizeof(server_address));

	// multicast address (sending)
    memset(&multi_address, 0, sizeof(multi_address));
    multi_address.sin_family = AF_INET;
    multi_address.sin_port = htons(MULTICAST_PORT);
    multi_address.sin_addr.s_addr = inet_addr(MULTICAST_ADDRESS);
    multi_len = sizeof(multi_address);

	// multicast address (receiving)
    memset(&own_address, 0, sizeof(own_address));
    own_address.sin_family = AF_INET;
    own_address.sin_port = htons(MULTICAST_PORT);
    own_address.sin_addr.s_addr = htonl(INADDR_ANY);
    own_len = sizeof(own_address);
	bind(sock_udp_multi, (struct sockaddr*)&own_address, sizeof(own_address));
	// join multicast group
	struct ip_mreq mreq;
	mreq.imr_interface.s_addr = htonl(INADDR_ANY);
	mreq.imr_multiaddr.s_addr = inet_addr(MULTICAST_ADDRESS);
	setsockopt(sock_udp_multi, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));

    // send name
    send(sock, name, MAX_NAME_LENGTH, MSG_DONTWAIT);

    printf(INIT_MESSAGE_FORMAT, ip, port);

    // start receiving thread
    pthread_t receive_thread;
    pthread_create(&receive_thread, NULL, receive_handler, NULL);

    // start UDP receiving thread
    pthread_t udp_thread;
    pthread_create(&udp_thread, NULL, udp_handler, NULL);

    // start UDP multicast thread
    pthread_t udp_multi_thread;
    pthread_create(&udp_multi_thread, NULL, udp_multi_handler, NULL);

    // start sending thread
    pthread_t send_thread;
    pthread_create(&send_thread, NULL, send_handler, NULL);

    // wait until all threads terminate
    pthread_join(receive_thread, NULL);
    pthread_join(udp_thread, NULL);
    pthread_join(udp_multi_thread, NULL);
    pthread_join(send_thread, NULL);

    close(sock);
	close(sock_udp);
	close(sock_udp_multi);
    return 0;
}
