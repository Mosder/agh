#include "server.h"

int server_socket, server_socket_udp;
int client_id = 0;
client_info *clients[MAX_CLIENTS];
pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;

// kill server function
void kill_server(int signo) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            send(clients[i]->socket, SERVER_DEAD_MESSAGE, strlen(SERVER_DEAD_MESSAGE)+1, MSG_DONTWAIT);
			close(clients[i]->socket);
        }
    }
    pthread_mutex_unlock(&clients_mutex);
	close(server_socket);
	close(server_socket_udp);
	raise(SIGKILL);
}

// ping client to check if they're alive
void ping_clients() {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            write(clients[i]->socket, PING_MESSAGE, strlen(PING_MESSAGE)+1);
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// server message
void server_message(char *mess, client_info *exclude) {
    pthread_mutex_lock(&clients_mutex);
	char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
	sprintf(message, SERVER_NAME": %s", mess);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] && clients[i] != exclude) {
            write(clients[i]->socket, message, strlen(message));
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// remove client from clients array
void remove_client(client_info *info) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] && clients[i] == info) {
			char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
			sprintf(message, CLIENT_DISCONNECTED_MESSAGE_FORMAT, info->name, info->id);
            close(info->socket);
            clients[i] = NULL;
			pthread_mutex_unlock(&clients_mutex);
			printf(message);
			server_message(message, NULL);
            break;
        }
    }
}

// check if clients are alive
void check_responses() {
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (clients[i]->alive) clients[i]->alive = 0;
            else {
				pthread_mutex_unlock(&clients_mutex);
				remove_client(clients[i]);
				pthread_mutex_lock(&clients_mutex);
			}
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
			sprintf(str + strlen(str), USERS_LIST_LINE_FORMAT, clients[i]->name, clients[i]->id);
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
void send_to_one(char *message, int recipient_id) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (clients[i]->id == recipient_id) {
                write(clients[i]->socket, message, strlen(message));
                break;
            }
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
	// request udp address
	char udp_req_buffer[MAX_MESSAGE_LENGTH];
	sprintf(udp_req_buffer, UDP_ADDRESS_REQUEST" %d", info->id);
	write(info->socket, udp_req_buffer, strlen(udp_req_buffer));

    while(1) {
        char buffer[MAX_MESSAGE_LENGTH];
        memset(buffer, 0, sizeof(buffer));
		read(info->socket, buffer, MAX_MESSAGE_LENGTH);
		// print requests in server console
		printf(CLIENT_NAME_FORMAT": %s\n", info->name, info->id, buffer);
        // LIST
        if (strncmp(buffer, COMMAND_LIST, strlen(COMMAND_LIST)) == 0) {
            char users[MAX_MESSAGE_LENGTH];
            get_users(users);
            write(info->socket, users, strlen(users));
        }
        // 2ALL
        else if(strncmp(buffer, COMMAND_2ALL, strlen(COMMAND_2ALL)) == 0) {
            char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
            sprintf(message, CLIENT_NAME_FORMAT": %s", info->name, info->id, buffer+strlen(COMMAND_2ALL)+1);
            send_to_all(message, info);
        }
        // 2ONE
        else if(strncmp(buffer, COMMAND_2ONE, strlen(COMMAND_2ONE)) == 0) {
            char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
            char *recipient = strtok(buffer+strlen(COMMAND_2ONE)+1, " ");
			int recipient_id = atoi(recipient);
            char *content = strtok(NULL, "");
            sprintf(message, PV_INDICATOR" "CLIENT_NAME_FORMAT": %s", info->name, info->id, content);
            send_to_one(message, recipient_id);
        }
        // STOP
        else if(strncmp(buffer, COMMAND_STOP, strlen(COMMAND_STOP)) == 0) {
            remove_client(info);
            free(info);
            break;
        }
        // ALIVE
        else if(strncmp(buffer, PONG_MESSAGE, strlen(PONG_MESSAGE)) == 0) {
            confirm_response(info);
        }
		// LOST CONNECTION
        else {
            remove_client(info);
            free(info);
            break;
        }
    }
    return NULL;
}

// count the number of connected clients
int client_count() {
    int count = 0;
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) count++;
    }
    pthread_mutex_unlock(&clients_mutex);
    return count;
}

// set udp address of a client with matching id
void set_udp_address(int id, struct sockaddr_in udp_address) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (clients[i]->id == id) {
				clients[i]->udp_address = udp_address;
				break;
            }
        }
    }
    pthread_mutex_unlock(&clients_mutex);
}

// return client info pointer by udp address
client_info* get_client_info_by_udp(const struct sockaddr_in *udp_address) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) {
            if (memcmp(&clients[i]->udp_address, udp_address, sizeof(struct sockaddr_in)) == 0) {
                pthread_mutex_unlock(&clients_mutex);
                return clients[i];
            }
        }
    }
    pthread_mutex_unlock(&clients_mutex);
    return NULL;
}

// handle UDP
void* handle_udp(void *args) {
    address_info *addr_info = (address_info*)args;
    struct sockaddr_in server_address, client_address;

    // server socket
    server_socket_udp = socket(AF_INET, SOCK_DGRAM, 0);
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(addr_info->port);
    server_address.sin_addr.s_addr = inet_addr(addr_info->ip);
	free(addr_info);
    // reuse address option
    int option = 1;
    setsockopt(server_socket_udp, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
    bind(server_socket_udp, (struct sockaddr *) &server_address, sizeof(server_address));

    while(1) {
        char buffer[MAX_MESSAGE_LENGTH];
        memset(buffer, 0, MAX_MESSAGE_LENGTH);
        int client_len = sizeof(client_address);
        recvfrom(server_socket_udp, buffer, MAX_MESSAGE_LENGTH, 0, (struct sockaddr*)&client_address, (socklen_t*)&client_len);

		// UDP address response
		if (strncmp(buffer, UDP_ADDRESS_RESPONSE, strlen(UDP_ADDRESS_RESPONSE)) == 0) {
			// print request
			printf("%s\n", buffer);
			int id = atoi(buffer+strlen(UDP_ADDRESS_RESPONSE)+1);
			set_udp_address(id, client_address);
		}
		// 2ALL
        else if(strncmp(buffer, COMMAND_2ALL, strlen(COMMAND_2ALL)) == 0) {
			client_info *info = get_client_info_by_udp(&client_address);
			// print request
			printf(CLIENT_NAME_FORMAT": %s\n", info->name, info->id, buffer);
            char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
            sprintf(message, CLIENT_NAME_FORMAT": %s", info->name, info->id, buffer+strlen(COMMAND_2ALL)+1);
			for (int i = 0; i < MAX_CLIENTS; i++) {
				if (clients[i] && clients[i] != info) {
					sendto(server_socket_udp, message, strlen(message), 0, (struct sockaddr*)&clients[i]->udp_address, sizeof(clients[i]->udp_address));
				}
			}
        }
    }

    return NULL;
}

int main(int argc, char *argv[]) {
	char *ip = DEFAULT_ADDRESS;
	int port = DEFAULT_PORT;

	if (argc == 3) {
		ip = argv[1];
		port = atoi(argv[2]);
	}

    // handle ctrl-c
    signal(SIGINT, kill_server);

    int client_socket;
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
    printf(INIT_MESSAGE_FORMAT, ip, port);

    // UDP thread
	address_info *addr_info = (address_info*)malloc(sizeof(address_info));
	strcpy(addr_info->ip, ip);
	addr_info->port = port;
    pthread_t udp_thread;
    pthread_create(&udp_thread, NULL, &handle_udp, (void*)addr_info);

    // pinging thread
    pthread_t pinging_thread;
    pthread_create(&pinging_thread, NULL, &handle_pinging, NULL);

    while(1) {
        int client_len = sizeof(client_address);
        client_socket = accept(server_socket, (struct sockaddr*)&client_address, (socklen_t*)&client_len);

        // if no space, don't add the new client
        if (client_count() >= MAX_CLIENTS) {
            printf(FULL_CHAT_MESSAGE);
            close(client_socket);
            continue;
        }

        // create new client info
        client_info *info = (client_info*)malloc(sizeof(client_info));
        info->address = client_address;
        info->socket = client_socket;
        info->alive = 1;
		info->id = ++client_id;
        read(client_socket, info->name, MAX_NAME_LENGTH);

        // add client and create handling thread
        add_client(info);
        pthread_t client_thread;
        pthread_create(&client_thread, NULL, &handle_client, (void*)info);

		// announce new client
		char message[MAX_NAME_LENGTH + MAX_MESSAGE_LENGTH];
		sprintf(message, CLIENT_CONNECTED_MESSAGE_FORMAT, info->name, info->id);
        printf(message);
		server_message(message, info);
    }

    return 0;
}
