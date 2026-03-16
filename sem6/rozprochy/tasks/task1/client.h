#include "common.h"

#define MULTICAST_ADDRESS "225.0.21.37"
#define MULTICAST_PORT 9000
#define MULTICAST_NAME_FORMAT "[%s (%d) MULTI]"

#define SERVER_DEAD_DISPLAY_MESSAGE "Server killed.\n"
#define NO_NAME_GIVEN_ERROR_MESSAGE "No name given.\n"

#define SLASH_COMMAND_LIST "/list"
#define SLASH_COMMAND_2ONE "/msg"
#define SLASH_COMMAND_STOP "/stop"
#define SLASH_COMMAND_U "/U"
#define SLASH_COMMAND_M "/M"

#define COMMAND_LIST_FORMAT SLASH_COMMAND_LIST
#define COMMAND_2ALL_FORMAT "<message>"
#define COMMAND_2ONE_FORMAT SLASH_COMMAND_2ONE" <recipient_id> <message>"
#define COMMAND_U_FORMAT SLASH_COMMAND_U
#define COMMAND_M_FORMAT SLASH_COMMAND_M
#define COMMAND_STOP_FORMAT SLASH_COMMAND_STOP

#define INIT_MESSAGE_FORMAT \
	"Connected to server at %s:%d\n"\
	"Possible actions:\n"\
	" - Get a list of users: "COMMAND_LIST_FORMAT"\n"\
	" - Message all users: "COMMAND_2ALL_FORMAT"\n"\
	" - Message one user: "COMMAND_2ONE_FORMAT"\n"\
	" - Send ASCII (UDP): "COMMAND_U_FORMAT"\n"\
	" - Send ASCII (UDP Multicast): "COMMAND_M_FORMAT"\n"\
	" - Leave chat: "COMMAND_STOP_FORMAT"\n"

#define ASCII_ART "\n"\
	"████████╗███████╗███╗   ███╗██████╗ ██╗     ███████╗ ██████╗ ███████╗\n"\
	"╚══██╔══╝██╔════╝████╗ ████║██╔══██╗██║     ██╔════╝██╔═══██╗██╔════╝\n"\
	"   ██║   █████╗  ██╔████╔██║██████╔╝██║     █████╗  ██║   ██║███████╗\n"\
	"   ██║   ██╔══╝  ██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝  ██║   ██║╚════██║\n"\
	"   ██║   ███████╗██║ ╚═╝ ██║██║     ███████╗███████╗╚██████╔╝███████║\n"\
	"   ╚═╝   ╚══════╝╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝ ╚═════╝ ╚══════╝\n"
