#include "time.h"

struct tm* get_tm() {
    time_t timestamp;
    time(&timestamp);
    struct tm *tmp = localtime(&timestamp);
    return tmp;
}