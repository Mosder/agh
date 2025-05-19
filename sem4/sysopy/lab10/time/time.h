#include <time.h>

#define TIME_PATTERN "[%d:%d:%d]"
#define TM_TO_STRING(tmp) (tmp->tm_hour), (tmp->tm_min), (tmp->tm_sec)