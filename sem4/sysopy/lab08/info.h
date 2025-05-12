#define QUEUE_LENGTH 5
#define MESSAGE_LENGTH 10
#define MEMORY_SIZE (QUEUE_LENGTH * MESSAGE_LENGTH * sizeof(char))

#define ALPHABET_START 'a'
#define ALPHABET_END 'z'
#define ALPHABET_LENGTH (ALPHABET_END - ALPHABET_START + 1)

#define SLEEP_BASE_LENGTH 5
#define SLEEP_INCREASE_MAX 10

#define SEM_QUEUE_SPACE "/queue_space"
#define SEM_REQUEST_COUNT "/request_count"
#define SEM_BUFFER_CHANGE "/buffer_change"

#define MEMORY_NAME "/memory_queue"