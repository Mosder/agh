#include <stdio.h>
#include <unistd.h>

int main(void) {
    for (int timer = 10; timer >= 0; timer--) {
        printf("Timer: %d\n", timer);
        sleep(1);
    }
    return 0;
}