#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define BUFFER_LIMIT 256

double f(double x) {
    return 4/(x*x + 1);
}

double calculate_integral(int process_count, double dx, double *t) {
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    double result = 0;
    int fd[process_count][2];
    for (int proc = 0; proc < process_count; proc++) {
        pipe(fd[process_count]);
        pid_t pid = fork();
        if (pid == 0) {
            close(fd[process_count][0]);
            double sub_result = 0;
            for (int i = proc; i < 1.0/dx; i += process_count) {
                sub_result += dx * f(i*dx);
            }
            char sub_result_buffer[BUFFER_LIMIT];
            snprintf(sub_result_buffer, BUFFER_LIMIT, "%f", sub_result);
            write(fd[process_count][1], sub_result_buffer, sizeof(char) * BUFFER_LIMIT);
            close(fd[process_count][1]);
            exit(0);
        }
        close(fd[process_count][1]);
    }
    char sub_result[BUFFER_LIMIT];
    for (int proc = 0; proc < process_count; proc++) {
        read(fd[process_count][0], sub_result, sizeof(char) * BUFFER_LIMIT);
        close(fd[process_count][0]);
        result += strtod(sub_result, NULL);
    }
    clock_gettime(CLOCK_MONOTONIC, &t1);
    *t = t1.tv_sec - t0.tv_sec + 1.0e-9*t1.tv_nsec - 1.0e-9*t0.tv_nsec;
    return result;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Nierpawidłowa liczba argumentów\n");
        return 1;
    }
    
    double dx = strtod(argv[1], NULL);
    int n = atoi(argv[2]);
    if (dx <= 0 || n < 1) {
        printf("Niepoprawne wartości argumentów\n");
        return 1;
    }

    double result, time_elapsed;
    for (int i = 1; i <= n; i++) {
        result = calculate_integral(i, dx, &time_elapsed);
        printf("processes: %d, result: %f, elapsed time: %fs\n", i, result, time_elapsed);
    }
    return 0;
}