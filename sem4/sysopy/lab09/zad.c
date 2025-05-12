#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_LIMIT 256

double f(double x) {
    return 4/(x*x + 1);
}

// calculate subrange of integral
double calculate_sub_integral(double range_start, double range_end, double dx) {
    double sub_result = 0.0;
    for (double x = range_start; x <= range_end; x += dx) {
        sub_result += dx * f(x);
    }
    return sub_result;
}

// calculate whole integral
double calculate_integral(int thread_count, double dx, double *t) {
    // define timespec structs and get start time
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    // define array of sub results and ready array
    double sub_results[thread_count];
    int ready_array[thread_count];
    memset(ready_array, 0, sizeof(ready_array));

    // define the size of sub ranges
    double sub_range_size = 1 / thread_count;
    // create threads for calculations
    for (int t_num = 0; t_num < thread_count; t_num++) {
        // pthread_t thread; - change to threads
        sub_results[t_num] = calculate_sub_integral(t_num * sub_range_size, (t_num+1) * sub_range_size, dx);
        ready_array[t_num] = 1;
    }

    // wait until all threads are ready
    int ready_threads = 0;
    while (ready_threads < thread_count) {
        ready_threads = 0;
        for (int t_num = 0; t_num < thread_count; t_num++) {
            ready_threads += ready_array[t_num];
        }
    }

    // sum up sub results
    double result = 0.0;
    for (int t_num = 0; t_num < thread_count; t_num++) {
        result += sub_results[t_num];
    }

    // get end time and set the time taken to variable t
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

    // calculate the integral for number of threads from 1..=n
    double result, time_elapsed;
    for (int i = 1; i <= n; i++) {
        result = calculate_integral(i, dx, &time_elapsed);
        printf("threads: %d, result: %f, elapsed time: %fs\n", i, result, time_elapsed);
    }
    return 0;
}