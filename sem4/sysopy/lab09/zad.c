#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

typedef struct {
    double range_start;
    double range_end;
    double dx;
    double *sub_result_address;
    int *ready_address;
} integral_args;

double f(double x) {
    return 4/(x*x + 1);
}

// calculate subrange of integral
void* calculate_sub_integral(void *args) {
    integral_args *data = (integral_args*) args;
    double sub_result = 0.0;
    for (double x = data->range_start; x < data->range_end; x += data->dx) {
        sub_result += data->dx * f(x);
    }
    *data->sub_result_address = sub_result;
    *data->ready_address = 1;
    return NULL;
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
    double sub_range_size = 1.0 / thread_count;
    // create threads for calculations
    pthread_t threads[thread_count];
    integral_args args[thread_count];
    for (int t_num = 0; t_num < thread_count; t_num++) {
        // define args structure
        args[t_num].range_start = t_num * sub_range_size;
        args[t_num].range_end = args[t_num].range_start + sub_range_size;
        args[t_num].dx = dx;
        args[t_num].sub_result_address = sub_results + t_num;
        args[t_num].ready_address = ready_array + t_num;
        // run calculations in new thread
        pthread_create(threads + t_num, NULL, calculate_sub_integral, (void*) (args+t_num));
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