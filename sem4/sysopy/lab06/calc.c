#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#define DEFAULT_DX 1e-9
#define DEFAULT_PROCESS_COUNT 4

double f(double x) {
    return 4/(x*x + 1);
}

double calculate_integral(int process_count, double dx, double range_start, double range_end) {
    double result = 0;
    unlink("pipe-subresults");
    mkfifo("pipe-subresults", S_IFIFO | S_IRWXU);
    for (int proc = 0; proc < process_count; proc++) {
        pid_t pid = fork();
        if (pid == 0) {
            double sub_result = 0;
            for (int i = proc; i < (range_end - range_start)/dx; i += process_count) {
                sub_result += dx * f(range_start + i*dx);
            }
            FILE *fp = fopen("pipe-subresults", "w");
            fwrite(&sub_result, sizeof(double), 1, fp);
            fclose(fp);
            exit(0);
        }
    }
    double sub_result;
    FILE *fp = fopen("pipe-subresults", "r");
    for (int proc = 0; proc < process_count; proc++) {
        fread(&sub_result, sizeof(double), 1, fp);
        result += sub_result;
    }
    fclose(fp);
    return result;
}

int main(int argc, char *argv[]) {
    int process_count;
    double dx, range_start, range_end;
    if (argc == 3) {
        dx = strtod(argv[1], NULL);
        process_count = atoi(argv[2]);
    }
    dx = dx > 0 ? dx : DEFAULT_DX;
    process_count = process_count >= 1 ? process_count : DEFAULT_PROCESS_COUNT;

    while(1) {
        unlink("pipe-range");
        mkfifo("pipe-range", S_IFIFO | S_IRWXU);
        FILE *fp_range = fopen("pipe-range", "r");
        fread(&range_start, sizeof(double), 1, fp_range);
        fread(&range_end, sizeof(double), 1, fp_range);
        fclose(fp_range);
        printf("Sczytano przedział: [%f; %f]\n", range_start, range_end);

        double result = calculate_integral(process_count, dx, range_start, range_end);
        FILE *fp_result = fopen("pipe-result", "w");
        fwrite(&result, sizeof(double), 1, fp_result);
        fclose(fp_result);
        printf("Obliczono i zapisano wynik\n");
    }
    return 0;
}