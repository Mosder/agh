#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main(void) {
    unlink("pipe-result");
    mkfifo("pipe-result", S_IFIFO | S_IRWXU);

    double range_start, range_end, result;
    printf("Podaj przedział, na którym funkcja ma być scałkowana:\n");
    scanf("%lf %lf", &range_start, &range_end);

    FILE *fp_range = fopen("pipe-range", "w");
    fwrite(&range_start, sizeof(double), 1, fp_range);
    fwrite(&range_end, sizeof(double), 1, fp_range);
    fclose(fp_range);
    printf("Zapisano przedział\n");

    FILE *fp_result = fopen("pipe-result", "r");
    fread(&result, sizeof(double), 1, fp_result);
    fclose(fp_result);

    printf("Sczytany wynik całkowania: %f\n", result);
    return 0;
}