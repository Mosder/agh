#include <stdio.h>

#ifndef DLL
#include "collatz.h"
#endif

#ifdef DLL
#include <dlfcn.h>
#endif

int main() {
    #ifndef DLL
    printf("non-dll test\n\n");
    #endif

    #ifdef DLL
    printf("dll test\n\n");

    void *libcollatz = dlopen("./libcollatz.so", RTLD_LAZY);
    if (!libcollatz) {
        printf("Encountered an error while loading the libcollatz library\n");
        return 0;
    }

    int (*test_collatz_convergence)();
    test_collatz_convergence = (int (*)())dlsym(libcollatz, "test_collatz_convergence");
    if (dlerror() != NULL) {
        printf("Encountered an error while loading the test_collatz_convergence function\n");
        return 0;
    }
    #endif

    const int TEST_SIZE = 5;
    const int MAX_ITER = 20;
    int test_numbers[5] = {1, 2, 17, 69, 420};
    for (int i = 0; i < TEST_SIZE; i++) {
        printf("Testing %d for MAX_ITER = %d\n", test_numbers[i], MAX_ITER);
        int steps[MAX_ITER];
        int iters = test_collatz_convergence(test_numbers[i], MAX_ITER, steps);
        if (iters == -1) {
            printf("Didn't manage to reach 1 in %d steps\n", MAX_ITER);
        }
        else {
            printf("Reached 1 in %d steps\n", iters);
            printf("Steps:\n");
            printf("%d", steps[0]);
            for (int i = 1; i <= iters; i++) {
                printf(" -> %d", steps[i]);
            }
            printf("\n\n");
        }
    }

    #ifdef DLL
    dlclose(libcollatz);
    #endif

    return 0;
}