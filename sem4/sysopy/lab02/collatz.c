#include "collatz.h"

int collatz_conjecture(int input) {
    return input % 2 == 0 ? input / 2 : 3*input + 1;
}

int test_collatz_convergence(int input, int max_iter, int *steps) {
    int iters = 0;
    steps[0] = input;
    while (input != 1 && iters++ < max_iter) {
        input = collatz_conjecture(input);
        steps[iters] = input;
    }
    return iters <= max_iter ? iters : -1;
}
