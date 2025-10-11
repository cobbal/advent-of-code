#include "common.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void checkImpl(bool b, char *expr, char *file, int line) {
    if (!b) {
        fprintf(stderr, "\nERROR %s:%d: check failure: %s\n\n", file, line, expr);
        exit(1);
    }
}

void dayHeader(int dayNumber) {
    printf("=== day-%02d ===\n", dayNumber);
}

void checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1) {
    printf("%22s: ", path);
    FILE *f = fopen(path, "r");
    check(f);
    int64_t result0 = part0(f);
    printf("%20lld ", result0);
    fseek(f, 0, SEEK_SET);
    int64_t result1 = part1(f);
    printf("%20lld ", result1);
    check(!fclose(f));
    if (result0 == expected0 && result1 == expected1) {
        printf("  good\n");
    } else {
        printf("   bad\n");
    }
}
