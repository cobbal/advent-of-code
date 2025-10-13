#include "common/common.h"
#include "common/set.h"

static int64_t solvePart0(FILE *f) {
    int c, x = 0, y = 0;
    int64Set visited = int64SetCreate();
    int64SetInsert(visited, 0);
    while ((c = fgetc(f)) != EOF) {
        if (c == '^') {
            y--;
        } else if (c == '>') {
            x++;
        } else if (c == 'v') {
            y++;
        } else if (c == '<') {
            x--;
        } else {
            continue;
        }
        int64SetInsert(visited, (int64_t)(uint32_t)x << 32 | (int64_t)(uint32_t)y);
    }
    size_t result = int64SetCount(visited);
    int64SetDestroy(visited);
    return (int64_t)result;
}

static int64_t solvePart1(FILE *f) {
    int c, x[2] = {0, 0}, y[2] = {0, 0};
    int64Set visited = int64SetCreate();
    int64SetInsert(visited, 0);
    for (int i = 0; (c = fgetc(f)) != EOF; i = !i) {
        if (c == '^') {
            y[i]--;
        } else if (c == '>') {
            x[i]++;
        } else if (c == 'v') {
            y[i]++;
        } else if (c == '<') {
            x[i]--;
        } else {
            continue;
        }
        int64SetInsert(visited, (int64_t)(uint32_t)x[i] << 32 | (int64_t)(uint32_t)y[i]);
    }
    size_t result = int64SetCount(visited);
    int64SetDestroy(visited);
    return (int64_t)result;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-03/input-ex0.txt", solvePart0, 2, solvePart1, 3);
    failed += checkInputInt("day-03/input-ex1.txt", solvePart0, 4, solvePart1, 3);
    failed += checkInputInt("day-03/input-ex2.txt", solvePart0, 2, solvePart1, 11);
    failed += checkInputInt("day-03/input-real0.txt", solvePart0, 2572, solvePart1, 2631);
    return failed;
}

daySolver day03 = {3, dayMain, true};
