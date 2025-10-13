#include "common/common.h"

static int64_t solvePart0(FILE *f) {
    int64_t floor = 0;
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }
    }
    return floor;
}

static int64_t solvePart1(FILE *f) {
    int64_t floor = 0;
    int64_t pos = 0;
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }
        pos++;
        if (floor < 0) {
            return pos;
        }
    }
    return -1;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-01/input-real0.txt", solvePart0, 280, solvePart1, 1797);
    return failed;
}

daySolver day01 = {1, dayMain, true};
