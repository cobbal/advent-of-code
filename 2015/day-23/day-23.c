#include "common/common.h"

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    (void)f;
    return 0;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    (void)f;
    return 0;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-23/input-ex0.txt", solvePart0, -1, solvePart1, -1);
    // failed += checkInputInt("day-23/input-real0.txt", solvePart0, -1, solvePart1, -1);
    return failed;
}
daySolver day23 = { 23, dayMain, false };
