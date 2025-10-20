#include "common/common.h"

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    int64_t total = 0;
    int l, w, h;
    while (fscanf(f, "%dx%dx%d\n", &l, &w, &h) == 3) {
        total += 2 * l * w + 2 * w * h + 2 * h * l;
        total += MIN(l * w, MIN(w * h, h * l));
    }
    return total;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    int64_t total = 0;
    int l, w, h;
    while (fscanf(f, "%dx%dx%d\n", &l, &w, &h) == 3) {
        total += 2 * MIN(l + w, MIN(w + h, h + l));
        total += w * l * h;
    }
    return total;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-02/input-ex0.txt", solvePart0, 101, solvePart1, 48);
    failed += checkInputInt("day-02/input-real0.txt", solvePart0, 1606483, solvePart1, 3842356);
    return failed;
}

daySolver day02 = {2, dayMain, true};
