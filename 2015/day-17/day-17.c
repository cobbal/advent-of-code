#include "common/common.h"
#include "common/vec_common.h"

static int64_t count(int64_t volume, size_t n, int64_t *capacities, int64_t useCap) {
    if (n <= 0 || useCap == 0) {
        return volume == 0;
    }
    int64_t sum = count(volume, n - 1, capacities + 1, useCap);
    if (volume >= capacities[0]) {
        sum += count(volume - capacities[0], n - 1, capacities + 1, useCap - 1);
    }
    return sum;
}

static int64_t solvePart0L(Arena arena, FILE *f, int liters) {
    int i;
    vec_int64 capacities = vec_int64_create(arena);
    // NOLINTNEXTLINE(cert-err34-c)
    while (fscanf(f, "%d\n", &i) == 1) {
        vec_int64_push(capacities, i);
    }
    return count(liters, capacities->count, capacities->elements, INT64_MAX);
}

static int64_t solvePart1L(Arena arena, FILE *f, int liters) {
    vec_int64 capacities = vec_int64_create(arena);
    // NOLINTNEXTLINE(cert-err34-c)
    for (int cap; fscanf(f, "%d\n", &cap) == 1;) {
        vec_int64_push(capacities, cap);
    }
    for (int i = 1;; i++) {
        auto solutions = count(liters, capacities->count, capacities->elements, i);
        if (solutions > 0) {
            return solutions;
        }
    }
}

static int64_t solvePart0Ex(Arena arena, FILE *f) {
    return solvePart0L(arena, f, 25);
}
static int64_t solvePart0Real(Arena arena, FILE *f) {
    return solvePart0L(arena, f, 150);
}

static int64_t solvePart1Ex(Arena arena, FILE *f) {
    return solvePart1L(arena, f, 25);
}
static int64_t solvePart1Real(Arena arena, FILE *f) {
    return solvePart1L(arena, f, 150);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-17/input-ex0.txt", solvePart0Ex, 4, solvePart1Ex, 3);
    failed += checkInputInt("day-17/input-real0.txt", solvePart0Real, 654, solvePart1Real, 57);
    return failed;
}

daySolver day17 = {17, dayMain, true};
