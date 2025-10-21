#include "common/common.h"

static int64_t code(size_t n) {
    int64_t code = 20151125;
    for (size_t i = 0; i < n; i++) {
        code = (code * 252533) % 33554393;
    }
    return code;
}

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    int64_t row, col;
    // NOLINTNEXTLINE(cert-err34-c)
    check(
        fscanf(
            f, "To continue, please consult the code grid in the manual.  Enter the code at row %lli, column %lli.",
            &row, &col
        ) == 2
    );
    int64_t diag0 = row + col - 2;
    int64_t tri = diag0 * (diag0 + 1) / 2;
    int64_t diag1 = col - 1;
    int64_t n = tri + diag1;
    return code(n);
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    (void)f;
    return 0;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-25/input-ex0.txt", solvePart0, 21345942, solvePart1, 0);
    failed += checkInputInt("day-25/input-real0.txt", solvePart0, 8997277, solvePart1, 0);
    return failed;
}

daySolver day25 = {25, dayMain, true};
