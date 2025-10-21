#include "common/common.h"

static VecI64 readPackages(Arena arena, FILE *f) {
    VecI64 packages;
    VEC_INIT(&packages, arena);
    // NOLINTNEXTLINE(cert-err34-c)
    for (int64_t in; fscanf(f, "%llu\n", &in) == 1; VEC_PUSH(packages, in)) {}
    return packages;
}

static int64_t sum(VecI64 v) {
    int64_t total = 0;
    VEC_FOR(i, v) { total += *i; }
    return total;
}

static bool canThird(VecI64 packages, int64_t targetSum0, int64_t targetSum1, size_t consider) {
    if (targetSum0 == 0 && targetSum1 == 0) { return true; }
    if (targetSum0 < 0 || targetSum1 < 0 || consider >= VEC_COUNT(packages)) { return false; }

    int64_t package = VEC_ELEMS(packages)[consider];
    if (canThird(packages, targetSum0, targetSum1, consider + 1)) { return true; }
    if (package > 0 && canThird(packages, targetSum0 - package, targetSum1, consider + 1)) { return true; }
    if (package > 0 && canThird(packages, targetSum0, targetSum1 - package, consider + 1)) { return true; }
    return false;
}

static bool canHalve(VecI64 packages, int64_t targetSum, size_t consider) {
    if (targetSum == 0) { return true; }
    if (targetSum < 0 || consider >= VEC_COUNT(packages)) { return false; }

    int64_t package = VEC_ELEMS(packages)[consider];
    if (canHalve(packages, targetSum, consider + 1)) { return true; }
    return package > 0 && canHalve(packages, targetSum - package, consider + 1);
}

static bool canHalve0(VecI64 packages) {
    return canHalve(packages, sum(packages) / 2, 0);
}

static bool canThird0(VecI64 packages) {
    auto target = sum(packages) / 3;
    return canThird(packages, target, target, 0);
}

static int64_t selectSumPrime(VecI64 packages, size_t maxN, int64_t targetSum, size_t consider, bool (*verifier)(VecI64)) {
    if (targetSum == 0) {
        // Found first partition, now check if other 2 work
        return !!verifier(packages);
    }
    if (maxN <= 0 || targetSum < 0 || consider >= VEC_COUNT(packages)) { return 0; }

    int64_t product0 = selectSumPrime(packages, maxN, targetSum, consider + 1, verifier);

    int64_t package = -(VEC_ELEMS(packages)[consider] *= -1);
    int64_t product1 = package * selectSumPrime(packages, maxN - 1, targetSum - package, consider + 1, verifier);
    VEC_ELEMS(packages)[consider] *= -1;

    if (!product0) { return product1; }
    if (!product1) { return product0; }
    return MIN(product0, product1);
}

static int64_t solvePart0(Arena arena, FILE *f) {
    VecI64 packages = readPackages(arena, f);
    int64_t target = sum(packages) / 3;
    for (size_t n = 1; n < VEC_COUNT(packages); n++) {
        auto quantum = selectSumPrime(packages, n, target, 0, canHalve0);
        if (quantum > 0) {
            return quantum;
        }
    }
    return 0;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    VecI64 packages = readPackages(arena, f);
    int64_t target = sum(packages) / 4;
    for (size_t n = 1; n < VEC_COUNT(packages); n++) {
        auto quantum = selectSumPrime(packages, n, target, 0, canThird0);
        if (quantum > 0) {
            return quantum;
        }
    }
    return 0;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-24/input-ex0.txt", solvePart0, 99, solvePart1, 44);
    failed += checkInputInt("day-24/input-real0.txt", solvePart0, 10723906903, solvePart1, 74850409);
    return failed;
}

daySolver day24 = {24, dayMain, true};
