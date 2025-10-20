#include <math.h>

#include "common/common.h"

static int64_t pushSmallPrime(VecI64 primes) {
    for (int p = VEC_ELEMS(primes)[VEC_COUNT(primes) - 1] + 2;; p += 2) {
        bool good = true;
        for (int64_t *d = VEC_ELEMS(primes); *d * *d <= p; d++) {
            if (p % *d == 0) {
                good = false;
                break;
            }
        }
        if (good) {
            VEC_PUSH(primes, p);
            return p;
        }
    }
}

static VecI64 makeSmallPrimes(Arena arena, size_t n) {
    VecI64 primes;
    VEC_INIT(&primes, arena);
    VEC_PUSH(primes, 2);
    VEC_PUSH(primes, 3);
    while (VEC_COUNT(primes) < n) {
        pushSmallPrime(primes);
    }
    return primes;
}

static int64_t divisorSum(VecI64 primes, int64_t n) {
    int64_t product = 1;
    for (size_t i = 0;; i++) {
        if (i == VEC_COUNT(primes)) { pushSmallPrime(primes); }
        int64_t p = VEC_ELEMS(primes)[i];
        int64_t sum = 0;
        int64_t pow = 1;
        while (n % p == 0) {
            sum += pow;
            pow *= p;
            n /= p;
        }
        product *= sum + pow;
        if (p * p > n) {
            return n == 1 ? product : product * (n + 1);
        }
    }
}

static void divisorsOf(VecI64 primes, int64_t n, VecI64 factors, VecI64 multiplicities) {
    VEC_CLEAR(factors);
    VEC_CLEAR(multiplicities);
    for (size_t i = 0; i < VEC_COUNT(primes); i++) {
        int64_t p = VEC_ELEMS(primes)[i];
        if (p > n) { break; }
        int64_t multiplicity = 0;
        while (n % p == 0) {
            multiplicity++;
            n /= p;
        }
        if (multiplicity) {
            VEC_PUSH(factors, p);
            VEC_PUSH(multiplicities, multiplicity);
        }
    }
}

static int64_t solvePart0(Arena arena, FILE *f) {
    int64_t cutoff;
    // NOLINTNEXTLINE(cert-err34-c)
    check(fscanf(f, "%llu", &cutoff) == 1);
    cutoff /= 10;

    auto primes = makeSmallPrimes(arena, 10);
    for (int64_t i = 1; ; i++) {
        if (divisorSum(primes, i) > cutoff) {
            return i;
        }
    }
}

static int64_t combos(size_t k, int64_t taken, int64_t rejected, VecI64 factors, VecI64 multiplicities) {
    if (rejected > 50) { return 0; }
    if (k >= VEC_COUNT(factors)) { return taken; }
    int64_t p = VEC_ELEMS(factors)[k];
    int64_t m = VEC_ELEMS(multiplicities)[k];
    int64_t pow = 1;
    int64_t rejPow = 1;
    for (int j = 0; j < m; j++) { rejPow *= p; }
    int64_t sum = 0;
    for (int j = 0; j <= m; j++) {
        sum += combos(k + 1, taken * pow, rejected * rejPow, factors, multiplicities);
        pow *= p;
        rejPow /= p;
    }
    return sum;
}

static int64_t solvePart1(Arena arena, FILE *f) {
    int64_t cutoff;
    // NOLINTNEXTLINE(cert-err34-c)
    check(fscanf(f, "%llu", &cutoff) == 1);
    cutoff = (cutoff + 10) / 11;

    auto primes = makeSmallPrimes(arena, 10);
    VecI64 factors, multiplicities;
    VEC_INIT(&factors, arena);
    VEC_INIT(&multiplicities, arena);
    for (int64_t i = 1;; i++) {
        divisorsOf(primes, i, factors, multiplicities);
        if (combos(0, 1, 1, factors, multiplicities) > cutoff) {
            return i;
        }
    }
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-20/input-real0.txt", solvePart0, 665280, solvePart1, 705600);
    return failed;
}

daySolver day20 = {20, dayMain, true};
