#include <iso646.h>

#include "common/common.h"
#include "common/vec.h"

typedef struct {
    int speed, flightTime, restTime;
} Stats;
typedef VEC(Stats) VecStats;

static VecStats readStats(Arena arena, FILE *f) {
    char *buf = nullptr;
    ssize_t bufLen = 0;
    VecStats stats;
    VEC_INIT(&stats, arena);
    while (getUntilDelimiter(arena, &buf, &bufLen, ' ', f) != EOF) {
        // NOLINTNEXTLINE(cert-err34-c)
        Stats s;
        check(fscanf(f, "can fly %d km/s for %d seconds, but then must rest for %d seconds.\n",
            &s.speed, &s.flightTime, &s.restTime ) == 3);
        VEC_PUSH(stats, s);
    }
    return stats;
}

static int64_t solvePart0N(Arena arena, FILE *f, int duration) {
    auto stats = readStats(arena, f);
    int64_t winner = 0;
    for (size_t i = 0; i < VEC_COUNT(stats); i++) {
        Stats s = VEC_ELEMS(stats)[i];
        int completeLapCount = duration / (s.flightTime + s.restTime);
        int finalLapLength = duration % (s.flightTime + s.restTime);
        int distance = s.speed * (s.flightTime * completeLapCount + MIN(finalLapLength, s.flightTime));
        winner = MAX(winner, distance);
    }
    return winner;
}

static int64_t solvePart1N(Arena arena, FILE *f, int duration) {
    auto stats = readStats(arena, f);
    bool *isFlying = arenaAlloc(arena, VEC_COUNT(stats), sizeof(*isFlying));
    int *timeLeft = arenaAlloc(arena, VEC_COUNT(stats), sizeof(*timeLeft));
    int *distance = arenaAlloc(arena, VEC_COUNT(stats), sizeof(*distance)); 
    int *points = arenaAlloc(arena, VEC_COUNT(stats), sizeof(*points));
    
    for (int t = 0; t < duration; t++) {
        for (size_t i = 0; i < VEC_COUNT(stats); i++) {
            Stats s = VEC_ELEMS(stats)[i];
            if (timeLeft[i] == 0) {
                isFlying[i] ^= 1;
                timeLeft[i] = isFlying[i] ? s.flightTime : s.restTime;
            }
            timeLeft[i]--;
            if (isFlying[i]) {
                distance[i] += s.speed;
            }
        }
        int bestDist = 0;
        for (size_t i = 0; i < VEC_COUNT(stats); i++) {
            bestDist = MAX(bestDist, distance[i]);
        }
        for (size_t i = 0; i < VEC_COUNT(stats); i++) {
            if (distance[i] == bestDist) {
                points[i]++;
            }
        }
    }
    
    int64_t winner = 0;
    for (size_t i = 0; i < VEC_COUNT(stats); i++) {
        winner = MAX(winner, points[i]);
    }
    return winner;
}

static int64_t solvePart0Ex(Arena arena, FILE *f) {
    return solvePart0N(arena, f, 1000);
}

static int64_t solvePart0Real(Arena arena, FILE *f) {
    return solvePart0N(arena, f, 2503);
}

static int64_t solvePart1Ex(Arena arena, FILE *f) {
    return solvePart1N(arena, f, 1000);
}

static int64_t solvePart1Real(Arena arena, FILE *f) {
    return solvePart1N(arena, f, 2503);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-14/input-ex0.txt", solvePart0Ex, 1120, solvePart1Ex, 689);
    failed += checkInputInt("day-14/input-real0.txt", solvePart0Real, 2696, solvePart1Real, 1084);
    return failed;
}

daySolver day14 = {14, dayMain, true};
