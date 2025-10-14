#include <iso646.h>

#include "common/common.h"
#include "common/vec_common.h"

typedef struct {
    int speed, flightTime, restTime;
} Stats;

#define VEC_ELEMENT_TYPE Stats
#include "common/vec_impl.c"
#undef VEC_ELEMENT_TYPE

static vec_Stats readStats(Arena arena, FILE *f) {
    char *buf = nullptr;
    ssize_t bufLen = 0;
    auto stats = vec_Stats_create(arena);
    while (getUntilDelimiter(arena, &buf, &bufLen, ' ', f) != EOF) {
        // NOLINTNEXTLINE(cert-err34-c)
        Stats s;
        check(fscanf(f, "can fly %d km/s for %d seconds, but then must rest for %d seconds.\n",
            &s.speed, &s.flightTime, &s.restTime ) == 3);
        vec_Stats_push(stats, s);
    }
    return stats;
}

static int64_t solvePart0N(Arena arena, FILE *f, int duration) {
    auto stats = readStats(arena, f);
    int64_t winner = 0;
    for (size_t i = 0; i < stats->count; i++) {
        Stats s = stats->elements[i];
        int completeLapCount = duration / (s.flightTime + s.restTime);
        int finalLapLength = duration % (s.flightTime + s.restTime);
        int distance = s.speed * (s.flightTime * completeLapCount + min(finalLapLength, s.flightTime));
        winner = max(winner, distance);
    }
    return winner;
}

static int64_t solvePart1N(Arena arena, FILE *f, int duration) {
    auto stats = readStats(arena, f);
    bool *isFlying = arenaAlloc(arena, stats->count, sizeof(*isFlying));
    int *timeLeft = arenaAlloc(arena, stats->count, sizeof(*timeLeft));
    int *distance = arenaAlloc(arena, stats->count, sizeof(*distance)); 
    int *points = arenaAlloc(arena, stats->count, sizeof(*points));
    
    for (int t = 0; t < duration; t++) {
        for (size_t i = 0; i < stats->count; i++) {
            Stats s = stats->elements[i];
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
        for (size_t i = 0; i < stats->count; i++) {
            bestDist = max(bestDist, distance[i]);
        }
        for (size_t i = 0; i < stats->count; i++) {
            if (distance[i] == bestDist) {
                points[i]++;
            }
        }
    }
    
    int64_t winner = 0;
    for (size_t i = 0; i < stats->count; i++) {
        winner = max(winner, points[i]);
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
