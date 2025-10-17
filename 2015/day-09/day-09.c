#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "common/common.h"

typedef struct {
    size_t city0, city1;
    int dist;
} Distance;

typedef VEC(Distance) VecDistance;

static size_t cityIndex(VecString cities, const char *city) {
    for (size_t i = 0; i < VEC_COUNT(cities); i++) {
        if (strcmp(VEC_ELEMS(cities)[i], city) == 0) {
            return i;
        }
    }
    VEC_PUSH(cities, arenaStrdup(VEC_ARENA(cities), city));
    return VEC_COUNT(cities) - 1;
}

typedef struct {
    int n;
    VecString cityNames;
    int *dists;
} Map;

static Map readMap(Arena arena, FILE *f) {
    VecString cities;
    VEC_INIT(&cities, arena);
    VEC_PUSH(cities, "START");
    VecDistance distances;
    VEC_INIT(&distances, arena);
    VecString words;
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) == 5) {
        size_t city0 = cityIndex(cities, VEC_ELEMS(words)[0]);
        auto city1 = cityIndex(cities, VEC_ELEMS(words)[2]);
        // NOLINTNEXTLINE(cert-err34-c)
        int dist = atoi(VEC_ELEMS(words)[4]);
        VEC_PUSH(distances, ((Distance){city0, city1, dist}));
    }
    int count = (int)VEC_COUNT(cities);
    Map map = {count, cities, arenaAlloc(arena, count * count, sizeof(*map.dists))};
    for (size_t i = 0; i < VEC_COUNT(distances); i++) {
        Distance d = VEC_ELEMS(distances)[i];
        map.dists[d.city0 * count + d.city1] = d.dist;
        map.dists[d.city1 * count + d.city0] = d.dist;
    }
    return map;
}

static int tsp(Map m, int pos, int visited) {
    visited |= 1 << pos;
    int best = INT_MAX;
    for (int next = 1; next < m.n; next++) {
        if (visited & (1 << next)) {
            continue;
        }
        best = min(best, m.dists[pos * m.n + next] + tsp(m, next, visited));
    }
    return best == INT_MAX ? 0 : best;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    return tsp(readMap(arena, f), 0, 0);
}

static int64_t solvePart1(Arena arena, FILE *f) {
    Map m = readMap(arena, f);
    for (int i = 0; i < m.n * m.n; i++) {
        m.dists[i] *= -1;
    }
    return -tsp(m, 0, 0);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-09/input-ex0.txt", solvePart0, 605, solvePart1, 982);
    failed += checkInputInt("day-09/input-real0.txt", solvePart0, 207, solvePart1, 804);
    return failed;
}

daySolver day09 = {9, dayMain, true};
