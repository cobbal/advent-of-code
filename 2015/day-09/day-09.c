#include <string.h>

#include "common/common.h"
#include "common/vec_common.h"

typedef struct {
    int city0, city1, dist;
} Distance;

#define VEC_ELEMENT_TYPE Distance
#include "common/vec_impl.c"
#undef VEC_ELEMENT_TYPE

static int cityIndex(vec_string cities, char *city) {
    for (size_t i = 0; i < cities->count; i++) {
        if (strcmp(cities->elements[i], city) == 0) {
            return i;
        }
    }
    vec_string_push(cities, arenaStrdup(cities->arena, city));
    return (int) cities->count - 1;
}

typedef struct {
    int n;
    vec_string cityNames;
    int *dists;
} Map;

static Map readMap(Arena arena, FILE *f) {
    vec_string cities = vec_string_create(arena);
    vec_string_push(cities, "START");
    vec_Distance distances = vec_Distance_create(arena);
    char *buf = nullptr;
    ssize_t buflen = 0;
    while (getUntilDelimiter(arena, &buf, &buflen, ' ', f) > 0) {
        auto city0 = cityIndex(cities, buf);
        fscanf(f, " to ");
        check(getUntilDelimiter(arena, &buf, &buflen, ' ', f) > 0);
        auto city1 = cityIndex(cities, buf);
        int dist;
        // NOLINTNEXTLINE(cert-err34-c)
        check(fscanf(f, " = %d\n", &dist) > 0);
        vec_Distance_push(distances, (Distance){city0, city1, dist});
    }
    int count = (int) cities->count;
    Map map = {count, cities, arenaAlloc(arena, count * count, sizeof(*map.dists))};
    for (size_t i = 0; i < distances->count; i++) {
        Distance d = distances->elements[i];
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
