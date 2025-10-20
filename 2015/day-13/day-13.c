#include <limits.h>
#include <string.h>

#include "common/common.h"
#include "common/vec.h"

typedef struct {
    size_t person0, person1;
    int happy;
} Relationship;
typedef VEC(Relationship) VecRelationship;

static size_t personIndex(VecString people, const char *person) {
    for (size_t i = 0; i < VEC_COUNT(people); i++) {
        if (strcmp(VEC_ELEMS(people)[i], person) == 0) {
            return i;
        }
    }
    VEC_PUSH(people, arenaStrdup(VEC_ARENA(people), person));
    return VEC_COUNT(people) - 1;
}

typedef struct {
    int n;
    VecString personNames;
    int *relationshipMatrix;
} Minefield;

static Minefield readMinefield(Arena arena, FILE *f, bool includeSelf) {
    VecString people;
    VEC_INIT(&people, arena);
    if (includeSelf) {
        VEC_PUSH(people, "<me>");
    }
    VecRelationship relationships;
    VEC_INIT(&relationships, arena);
    char *buf = nullptr;
    ssize_t buflen = 0;
    while (getUntilDelimiter(arena, &buf, &buflen, ' ', f) > 0) {
        auto person0 = personIndex(people, buf);
        fscanf(f, " would ");
        int dist;
        if (fgetc(f) == 'g') {
            // NOLINTNEXTLINE(cert-err34-c)
            check(fscanf(f, "ain %d happiness units by sitting next to ", &dist));
        } else {
            // NOLINTNEXTLINE(cert-err34-c)
            check(fscanf(f, "ose %d happiness units by sitting next to ", &dist));
            dist *= -1;
        }
        check(getUntilDelimiter(arena, &buf, &buflen, '.', f) > 0);
        auto person1 = personIndex(people, buf);
        fscanf(f, "\n");
        VEC_PUSH(relationships, ((Relationship){person0, person1, dist}));
    }
    int count = (int)VEC_COUNT(people);
    Minefield minefield = {count, people, arenaAlloc(arena, count * count, sizeof(*minefield.relationshipMatrix))};
    for (size_t i = 0; i < VEC_COUNT(relationships); i++) {
        Relationship d = VEC_ELEMS(relationships)[i];
        minefield.relationshipMatrix[d.person0 * count + d.person1] = d.happy;
    }
    return minefield;
}

static int tsp(Minefield m, int pos, int visited) {
    visited |= 1 << pos;
    int best = INT_MIN;
    for (int next = 1; next < m.n; next++) {
        if (visited & (1 << next)) {
            continue;
        }
        best = MAX(best, m.relationshipMatrix[pos * m.n + next] + m.relationshipMatrix[next * m.n + pos] + tsp(m, next, visited));
    }
    return best == INT_MIN ? m.relationshipMatrix[pos * m.n + 0] + m.relationshipMatrix[0 * m.n + pos] : best;
}

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    return tsp(readMinefield(arena, f, false), 0, 0);
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    return tsp(readMinefield(arena, f, true), 0, 0);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-13/input-ex0.txt", solvePart0, 330, solvePart1, 286);
    failed += checkInputInt("day-13/input-real0.txt", solvePart0, 664, solvePart1, 640);
    return failed;
}

daySolver day13 = {13, dayMain, true};
