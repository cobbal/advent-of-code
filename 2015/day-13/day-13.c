#include <string.h>

#include "common/common.h"
#include "common/vec_common.h"

typedef struct {
    size_t person0, person1;
    int happy;
} Relationship;

#define VEC_ELEMENT_TYPE Relationship
#include "common/vec_impl.c"
#undef VEC_ELEMENT_TYPE

static size_t personIndex(vec_string people, const char *person) {
    for (size_t i = 0; i < people->count; i++) {
        if (strcmp(people->elements[i], person) == 0) {
            return i;
        }
    }
    vec_string_push(people, arenaStrdup(people->arena, person));
    return people->count - 1;
}

typedef struct {
    int n;
    vec_string personNames;
    int *relationshipMatrix;
} Minefield;

static Minefield readMinefield(Arena arena, FILE *f, bool includeSelf) {
    vec_string people = vec_string_create(arena);
    if (includeSelf) {
        vec_string_push(people, "<me>");
    }
    vec_Relationship relationships = vec_Relationship_create(arena);
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
        vec_Relationship_push(relationships, (Relationship){person0, person1, dist});
    }
    int count = (int) people->count;
    Minefield minefield = {count, people, arenaAlloc(arena, count * count, sizeof(*minefield.relationshipMatrix))};
    for (size_t i = 0; i < relationships->count; i++) {
        Relationship d = relationships->elements[i];
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
        best = max(best, m.relationshipMatrix[pos * m.n + next] + m.relationshipMatrix[next * m.n + pos] + tsp(m, next, visited));
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
