#include <stdlib.h>
#include <string.h>

#include "common/common.h"
#include "common/vec.h"

typedef enum {
    children, cats, samoyeds, pomeranians, akitas,
    vizslas, goldfish, trees, cars, perfumes,
    nThings,
} Thing;

typedef struct {
    int n;
    int8_t things[nThings];
} Sue;

typedef VEC(Sue) VecSue;

static Thing thingOfString(char *s) {
    if (strcmp(s, "children:") == 0) { return children; }
    if (strcmp(s, "cats:") == 0) { return cats; }
    if (strcmp(s, "samoyeds:") == 0) { return samoyeds; }
    if (strcmp(s, "pomeranians:") == 0) { return pomeranians; }
    if (strcmp(s, "akitas:") == 0) { return akitas; }
    if (strcmp(s, "vizslas:") == 0) { return vizslas; }
    if (strcmp(s, "goldfish:") == 0) { return goldfish; }
    if (strcmp(s, "trees:") == 0) { return trees; }
    if (strcmp(s, "cars:") == 0) { return cars; }
    if (strcmp(s, "perfumes:") == 0) { return perfumes; }
    fprintf(stderr, "unknown thing <%s>\n", s);
    exit(1);
}

static VecSue readSues(Arena arena, FILE *f) {
    VecString words;
    VecSue result;
    VEC_INIT(&result, arena);
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) > 1) {
        Sue sue = {};
        for (int i = 0; i < nThings; i++) {
            sue.things[i] = -1;
        }
        // NOLINTNEXTLINE(cert-err34-c)
        sue.n = atoi(VEC_ELEMS(words)[1]);
        for (size_t i = 2; i + 1 < VEC_COUNT(words); i += 2) {
            // NOLINTNEXTLINE(cert-err34-c)
            sue.things[thingOfString(VEC_ELEMS(words)[i])] = (int8_t)atoi(VEC_ELEMS(words)[i + 1]);
        }
        VEC_PUSH(result, sue);
    }
    return result;
}

static bool suesMatch(Sue target, Sue memory, const int cmp[nThings]) {
    for (Thing i = 0; i < nThings; i++) {
        if (memory.things[i] < 0) { continue; }
        bool match = cmp[i] < 0
            ? target.things[i] > memory.things[i]
            : cmp[i] > 0
            ? target.things[i] < memory.things[i]
            : target.things[i] == memory.things[i];
        if (!match) {
            return false;
        }
    }
    return true;
}

static Sue targetSue = {0, {3, 7, 2, 3, 0, 0, 5, 3, 2, 1}};

static int64_t solvePart0(Arena arena, FILE *f) {
    auto sues = readSues(arena, f);
    int cmp[nThings] = {};
    VEC_FOR(sue, sues) {
        if (suesMatch(targetSue, *sue, cmp)) {
            return sue->n;
        }
    }
    return 0;
}

static int64_t solvePart1(Arena arena, FILE *f) {
    auto sues = readSues(arena, f);
    int cmp[nThings] = {};
    cmp[cats] = cmp[trees] = 1;
    cmp[pomeranians] = cmp[goldfish] = -1;
    VEC_FOR(sue, sues) {
        if (suesMatch(targetSue, *sue, cmp)) {
            return sue->n;
        }
    }
    return 0;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-16/input-real0.txt", solvePart0, 103, solvePart1, 405);
    return failed;
}

daySolver day16 = {16, dayMain, true};
