#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "common/common.h"
#include "common/gmap.h"
#include "common/parser.h"
#include "common/vec.h"

static char *replace(Arena arena, size_t pos, size_t len, const char *text, size_t textLen, const char *replacement) {
    size_t repLen = strlen(replacement);
    char *result = arenaAlloc(arena, textLen + repLen + 1, 1);
    memcpy(result, text, pos);
    memcpy(result + pos, replacement, repLen);
    memcpy(result + pos + repLen, text + pos + len, textLen - pos - len);
    return result;
}

static int strcmp_void(const void *a, const void *b) { return strcmp(a, b); }

static int64_t solvePart0(Arena arena, FILE *f) {
    VecString words;
    VecString replacements;
    VEC_INIT(&replacements, arena);
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) == 3) {
        VEC_PUSH(replacements, VEC_ELEMS(words)[0]);
        VEC_PUSH(replacements, VEC_ELEMS(words)[2]);
    }

    // skip the blank line
    (void)words;

    check(readLineWords(arena, f, &words) && VEC_COUNT(words) == 1);
    char *text = VEC_ELEMS(words)[0];
    size_t textLen = strlen(text);
    GMap seen = gmapEmpty(arena, strcmp_void);
    // fprintf(stderr, "\n");
    for (size_t i = 0; i < textLen; i++) {
        for (size_t j = 0; j < VEC_COUNT(replacements) - 1; j += 2) {
            char *find = VEC_ELEMS(replacements)[j];
            char *replacement = VEC_ELEMS(replacements)[j + 1];
            if (strncmp(find, text + i, strlen(find)) == 0) {
                char *replaced = replace(arena, i, strlen(find), text, textLen, replacement);
                // fprintf(stderr, "%s\n", replaced);
                seen = gmapInsert(seen, replaced, nullptr).map;
            }
        }
    }

    return (int64_t)gmapCount(seen);
}

static VecString tokenize(Arena arena, const char *str) {
    VecString tokens;
    VEC_INIT(&tokens, arena);
    while (*str) {
        char token[3] = {0};
        if (islower(str[1])) {
            token[0] = str[0];
            token[1] = str[1];
            str += 2;
        } else {
            token[0] = str[0];
            str += 1;
        }
        VEC_PUSH(tokens, arenaStrdup(arena, token));
    }
    return tokens;
}

static intptr_t minExpansions(GMap *cache, ParseForest forest) {
    if (forest == nullptr) { return INT32_MAX; }
    if (VEC_COUNT(forest->productions) == 0) { return 0; }
    auto lookupRes = gmapLookup(*cache, forest);
    if (lookupRes.found) {
        return (intptr_t)lookupRes.value;
    }

    intptr_t best = INTPTR_MAX;
    VEC_FOR(prod, forest->productions) {
        int64_t prodBest = 1;
        VEC_FOR(subForest, (*prod)->sequence) {
            prodBest += minExpansions(cache, *subForest);
        }
        best = min(prodBest, best);
    }
    *cache = gmapInsert(*cache, forest, (void *)best).map;
    return best;
}

int ptrcmp(const void *l, const void *r) {
    return l < r ? -1 : l > r ? 1 : 0;
}

static int64_t solvePart1(Arena arena, FILE *f) {
    Grammar g = arenaAlloc(arena, 1, sizeof(*g));
    g->arena = arena;
    VEC_INIT(&g->rules, arena);
    g->start = "e";

    VecString words;
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) == 3) {
        char *nonTerm = VEC_ELEMS(words)[0];
        Rule rule = arenaAlloc(arena, 1, sizeof(*rule));
        rule->symbol = nonTerm;
        VecString tokens = tokenize(arena, VEC_ELEMS(words)[2]);
        rule->production = tokens;
        VEC_PUSH(g->rules, rule);
    }

    // skip the blank line
    (void)words;

    check(readLineWords(arena, f, &words) && VEC_COUNT(words) == 1);
    VecString text = tokenize(arena, VEC_ELEMS(words)[0]);
    VecString textStrings;
    VEC_INIT_AND_FILL(&textStrings, arena, VEC_COUNT(text), nullptr);
    VEC_FORI(e, text) {
        VEC_ELEMS(textStrings)[e.i] = *(e.ptr);
    }

    ParseForest forest = parseForest(parse(g, textStrings));
    GMap cache = gmapEmpty(arena, ptrcmp);
    return minExpansions(&cache, forest);
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-19/input-ex0.txt", solvePart0, 7, solvePart1, 6);
    failed += checkInputInt("day-19/input-real0.txt", solvePart0, 518, solvePart1, 200);
    return failed;
}

daySolver day19 = {19, dayMain, true};
