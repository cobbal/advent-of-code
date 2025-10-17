#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "common/common.h"
#include "common/gmap.h"
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
    (void) words;

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

    return (int64_t) gmapCount(seen);
}

static VecString tokenize(Arena arena, const char *str) {
    VecString tokens;
    VEC_INIT(&tokens, arena);
    while (*str) {
        if (islower(str[1])) {
            char token[] = {str[0], str[1], 0};
            VEC_PUSH(tokens, arenaStrdup(arena, token));
            str += 2;
        } else {
            char token[] = {str[0], 0};
            VEC_PUSH(tokens, arenaStrdup(arena, token));
            str += 1;
        }
    }
    return tokens;
}

typedef struct State_impl {
    char *left;
    VecString production;
    int pos;
    int origin;
} *State;
static State mkState(Arena arena, char *left, VecString production, int pos, int origin)
{
    State ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->left = left;
    ret->production = production;
    ret->pos = pos;
    ret->origin = origin;
    return ret;
}

// static int stateCmp(void *lhs, void *rhs);
//
// static VecGMap initStates(VecString tokens) {
//     VecGMap states;
//     VEC_INIT(&states, VEC_ARENA(tokens));
//     for (int k = 0; k <= VEC_COUNT(tokens); k++) {
//         VEC_PUSH(states, gmapEmpty(VEC_ARENA(tokens), stateCmp));
//     }
// }
//
/*
static void earleyParse(VecString tokens, vec_vec_string grammar, char *startToken) {
    Arena arena = tokens->arena;
    vec_GMap states = initStates(tokens);
    VecString startProduction = vec_string_create(arena);
    VEC_PUSH(startProduction, startToken);
    states->elements[0] = gmapInsert(states->elements[0], mkState(arena, "", startProduction, 0, 0), nullptr).map;
    for (int k = 0; k <= tokens->count; k++) {
        for each state in S[k] do  // S[k] can expand during this loop
            if not FINISHED(state) then
                if NEXT_ELEMENT_OF(state) is a nonterminal then
                    PREDICTOR(state, k, grammar)         // non_terminal
                else do
                    SCANNER(state, k, words)             // terminal
            else do
                COMPLETER(state, k)
        end
    end
    return chart

procedure PREDICTOR((A → α•Bβ, j), k, grammar)
    for each (B → γ) in GRAMMAR_RULES_FOR(B, grammar) do
        ADD_TO_SET((B → •γ, k), S[k])
    end

procedure SCANNER((A → α•aβ, j), k, words)
    if j < LENGTH(words) and a ⊂ PARTS_OF_SPEECH(words[k]) then
        ADD_TO_SET((A → αa•β, j), S[k+1])
    end

procedure COMPLETER((B → γ•, x), k)
    for each (A → α•Bβ, j) in S[x] do
        ADD_TO_SET((A → αB•β, j), S[k])
    end












static void parse(GMap grammar, const char **text, int tokenBudget) {
    if (tokenBudget < 0) { return; }
    if (*text == 
}

static int64_t solvePart1(Arena arena, FILE *f) {
    VecString words;
    GMap grammar = gmapEmpty(arena, strcmp_void);
    while (((words = readLineWords(arena, f))) && words->count == 3) {
        char *key = words->elements[0];
        VecString production = tokenize(arena, words->elements[2]);
        vec_vec_string productions = gmapLookup(grammar, key).value ?: vec_vec_string_create(arena);
        vec_vec_string_push(productions, production);
        grammar = gmapInsert(grammar, key, productions).map;
    }

    // skip the blank line
    (void) words;

    check(((words = readLineWords(arena, f))) && words->count == 1);
    VecString text = tokenize(arena, words->elements[0]);
    VEC_PUSH(text, nullptr);

    return 0;
}
*/

static int64_t solvePart1(Arena arena, FILE *f) {}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-19/input-ex0.txt", solvePart0, 7, solvePart1, 6);
    failed += checkInputInt("day-19/input-real0.txt", solvePart0, 518, solvePart1, -1);
    return failed;
}

daySolver day19 = {19, dayMain, true};
