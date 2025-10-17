#include "parser.h"

#include <string.h>

#include "common/gmap.h"
#include "common/vec.h"

struct Grammar_impl {
    Arena arena;
    VecGrammarSymbol symbols;
    GMap terminalsByName; // char* -> intptr_t
    GMap nonTerminalsByName; // char* -> intptr_t
    GMap productions; // intptr_t -> vec_GrammarSymbol
};

static int ptrCmp(const void *a, const void *b) { return a < b ? -1 : a > b ? 1 : 0; }
static int strcmpVoid(const void *a, const void *b) { return strcmp(a, b); }

Grammar grammarGreate(Arena arena) {
    Grammar grammar = arenaAlloc(arena, 1, sizeof(*grammar));
    grammar->arena = arena;
    VEC_INIT(&grammar->symbols, arena);
    grammar->terminalsByName = gmapEmpty(arena, strcmpVoid);
    grammar->nonTerminalsByName = gmapEmpty(arena, strcmpVoid);
    grammar->productions = gmapEmpty(arena, ptrCmp);
    return grammar;
}

void grammarAddRule(Grammar grammar, const char *nonTerminal, VecGrammarSymbol production) {
}

bool parse(Grammar grammar, const char **tokens) {}
