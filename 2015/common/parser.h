#pragma once
#include "arena.h"
#include "vec.h"

typedef struct {
    bool isTerminal;
    const char *name;
} GrammarSymbol;

typedef VEC(GrammarSymbol) VecGrammarSymbol;

typedef struct Grammar_impl *Grammar;

Grammar grammarGreate(Arena arena);
void grammarAddRule(Grammar grammar, const char *nonTerminal, VecGrammarSymbol production);
bool parse(Grammar grammar, const char **tokens);
