#pragma once
#include "arena.h"
#include "vec.h"

typedef struct {
    bool isTerminal;
    const char *name;
} GrammarSymbol;

typedef VEC(GrammarSymbol) VecGrammarSymbol;

typedef struct Grammar_impl *Grammar;

typedef struct ParseTreeImpl *ParseTree;

typedef VEC(ParseTree) ParseForest;

struct ParseTreeImpl {
    const char *symbol;
    bool isTerminal;
    ParseForest forest;
};

Grammar grammarCreate(Arena arena);
void grammarAddRule(Grammar grammar, const char *nonTerminalStr, VecGrammarSymbol production);
void printGrammar(Grammar grammar);
ParseForest parse(Grammar grammar, const char *startSymbol, VecString tokenStrings);
