#pragma once
#include "arena.h"
#include "vec.h"

typedef struct {
    char *symbol;
    VecString production;
} *Rule;

typedef VEC(Rule) VecRule;

typedef struct {
    Arena arena;
    char *start;
    VecRule rules;
} *Grammar;

typedef struct ProductionParse_impl *ProductionParse;

typedef VEC(ProductionParse) VecProductionParse;

typedef struct {
    char *symbol;
    bool isTerminal;
    VecProductionParse productions;
} *ParseForest;

typedef VEC(ParseForest) VecParseForest;

struct ProductionParse_impl {
    Rule rule;
    VecParseForest sequence;
};

typedef struct Parse_impl *Parse;
Parse parse(Grammar grammar, VecString tokenStrings);

ParseForest parseForest(Parse parse);

void printGrammar(Grammar grammar);
