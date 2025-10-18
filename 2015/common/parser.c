#include "parser.h"

#include <stdio.h>
#include <string.h>

#include "common/common.h"
#include "common/gmap.h"
#include "common/vec.h"

struct Grammar_impl {
    Arena arena;
    VecGrammarSymbol symbols;
    GMap terminalsByName; // char* -> intptr_t
    GMap nonTerminalsByName; // char* -> intptr_t
    VecVecIntptr productions;
};

static int strcmpVoid(const void *a, const void *b) { return strcmp(a, b); }

Grammar grammarCreate(Arena arena) {
    Grammar grammar = arenaAlloc(arena, 1, sizeof(*grammar));
    grammar->arena = arena;
    VEC_INIT(&grammar->symbols, arena);
    grammar->terminalsByName = gmapEmpty(arena, strcmpVoid);
    grammar->nonTerminalsByName = gmapEmpty(arena, strcmpVoid);
    VEC_INIT(&grammar->productions, arena);
    return grammar;
}

static intptr_t lookup(Grammar grammar, const char *name, bool isTerminal) {
    GMap *map = isTerminal ? &grammar->terminalsByName : &grammar->nonTerminalsByName;
    auto lookup = gmapLookup(*map, name);
    intptr_t result = (intptr_t)lookup.value;
    if (!lookup.found) {
        result = (intptr_t)VEC_COUNT(grammar->symbols);
        *map = gmapInsert(*map, name, (void *)result).map;
        VEC_PUSH(grammar->symbols, ((GrammarSymbol){ isTerminal, name }));
    }
    return result;
}

void grammarAddRule(Grammar grammar, const char *nonTerminalStr, VecGrammarSymbol production) {
    intptr_t nonTerminal = lookup(grammar, nonTerminalStr, false);
    VecIntptr productionVec;
    VEC_INIT(&productionVec, grammar->arena);
    VEC_PUSH(productionVec, nonTerminal);
    VEC_FOR(symbol, production) {
        VEC_PUSH(productionVec, lookup(grammar, symbol->name, symbol->isTerminal));
    }
    VEC_PUSH(grammar->productions, productionVec);
}

void printGrammar(Grammar grammar) {
    VEC_FOR(production, grammar->productions) {
        VEC_FORI(prod, *production) {
            if (prod.i == 0) {
                GrammarSymbol nonTerminal = VEC_ELEMS(grammar->symbols)[*prod.ptr];
                fprintf(stderr, "<%s> ::= ", nonTerminal.name);
            } else {
                GrammarSymbol prodSym = VEC_ELEMS(grammar->symbols)[*prod.ptr];
                if (prodSym.isTerminal) {
                    fprintf(stderr, "\"%s\" ", prodSym.name);
                } else {
                    fprintf(stderr, "<%s> ", prodSym.name);
                }
            }
        }
        fprintf(stderr, "\n");
    }
}

typedef const struct State_impl {
    size_t productionIdx;
    size_t productionPos;
    size_t origin;
} *State;

typedef VEC(State) VecState;
typedef VEC(VecState) VecVecState;

static State mkState(Arena arena, size_t productionIdx, size_t productionPos, size_t origin) {
    struct State_impl *ret = arenaAlloc(arena, 1, sizeof(*ret));
    ret->productionIdx = productionIdx;
    ret->productionPos = productionPos;
    ret->origin = origin;
    return ret;
}

static int sizeCmp(size_t l, size_t r) { return l < r ? -1 : l > r ? 1 : 0; }

static int stateCmp(const void *l, const void *r) {
    State sl = l;
    State sr = r;
    return
        sizeCmp(sl->productionIdx, sr->productionIdx) ? :
        sizeCmp(sl->productionPos, sr->productionPos) ? :
        sizeCmp(sl->origin, sr->origin);
}

static bool stateFinished(Grammar g, State s) {
    auto production = VEC_ELEMS(g->productions)[s->productionIdx];
    return s->productionPos + 1 == VEC_COUNT(production);
}

static void debugState(Grammar g, State s) {
    VecIntptr prod = VEC_ELEMS(g->productions)[s->productionIdx];
    VEC_FORI(e, prod) {
        if (e.i == 0) {
            GrammarSymbol nonTerminal = VEC_ELEMS(g->symbols)[*e.ptr];
            fprintf(stderr, "(<%s> ::= ", nonTerminal.name);
        } else {
            GrammarSymbol prodSym = VEC_ELEMS(g->symbols)[*e.ptr];
            if (prodSym.isTerminal) {
                fprintf(stderr, "\"%s\" ", prodSym.name);
            } else {
                fprintf(stderr, "<%s> ", prodSym.name);
            }
        }
        if (e.i == s->productionPos) {
            fprintf(stderr, "[] ");
        }
    }
    fprintf(stderr, ", %llu)\n", s->origin);
}

// based on https://loup-vaillant.fr/tutorials/earley-parsing/parser
static ParseForest parseForestHelp(Grammar g, VecVecState s, intptr_t nonTerm, size_t start, size_t end) {
    Arena arena = g->arena;
    VecState states = VEC_ELEMS(s)[end];
    VEC_FOR(state, states) {
        VecIntptr production = VEC_ELEMS(g->productions)[(*state)->productionIdx];
        if (nonTerm == VEC_ELEMS(production)[0] && start == (*state)->origin) {
            ParseTree tree = arenaAlloc(g->arena, 1, sizeof(*tree));
            tree->symbol = VEC_ELEMS(g->symbols)[nonTerm].name;
            tree->isTerminal = true;

            for (size_t i = 1; i < VEC_COUNT(production); i++) {
                size_t symIdx = VEC_ELEMS(production)[i];
                GrammarSymbol sym = VEC_ELEMS(g->symbols)[symIdx];
            }
        }
    }
}

static ParseForest parseForest(Grammar g, VecGMap s, intptr_t startSymbol) {
    VecVecState completedStates;
    VEC_INIT(&completedStates, g->arena);
    VEC_FOR(e, s) {
        VecState completed;
        VEC_INIT(&completed, g->arena);
        auto kvs = gmapElements(*e);
        VEC_FOR(kv, kvs) {
            if (stateFinished(g, kv->key)) {
                VEC_PUSH(completed, kv->key);
            }
        }
        VEC_PUSH(completedStates, completed);
    }
    return parseForestHelp(g, completedStates, startSymbol, 0, VEC_COUNT(s) - 1);
}

// based on https://en.wikipedia.org/w/index.php?title=Earley_parser&oldid=1316088387#Pseudocode
ParseForest parse(Grammar grammar, const char *startSymbol, VecString tokenStrings) {
    Arena arena = grammar->arena;
    VecIntptr tokens;
    VEC_INIT_AND_FILL(&tokens, arena, VEC_COUNT(tokenStrings), 0);
    VEC_FORI(tok, tokenStrings) {
        VEC_ELEMS(tokens)[tok.i] = lookup(grammar, *tok.ptr, true);
    }

    // function INIT(words)
    //     S ← CREATE_ARRAY(LENGTH(words) + 1)
    //     for k ← from 0 to LENGTH(words) do
    //         S[k] ← EMPTY_ORDERED_SET
    VecGMap s;
    VEC_INIT_AND_FILL(&s, arena, VEC_COUNT(tokens) + 1, gmapEmpty(arena, stateCmp));

    // function EARLEY_PARSE(words, grammar)
    //     INIT(words)
    check(gmapLookup(grammar->nonTerminalsByName, startSymbol).found);
    auto startSymbolIdx = (intptr_t)gmapLookup(grammar->nonTerminalsByName, startSymbol).value;
    VEC_FORI(prod, grammar->productions) {
        // nullable grammars not currently supported. Detect them and fail.
        check(VEC_COUNT(*prod.ptr) > 1);
        //     ADD_TO_SET((γ → •S, 0), S[0])
        if (VEC_ELEMS(*prod.ptr)[0] == startSymbolIdx) {
            VEC_ELEMS(s)[0] = gmapInsert(VEC_ELEMS(s)[0], mkState(arena, prod.i, 0, 0), nullptr).map;
        }
    }
    //     for k ← from 0 to LENGTH(words) do
    for (size_t k = 0; k <= VEC_COUNT(tokens); k++) {
        //         for each state in S[k] do  // S[k] can expand during this loop
        VecState skQueue;
        VEC_INIT_AND_FILL(&skQueue, arena, gmapCount(VEC_ELEMS(s)[k]), nullptr);
        {
            VecGKeyValue elements = gmapElements(VEC_ELEMS(s)[k]);
            VEC_FORI(elem, elements) {
                VEC_ELEMS(skQueue)[elem.i] = elem.ptr->key;
            }
        }

        //         for each state in S[k] do  // S[k] can expand during this loop
        for (size_t stateIdx = 0; stateIdx < VEC_COUNT(skQueue); stateIdx++) {
            State state = VEC_ELEMS(skQueue)[stateIdx];
            VecIntptr production = VEC_ELEMS(grammar->productions)[state->productionIdx];
            //             if not FINISHED(state) then
            if (!stateFinished(grammar, state)) {
                intptr_t nextSymbolIdx = VEC_ELEMS(production)[state->productionPos + 1];
                GrammarSymbol nextSymbol = VEC_ELEMS(grammar->symbols)[nextSymbolIdx];
                //                 if NEXT_ELEMENT_OF(state) is a nonterminal then
                if (!nextSymbol.isTerminal) {
                    //                     PREDICTOR(state, k, grammar)         // non_terminal
                    // procedure PREDICTOR((A → α•Bβ, j), k, grammar)
                    //     for each (B → γ) in GRAMMAR_RULES_FOR(B, grammar) do
                    VEC_FORI(prod, grammar->productions) {
                        if (VEC_ELEMS(*prod.ptr)[0] != nextSymbolIdx) { continue; }
                        //         ADD_TO_SET((B → •γ, k), S[k])
                        State nextState = mkState(arena, prod.i, 0, k);
                        auto insertResult = gmapInsert(VEC_ELEMS(s)[k], nextState, nullptr);
                        VEC_ELEMS(s)[k] = insertResult.map;
                        if (!insertResult.replaced) {
                            VEC_PUSH(skQueue, nextState);
                        }
                    }
                } else {
                    //                 else do
                    //                     SCANNER(state, k, words)             // terminal
                    // procedure SCANNER((A → α•aβ, j), k, words)
                    //     if j < LENGTH(words) and a ⊂ PARTS_OF_SPEECH(words[k]) then
                    if (state->origin < VEC_COUNT(tokens) && nextSymbolIdx == VEC_ELEMS(tokens)[k]) {
                        //         ADD_TO_SET((A → αa•β, j), S[k+1])
                        State nextState = mkState(arena, state->productionIdx, state->productionPos + 1, state->origin);
                        VEC_ELEMS(s)[k + 1] = gmapInsert(VEC_ELEMS(s)[k + 1], nextState, nullptr).map;
                    }
                }
            } else {
                //             else do
                //                 COMPLETER(state, k)
                // procedure COMPLETER((B → γ•, x), k)
                //     for each (A → α•Bβ, j) in S[x] do
                VecGKeyValue originStates = gmapElements(VEC_ELEMS(s)[state->origin]);
                VEC_FOR(elems, originStates) {
                    State originState = elems->key;
                    VecIntptr originProd = VEC_ELEMS(grammar->productions)[originState->productionIdx];
                    if (
                        originState->productionPos + 1 < VEC_COUNT(originProd) &&
                        VEC_ELEMS(originProd)[originState->productionPos + 1] == VEC_ELEMS(production)[0]
                    ) {
                        //         ADD_TO_SET((A → αB•β, j), S[k])
                        State nextState = mkState(
                            arena, originState->productionIdx, originState->productionPos + 1, originState->origin
                        );
                        auto insertResult = gmapInsert(VEC_ELEMS(s)[k], nextState, nullptr);
                        VEC_ELEMS(s)[k] = insertResult.map;
                        if (!insertResult.replaced) {
                            VEC_PUSH(skQueue, nextState);
                        }
                    }
                }
            }
        }
    }
    
    for (size_t k = 0; k <= VEC_COUNT(tokens); k++) {
        fprintf(stderr, "=== %llu ===\n", k);
        auto elems = gmapElements(VEC_ELEMS(s)[k]);
        VEC_FOR(e, elems) {
            if (stateFinished(grammar, e->key)) {
                debugState(grammar, e->key);
            }
        }
    }

    auto elems = gmapElements(VEC_ELEMS(s)[VEC_COUNT(s) - 1]);
    // VEC_FOR(e, elems) {
    //     debugState(grammar, e->key);
    // }
    VEC_FOR(e, elems) {
        State state = e->key;
        VecIntptr prod = VEC_ELEMS(grammar->productions)[state->productionIdx];
        if (state->origin == 0 && VEC_ELEMS(prod)[0] == startSymbolIdx && stateFinished(grammar, state)) {
            // debugState(grammar, e->key);
        }
    }
    return VEC_OF_PTR(ParseForest, nullptr);
}
