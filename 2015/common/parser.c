#include "parser.h"

#include <stdio.h>
#include <string.h>

#include "common/common.h"
#include "common/gmap.h"
#include "common/list.h"
#include "common/vec.h"

// Based on https://joshuagrams.github.io/pep

typedef struct {
    Rule rule;
    size_t dot;
} LR0;

static char *nextSymbol(LR0 lr0) {
    return lr0.dot < VEC_COUNT(lr0.rule->production)
        ? VEC_ELEMS(lr0.rule->production)[lr0.dot]
        : nullptr;
}

typedef struct {
    bool isLR0;

    union {
        char *symbol;
        LR0 lr0;
    };
} Tag;

static Tag advance(LR0 lr0) {
    if (lr0.dot + 1 == VEC_COUNT(lr0.rule->production)) {
        return (Tag){.isLR0 = false, {.symbol = lr0.rule->symbol}};
    }
    return (Tag){.isLR0 = true, {.lr0 = {lr0.rule, lr0.dot + 1}}};
}

typedef VEC(struct EarleyItem_impl *) VecEarleyItem;

typedef struct {
    Grammar grammar;
    size_t position; // index into the input sequence
    VecEarleyItem items; // items for iteration/processing
    GMap idx; // IdxKey -> EarleyItem: items by tag and start for uniqueness
    GMap wants; // string -> VecEarleyItem: incomplete items by next symbol
} *EarleySet;

static int icmp(uintptr_t l, uintptr_t r) { return l < r ? -1 : l > r ? 1 : 0; }

static int tagCmp(Tag l, Tag r) {
    if (!l.isLR0 && !r.isLR0) {
        return strcmp(l.symbol, r.symbol);
    }
    if (l.isLR0 && r.isLR0) {
        return icmp((uintptr_t)l.lr0.rule, (uintptr_t)r.lr0.rule)
            ? : icmp(l.lr0.dot, r.lr0.dot);
    }
    return icmp(l.isLR0, r.isLR0);
}

static int strcmpVoid(const void *a, const void *b) { return strcmp(a, b); }

typedef struct EarleyItem_impl *EarleyItem;
typedef struct DerivationList_impl *DerivationList;

struct DerivationList_impl {
    EarleyItem left, down;
    DerivationList next;
    Rule rule;
};

struct EarleyItem_impl {
    Tag tag;
    EarleySet start;
    EarleySet end;
    DerivationList derivations;
};

[[maybe_unused]]
static void printEarleyItem(EarleyItem item);
[[maybe_unused]]
static void printEarleySet(EarleySet set);

static int itemCmp(const void *l, const void *r) {
    const struct EarleyItem_impl *il = l, *ir = r;
    return tagCmp(il->tag, ir->tag)
        ? : icmp((uintptr_t)il->start, (uintptr_t)ir->start)
        ? : icmp((uintptr_t)il->end, (uintptr_t)ir->end);
}

static EarleyItem mkEarleyItem(Tag tag, EarleySet start, EarleySet end) {
    EarleyItem item = arenaAlloc(start->grammar->arena, 1, sizeof(*item));
    item->tag = tag;
    item->start = start;
    item->end = end;
    return item;
}

static EarleySet mkEarleySet(Grammar grammar, size_t position) {
    EarleySet set = arenaAlloc(grammar->arena, 1, sizeof(*set));
    set->grammar = grammar;
    set->position = position;
    VEC_INIT(&set->items, grammar->arena);
    set->idx = gmapEmpty(grammar->arena, itemCmp);
    set->wants = gmapEmpty(grammar->arena, strcmpVoid);
    return set;
}

static EarleyItem appendItem(EarleyItem item) {
    Tag tag = item->tag;
    EarleySet start = item->start;
    EarleySet end = item->end;
    VEC_PUSH(end->items, item);
    end->idx = gmapInsert(end->idx, mkEarleyItem(tag, start, end), item).map;
    if (tag.isLR0) {
        VecEarleyItem wants;
        char *next = nextSymbol(tag.lr0);
        auto lookupRes = gmapLookup(end->wants, next);
        if (lookupRes.found) {
            wants = VEC_OF_PTR(VecEarleyItem, lookupRes.value);
        } else {
            VEC_INIT(&wants, start->grammar->arena);
            end->wants = gmapInsert(end->wants, next, PTR_OF_VEC(wants)).map;
        }
        VEC_PUSH(wants, item);
    }
    return item;
}

static void printRule(Rule rule, size_t dot);

static EarleyItem addItem(Tag tag, EarleySet start, EarleySet end) {
    EarleyItem item = mkEarleyItem(tag, start, end);
    auto lookupRes = gmapLookup(end->idx, item);
    return lookupRes.found ? lookupRes.value : appendItem(item);
}

void addDerivation(EarleyItem item, EarleyItem left, EarleyItem down, Rule rule) {
    Arena arena = item->end->grammar->arena;
    for (DerivationList search = item->derivations; search; search = search->next) {
        if (left == search->left && down == search->down && rule == search->rule) {
            return;
        }
    }
    DerivationList deriv = arenaAlloc(arena, 1, sizeof(*deriv));
    deriv->left = left;
    deriv->down = down;
    deriv->rule = rule;
    deriv->next = item->derivations;
    item->derivations = deriv;
}

static EarleySet predict(EarleySet set, const char *nonTerminal);
static void complete(EarleyItem completed);
static EarleySet scan(EarleySet set1, char *terminal);
static EarleySet process(EarleySet set);

ParseForest collectForest(Arena arena, GMap *cache, EarleyItem item);
void vizItem(Arena arena, EarleyItem item);

struct Parse_impl {
    Arena arena;
    EarleyItem root;
};

Parse parse(Grammar grammar, VecString tokenStrings) {
    EarleySet s0 = process(predict(mkEarleySet(grammar, 0), grammar->start));
    EarleySet s = s0;
    VEC_FOR(tokenPtr, tokenStrings) {
        s = process(scan(s, *tokenPtr));
    }

    auto res = gmapLookup(s->idx, mkEarleyItem((Tag){.isLR0 = false, .symbol = grammar->start}, s0, s));
    if (res.found) {
        Parse result = arenaAlloc(grammar->arena, 1, sizeof(*result));
        result->arena = grammar->arena;
        result->root = res.value;
        return result;
    }
    return nullptr;
}

ParseForest parseForest(Parse parse) {
    GMap cache = gmapEmpty(parse->arena, itemCmp);
    return collectForest(parse->arena, &cache, parse->root);
}

static EarleySet predict(EarleySet set, const char *nonTerminal) {
    VEC_FOR(rulePtr, set->grammar->rules) {
        if (strcmp((*rulePtr)->symbol, nonTerminal) != 0) { continue; }
        if (VEC_COUNT((*rulePtr)->production) > 0) {
            addItem((Tag){.isLR0 = true, .lr0 = {*rulePtr, 0}}, set, set);
        } else {
            // completed nullable rule
            auto item = addItem((Tag){.isLR0 = false, .symbol = (*rulePtr)->symbol}, set, set);
            addDerivation(item, nullptr, nullptr, *rulePtr);
        }
    }
    return set;
}

static void complete(EarleyItem completed) {
    check(!completed->tag.isLR0);
    auto wantsPtr = gmapLookup(completed->start->wants, completed->tag.symbol).value;
    if (!wantsPtr) { return; }
    auto wants = VEC_OF_PTR(VecEarleyItem, wantsPtr);
    for (size_t i = 0; i < VEC_COUNT(wants); i++) {
        EarleyItem item = VEC_ELEMS(wants)[i];
        check(item->tag.isLR0);
        EarleyItem added = addItem(advance(item->tag.lr0), item->start, completed->end);
        addDerivation(added, item, completed, item->tag.lr0.rule);
    }
}

static EarleySet scan(EarleySet set1, char *terminal) {
    auto set2 = mkEarleySet(set1->grammar, set1->position + 1);
    addItem((Tag){.isLR0 = false, .symbol = terminal}, set1, set2);
    return set2;
}

static EarleySet process(EarleySet set) {
    size_t old;
    do {
        old = VEC_COUNT(set->items);
        for (size_t k = 0; k < VEC_COUNT(set->items); k++) {
            auto item = VEC_ELEMS(set->items)[k];
            if (item->tag.isLR0) {
                predict(set, nextSymbol(item->tag.lr0));
            } else {
                complete(item);
            }
        }
    } while (VEC_COUNT(set->items) > old);
    return set;
}

static void printRule(Rule rule, size_t dot) {
    fprintf(stderr, "%s ::=", rule->symbol);
    VEC_FORI(e, rule->production) {
        fprintf(stderr, " %s%s", e.i == dot ? "[] " : "", *e.ptr);
    }
    fprintf(stderr, "\n");
}

void printGrammar(Grammar grammar) {
    fprintf(stderr, "<START> ::= %s\n", grammar->start);
    VEC_FOR(rule, grammar->rules) {
        printRule(*rule, SIZE_MAX);
    }
}

[[maybe_unused]]
static void printEarleyItem(EarleyItem item) {
    fprintf(stderr, "[%llu-%llu]    ", item->start->position, item->end->position);
    if (item->tag.isLR0) {
        printRule(item->tag.lr0.rule, item->tag.lr0.dot);
    } else {
        fprintf(stderr, "complete(%s)\n", item->tag.symbol);
    }
}

static void printEarleySet(EarleySet set) {
    fprintf(stderr, "=== %llu ===\n", set->position);
    VEC_FOR(item, set->items) {
        printEarleyItem(*item);
    }
}

typedef LIST(EarleyItem) ListEarleyItem;

typedef struct {
    Rule rule;
    ListEarleyItem items;
} EarleyProduction;

typedef VEC(EarleyProduction) VecEarleyProduction;

VecEarleyProduction getRuleDerivs(EarleyItem item, VecEarleyProduction downs) {
    Arena arena = item->end->grammar->arena;
    if (item->tag.isLR0 && item->tag.lr0.dot == 0) {
        return downs;
    }
    VecEarleyProduction results;
    VEC_INIT(&results, arena);
    for (DerivationList deriv = item->derivations; deriv; deriv = deriv->next) {
        if (deriv->left) {
            VecEarleyProduction newDowns;
            VEC_INIT_AND_FILL(
                &newDowns, arena, VEC_COUNT(downs),
                ((EarleyProduction){ .rule = deriv->rule, .items = LIST_NIL(ListEarleyItem) })
            );
            VEC_FORI(d, downs) {
                check(!d.ptr->rule || deriv->rule == d.ptr->rule);
                VEC_ELEMS(newDowns)[d.i].items = LIST_CONS(arena, deriv->down, d.ptr->items);
            }
            VecEarleyProduction subResults = getRuleDerivs(deriv->left, newDowns);
            VEC_FOR(subResultPtr, subResults) {
                VEC_PUSH(results, *subResultPtr);
            }
        } else {
            VEC_FOR(subResultPtr, downs) {
                VEC_PUSH(results, *subResultPtr);
            }
        }
    }
    return results;
}

void vizItem(Arena arena, EarleyItem item) {
    fprintf(stderr, "<table>\n");
    if (!item->tag.isLR0) {
        bool isTerminal = !item->derivations; //!item->rule;
        fprintf(
            stderr, "<tr><th colspan=\"%llu\"%s>%s</th></tr>\n",
            1ULL, //isTerminal ? 1 : VEC_COUNT(item->rule->production),
            isTerminal ? " style=\"color: red;\"" : "",
            item->tag.symbol
        );
    }

    VecEarleyProduction startingDowns;
    VEC_INIT_AND_FILL(&startingDowns, arena, 1, ((EarleyProduction){.rule=nullptr,.items=LIST_NIL(ListEarleyItem)}));
    auto parse = getRuleDerivs(item, startingDowns);
    VEC_FOR(subItems, parse) {
        fprintf(stderr, "<tr>\n");
        for (ListEarleyItem l = subItems->items; !LIST_IS_NIL(l); l = LIST_CDR(l)) {
            fprintf(stderr, "<td>\n");
            vizItem(arena, LIST_CAR(l));
            fprintf(stderr, "</td>\n");
        }
        fprintf(stderr, "</tr>\n");
    }
    fprintf(stderr, "</table>\n");
}

ParseForest mkForest(Arena arena, char *symbol, bool isTerminal, VecProductionParse productions) {
    ParseForest forest = arenaAlloc(arena, 1, sizeof(*forest));
    forest->symbol = symbol;
    forest->isTerminal = isTerminal;
    forest->productions = productions;
    return forest;
}

void ind(size_t indent) {
    for (size_t i = 0; i < indent; i++) { fputc(' ', stderr); }
}

ParseForest collectForest(Arena arena, GMap *cache, EarleyItem item) {
    auto lookupRes = gmapLookup(*cache, item);
    if (lookupRes.found) {
        return lookupRes.value;
    }

    check(!item->tag.isLR0);
    VecProductionParse productions;
    VEC_INIT(&productions, arena);
    VecEarleyProduction startingDowns;
    VEC_INIT_AND_FILL(&startingDowns, arena, 1, ((EarleyProduction){ .rule=nullptr, .items=LIST_NIL(ListEarleyItem) }));
    auto parse = getRuleDerivs(item, startingDowns);
    VEC_FORI(alternate, parse) {
        ProductionParse prods = arenaAlloc(arena, 1, sizeof(*prods));
        prods->rule = alternate.ptr->rule;
        VEC_INIT(&prods->sequence, arena);
        for (ListEarleyItem items = alternate.ptr->items; !LIST_IS_NIL(items); items = LIST_CDR(items)) {
            EarleyItem z = LIST_CAR(items);
            VEC_PUSH(prods->sequence, collectForest(arena, cache, z));
        }
        VEC_PUSH(productions, prods);
    }

    auto result = mkForest(arena, item->tag.symbol, VEC_COUNT(productions) == 0, productions);
    *cache = gmapInsert(*cache, item, result).map;
    return result;
}
