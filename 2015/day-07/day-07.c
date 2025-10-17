#include <stdlib.h>
#include <string.h>

#include "common/common.h"

typedef struct {
    bool isConstant;

    union {
        uint16_t constant;
        int64_t wire;
    };
} Wire;

typedef struct {
    enum {
        eof = -1,
        CONST = 1,
        NOT,
        AND, OR,
        LSHIFT, RSHIFT,
    } tag;

    Wire in0, in1;
    int64_t out;
} Gate;
typedef VEC(Gate) VecGate;

Wire getWire(char *buf) {
    Wire ret = {};
    if ('a' <= *buf && *buf <= 'z') {
        ret.isConstant = false;
        strncpy((char *) &ret.wire, buf, 7);
    } else if ('0' <= *buf && *buf <= '9') {
        ret.isConstant = true;
        // NOLINTNEXTLINE(cert-err34-c)
        ret.constant = atoi(buf);
    } else {
        fprintf(stderr, "bad wire <%s>\n", buf);
        exit(1);
    }
    return ret;
}

Gate getGate(Arena arena, FILE *f, char **buf, ssize_t *bufLen) {
    Gate ret = {};
    if (getUntilDelimiter(arena, buf, bufLen, ' ', f) <= 0) {
        ret.tag = eof;
        return ret;
    }
    char peek = (*buf)[0];
    if (peek == 'N') {
        ret.tag = NOT;
        getUntilDelimiter(arena, buf, bufLen, ' ', f);
        ret.in0 = getWire(*buf);
    } else {
        ret.in0 = getWire(*buf);
        getUntilDelimiter(arena, buf, bufLen, ' ', f);
        peek = (*buf)[0];
        if (peek == 'A') {
            ret.tag = AND;
            getUntilDelimiter(arena, buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'O') {
            ret.tag = OR;
            getUntilDelimiter(arena, buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'L') {
            ret.tag = LSHIFT;
            getUntilDelimiter(arena, buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'R') {
            ret.tag = RSHIFT;
            getUntilDelimiter(arena, buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == '-') {
            ret.tag = CONST;
        }
    }
    if (ret.tag != CONST) {
        fscanf(f, "-> ");
    }
    getUntilDelimiter(arena, buf, bufLen, '\n', f);
    strncpy((char *) &ret.out, *buf, 7);
    return ret;
}

uint16_t compute(Gate *gates, Wire wire) {
    if (wire.isConstant) { return wire.constant; }
    Gate g = gates[wire.wire];
    if (g.out != -1) { return g.out; }
    uint16_t result;
    switch (g.tag) {
    case CONST:
        result = compute(gates, g.in0);
        break;
    case NOT:
        result = ~compute(gates, g.in0);
        break;
    case AND:
        result = compute(gates, g.in0) & compute(gates, g.in1);
        break;
    case OR:
        result = compute(gates, g.in0) | compute(gates, g.in1);
        break;
    case LSHIFT:
        result = compute(gates, g.in0) << compute(gates, g.in1);
        break;
    case RSHIFT:
        result = compute(gates, g.in0) >> compute(gates, g.in1);
        break;
    case eof:
    default:
        fprintf(stderr, "internal error on wire %s\n", (char *) &wire);
        exit(1);
    }
    gates[wire.wire].out = result;
    return result;
}

static void printWire(FILE *f, Wire wire) {
    if (wire.isConstant) {
        fprintf(f, "%d", wire.constant);
    } else {
        fprintf(f, "%s", (char *) &wire.wire);
    }
}

[[maybe_unused]]
static void printGate(FILE *f, Gate g) {
    printWire(f, g.in0);
    fprintf(f, " %s ", (char *[]){"eof", "??0", "CONST", "NOT", "AND", "OR", "LSHIFT", "RSHIFT"}[g.tag + 1]);
    printWire(f, g.in1);
    fprintf(f, " -> %s\n", (char *) &g.out);
}

static struct SolveCommonResult {
    VecGate gates, gatesByOutput;
} parseInput(Arena arena, FILE *f) {
    char *buf = nullptr;
    ssize_t bufLength = 0;
    VecGate gates;
    VEC_INIT(&gates, arena);
    for (Gate g; (g = getGate(arena, f, &buf, &bufLength)).tag != eof;) {
        // printGate(stderr, g);
        VEC_PUSH(gates, g);
    }
    int64_t maxGate = 'b';
    VEC_FOR(gate, gates) {
        maxGate = max(maxGate, gate->out);
        if (maxGate > 0xffff) {
            printf("BIG GATE\n");
        }
    }
    // TODO: stupidly sparse
    VecGate gatesByOutput;
    VEC_INIT_AND_FILL(&gatesByOutput, arena, maxGate + 1, (Gate){.tag = eof});
    VEC_FOR(gateRef, gates) {
        Gate gate = *gateRef;
        uint64_t out = gate.out;
        gate.out = -1;
        VEC_ELEMS(gatesByOutput)[out] = gate;
    }
    return (struct SolveCommonResult){ gates, gatesByOutput };
}

static int64_t solvePart0(Arena arena, FILE *f) {
    auto input = parseInput(arena, f);
    return compute(VEC_ELEMS(input.gatesByOutput), (Wire){.isConstant = false, .wire = 'a'});
}

static int64_t solvePart1(Arena arena, FILE *f) {
    auto input = parseInput(arena, f);
    uint16_t result = compute(VEC_ELEMS(input.gatesByOutput), (Wire){.isConstant = false, .wire = 'a'});
    VEC_FOR(gate, input.gates) {
        VEC_ELEMS(input.gatesByOutput)[gate->out].out = -1;
    }
    VEC_ELEMS(input.gatesByOutput)['b'].out = result;
    return compute(VEC_ELEMS(input.gatesByOutput), (Wire){.isConstant = false, .wire = 'a'});
}

static int dayMain() {
    int failed = 0;
    // failed += checkInputInt("day-07/input-ex0.txt", solvePart0, 65079, solvePart1, 65079);
    failed += checkInputInt("day-07/input-real0.txt", solvePart0, 46065, solvePart1, 14134);
    return failed;
}

daySolver day07 = {7, dayMain, true};
