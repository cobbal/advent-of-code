#include <stdlib.h>
#include <string.h>

#include "../common/common.h"

typedef struct {
    bool isConstant;

    union {
        uint16_t constant;
        uint64_t wire;
    };
} wire;

typedef struct {
    enum {
        eof = -1,
        CONST = 1,
        NOT,
        AND, OR,
        LSHIFT, RSHIFT,
    } tag;

    wire in0, in1;
    uint64_t out;
} gate;

#define VEC_ELEMENT_TYPE gate
#include "common/vec_impl.c"
#undef VEC_ELEMENT_TYPE

wire getWire(char *buf) {
    wire ret = {0};
    if ('a' <= *buf && *buf <= 'z') {
        ret.isConstant = false;
        strncpy((char *) &ret.wire, buf, 7);
    } else if ('0' <= *buf && *buf <= '9') {
        ret.isConstant = true;
        ret.constant = atoi(buf);
    } else {
        fprintf(stderr, "bad wire <%s>\n", buf);
        exit(1);
    }
    return ret;
}

gate getGate(FILE *f, char **buf, ssize_t *bufLen) {
    gate ret = {0};
    if (getUntilDelimiter(buf, bufLen, ' ', f) <= 0) {
        ret.tag = eof;
        return ret;
    }
    char peek = (*buf)[0];
    if (peek == 'N') {
        ret.tag = NOT;
        getUntilDelimiter(buf, bufLen, ' ', f);
        ret.in0 = getWire(*buf);
    } else {
        ret.in0 = getWire(*buf);
        getUntilDelimiter(buf, bufLen, ' ', f);
        peek = (*buf)[0];
        if (peek == 'A') {
            ret.tag = AND;
            getUntilDelimiter(buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'O') {
            ret.tag = OR;
            getUntilDelimiter(buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'L') {
            ret.tag = LSHIFT;
            getUntilDelimiter(buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == 'R') {
            ret.tag = RSHIFT;
            getUntilDelimiter(buf, bufLen, ' ', f);
            ret.in1 = getWire(*buf);
        } else if (peek == '-') {
            ret.tag = CONST;
        }
    }
    if (ret.tag != CONST) {
        fscanf(f, "-> ");
    }
    getUntilDelimiter(buf, bufLen, '\n', f);
    strncpy((char *) &ret.out, *buf, 7);
    return ret;
}

uint16_t compute(gate *gates, wire wire) {
    if (wire.isConstant) { return wire.constant; }
    gate g = gates[wire.wire];
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

static void solveCommon(FILE *f, vec_gate *gatesPtr, vec_gate *gatesByOutputPtr) {
    char *buf = nullptr;
    ssize_t bufLength = 0;
    vec_gate gates = vec_gate_create();
    *gatesPtr = gates;
    for (gate g; (g = getGate(f, &buf, &bufLength)).tag != eof;) {
        // fprintf(stderr, "%s %d %d/%s -> %s\n", &g.in0, g.tag, g.imm, &g.in1, &g.out);
        vec_gate_push(gates, g);
    }
    int64_t maxGate = 'b';
    for (size_t i = 0; i < gates->count; i++) {
        maxGate = max(maxGate, gates->elements[i].out);
        if (maxGate > 0xffff) {
            printf("BIG GATE\n");
        }
    }
    // TODO: stupidly sparse
    vec_gate gatesByOutput = vec_gate_createAndFill(maxGate + 1, (gate){.tag = eof, {0}});
    *gatesByOutputPtr = gatesByOutput;
    for (size_t i = 0; i < gates->count; i++) {
        gate g = gates->elements[i];
        uint64_t out = g.out;
        g.out = -1;
        gatesByOutput->elements[out] = g;
    }
    free(buf);
}

static int64_t solvePart0(FILE *f) {
    vec_gate gates, gatesByOutput;
    solveCommon(f, &gates, &gatesByOutput);
    uint16_t result = compute(gatesByOutput->elements, (wire){.isConstant = false, .wire = 'a'});
    vec_gate_destroy(gates);
    vec_gate_destroy(gatesByOutput);
    return result;
}

static int64_t solvePart1(FILE *f) {
    vec_gate gates, gatesByOutput;
    solveCommon(f, &gates, &gatesByOutput);

    uint16_t result = compute(gatesByOutput->elements, (wire){.isConstant = false, .wire = 'a'});
    for (size_t i = 0; i < gates->count; i++) {
        gatesByOutput->elements[gates->elements[i].out].out = -1;
    }
    gatesByOutput->elements['b'].out = result;
    result = compute(gatesByOutput->elements, (wire){.isConstant = false, .wire = 'a'});

    vec_gate_destroy(gates);
    vec_gate_destroy(gatesByOutput);
    return result;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-07/input-ex0.txt", solvePart0, 65079, solvePart1, 65079);
    failed += checkInputInt("day-07/input-real0.txt", solvePart0, 46065, solvePart1, 14134);
    return failed;
}

daySolver day07 = {7, dayMain, true};
