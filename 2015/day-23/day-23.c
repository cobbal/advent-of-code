#include <stdlib.h>
#include <string.h>

#include "common/common.h"

typedef struct {
    enum { HLF, TPL, INC, JMP, JIE, JIO } op;

    char reg;
    int offset;
} Instruction;

typedef VEC(Instruction) Program;

[[maybe_unused]]
static void debugInstruction(Instruction instr) {
    char *opNames[] = { "hlf", "tpl", "inc", "jmp", "jie", "jio" };
    fprintf(stderr, "%s %c %i\n", opNames[instr.op], instr.reg, instr.offset);
}

Program readProgram(Arena arena, FILE *f) {
    Program prog;
    VEC_INIT(&prog, arena);
    VecString words;
    VEC_INIT(&words, arena);
    while (readLineWords(arena, f, &words) && VEC_COUNT(words) >= 2) {
        Instruction instr = {};
        char *opStr = VEC_ELEMS(words)[0];
        char reg = VEC_ELEMS(words)[1][0];
        // NOLINTNEXTLINE(cert-err34-c)
        int offset = atoi(VEC_END(words)[-1]);
        if (strcmp(opStr, "hlf") == 0) {
            instr.op = HLF;
            instr.reg = reg;
        } else if (strcmp(opStr, "tpl") == 0) {
            instr.op = TPL;
            instr.reg = reg;
        } else if (strcmp(opStr, "inc") == 0) {
            instr.op = INC;
            instr.reg = reg;
        } else if (strcmp(opStr, "jmp") == 0) {
            instr.op = JMP;
            instr.offset = offset;
        } else if (strcmp(opStr, "jie") == 0) {
            instr.op = JIE;
            instr.reg = reg;
            instr.offset = offset;
        } else if (strcmp(opStr, "jio") == 0) {
            instr.op = JIO;
            instr.reg = reg;
            instr.offset = offset;
        } else {
            fprintf(stderr, "bad op: <%s>\n", opStr);
            exit(1);
        }
        VEC_PUSH(prog, instr);
    }
    return prog;
}

static size_t step(Program program, int64_t regFile[2], size_t pc) {
    check(pc < VEC_COUNT(program));
    Instruction instr = VEC_ELEMS(program)[pc];
    int64_t *reg = &regFile[instr.reg == 'b' ? 1 : 0];
    switch(instr.op) {
    case HLF:
        *reg /= 2;
        return pc + 1;
    case TPL:
        *reg *= 3;
        return pc + 1;
    case INC:
        (*reg)++;
        return pc + 1;
    case JMP:
        return pc + instr.offset;
    case JIE:
        return (*reg % 2) == 0 ? pc + instr.offset : pc + 1;
    case JIO:
        return *reg == 1 ? pc + instr.offset : pc + 1;
    }
    return ~0;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    Program program = readProgram(arena, f);
    int64_t regFile[2] = {};
    for (size_t pc = 0; pc < VEC_COUNT(program);) {
        pc = step(program, regFile, pc);
    }
    return regFile[1];
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    Program program = readProgram(arena, f);
    int64_t regFile[2] = {1, 0};
    for (size_t pc = 0; pc < VEC_COUNT(program);) {
        pc = step(program, regFile, pc);
    }
    return regFile[1];
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-23/input-ex0.txt", solvePart0, 0, solvePart1, 0);
    failed += checkInputInt("day-23/input-real0.txt", solvePart0, 184, solvePart1, 231);
    return failed;
}

daySolver day23 = {23, dayMain, true};
