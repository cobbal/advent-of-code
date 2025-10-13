#include "common/common.h"

typedef enum {
    NORMAL,
    STRING,
    BACKSLASH,
    HEX0, HEX1,
    ERROR,
    LexStateMax
} LexState;

typedef struct {
    LexState newState;
    int incrMem;
} Action;

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    Action transitions[LexStateMax][256] = {0};
    for (int i = 0; i < 256; i++) {
        transitions[NORMAL][i] = (Action){NORMAL, 0};
        transitions[STRING][i] = (Action){STRING, 1};
        transitions[BACKSLASH][i] = (Action){ERROR, 0};
        transitions[HEX0][i] = (Action){HEX1, 0};
        transitions[HEX1][i] = (Action){STRING, 1};
        transitions[ERROR][i] = (Action){ERROR, 0};
    }
    transitions[NORMAL]['"'] = (Action){STRING, 0};
    transitions[STRING]['"'] = (Action){NORMAL, 0};
    transitions[STRING]['\\'] = (Action){BACKSLASH, 0};
    transitions[BACKSLASH]['"'] = (Action){STRING, 1};
    transitions[BACKSLASH]['\\'] = (Action){STRING, 1};
    transitions[BACKSLASH]['x'] = (Action){HEX0, 0};

    int c;
    int reprCount = 0, memCount = 0;
    LexState state = NORMAL;
    while ((c = fgetc(f)) != EOF) {
        if (c != '\n') { reprCount++; }
        Action action = transitions[state][c];
        state = action.newState;
        memCount += action.incrMem;
    }
    return reprCount - memCount;
}

static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    int c;
    int reprCount = 0, memCount = 0, inString = 0;
    while ((c = fgetc(f)) != EOF) {
        if (c == '\n') {
            if (inString) { reprCount++; }
            inString = 0;
            continue;
        }
        memCount++;
        if (!inString) { reprCount++; }
        inString = 1;
        switch (c) {
        case '"':
            case'\\':
            reprCount += 2;
            break;
        default:
            reprCount++;
        }
    }
    return reprCount - memCount;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-08/input-ex0.txt", solvePart0, 12, solvePart1, 19);
    failed += checkInputInt("day-08/input-real0.txt", solvePart0, 1350, solvePart1, 2085);
    return failed;
}

daySolver day08 = {8, dayMain, true};
