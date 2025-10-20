#include <stdlib.h>

#include "../common/common.h"

typedef struct {
    enum { eof = -2, on = 1, off = -1, toggle = 2 } op;

    int x0, y0, x1, y1;
} command;

command getCommand(FILE *f) {
    char buf[8] = {};
    command ret = {};
    if (fread(buf, 1, 7, f) != 7) {
        ret.op = eof;
        return ret;
    }
    switch (buf[6]) {
        case 'n': // turn on
            fscanf(f, " ");
            ret.op = on;
            break;
        case 'f': // turn off
            fscanf(f, "f ");
            ret.op = off;
            break;
        case ' ': // toggle
            ret.op = toggle;
            break;
        default:
            ret.op = eof;
            return ret;
    }
    if (fscanf(f, "%d,%d through %d,%d\n", &ret.x0, &ret.y0, &ret.x1, &ret.y1) != 4) {
        ret.op = eof;
    }
    return ret;
}

static int64_t solvePart0(Arena arena, FILE *f) {
    bool *lights = arenaAlloc(arena, 1000 * 1000, sizeof(*lights));
    command c;
    while ((c = getCommand(f)).op != eof) {
        for (int x = c.x0; x <= c.x1; x++) {
            for (int y = c.y0; y <= c.y1; y++) {
                if (c.op == on) {
                    lights[1000 * y + x] = 1;
                } else if (c.op == off) {
                    lights[1000 * y + x] = 0;
                } else {
                    lights[1000 * y + x] ^= 1;
                }
            }
        }
    }
    int64_t lightCount = 0;
    for (int i = 0; i < 1000 * 1000; i++) {
        lightCount += lights[i];
    }
    return lightCount;
}

static int64_t solvePart1(Arena arena, FILE *f) {
    int *lights = arenaAlloc(arena, 1000 * 1000, sizeof(*lights));
    command c;
    while ((c = getCommand(f)).op != eof) {
        for (int x = c.x0; x <= c.x1; x++) {
            for (int y = c.y0; y <= c.y1; y++) {
                lights[1000 * y + x] = MAX(0, lights[1000 * y + x] + c.op);
            }
        }
    }
    int64_t lightCount = 0;
    for (int i = 0; i < 1000 * 1000; i++) {
        lightCount += lights[i];
    }
    return lightCount;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-06/input-ex0.txt", solvePart0, 998996, solvePart1, 1001996);
    failed += checkInputInt("day-06/input-real0.txt", solvePart0, 569999, solvePart1, 17836115);
    return failed;
}

daySolver day06 = {6, dayMain, true};
