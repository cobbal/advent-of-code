#include <string.h>
#include "common/common.h"

static int64_t solvePart0(FILE *f) {
    int64_t nice = 0;
    int c = 0;
    int vowels = 0;
    int forbidden = 0;
    int doubles = 0;
    while (true) {
        int prev = c;
        switch (c = fgetc(f)) {
            case EOF:
                nice += vowels > 2 && forbidden == 0 && doubles > 0;
                return nice;
            case '\n':
                nice += vowels > 2 && forbidden == 0 && doubles > 0;
                vowels = forbidden = doubles = 0;
                continue;
            case 'a':
            case 'e':
            case 'i':
            case 'o':
            case 'u':
                vowels++;
            default:
                if (prev == c) { doubles++; }
                if (prev == 'a' && c == 'b') { forbidden++; }
                if (prev == 'c' && c == 'd') { forbidden++; }
                if (prev == 'p' && c == 'q') { forbidden++; }
                if (prev == 'x' && c == 'y') { forbidden++; }
        }
    }
}

static int64_t solvePart1(FILE *f) {
    int64_t nice = 0;
    int prevIdx = 0, prev = 0, c = 0;
    int hasGap = 0;
    uint8_t pairs[256 * 256] = {0};
    while (true) {
        int prev2 = prev;
        prev = c;
        switch (c = fgetc(f)) {
            case EOF:
            case '\n':
                nice += memchr(pairs, 2, sizeof(pairs)) != nullptr && hasGap;
                hasGap = false;
                prevIdx = prev = 0;
                memset(pairs, 0, sizeof(pairs));
                if (c == EOF) { return nice; }
                continue;
            default:
                if (prev2 == c) { hasGap = 1; }
                int idx = 256 * prev + c;
                if (prevIdx == idx) {
                    prevIdx = 0;
                    continue;
                }
                prevIdx = idx;
                pairs[idx] = min(2, pairs[idx] + 1);
        }
    }
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-05/input-ex0.txt", solvePart0, 2, solvePart1, 0);
    failed += checkInputInt("day-05/input-ex1.txt", solvePart0, 0, solvePart1, 2);
    failed += checkInputInt("day-05/input-real0.txt", solvePart0, 236, solvePart1, 51);
    return failed;
}

daySolver day05 = {5, dayMain, true};
