#include <string.h>

#include "common/common.h"

static void incr(char *revPass) {
    for (char *c = revPass; *c; c++) {
        switch (++*c) {
        case 'z' + 1:
            *c = 'a';
            break;
        case 'i': case 'o': case 'l':
            ++*c;
            memset(revPass, 'a', c - revPass);
            return;
        default:
            return;
        }
    }
}

static void incr0(char *revPass) {
    char *lastBad = strrchr(revPass, 'i');
    lastBad = max(lastBad, strrchr(revPass, 'o'));
    lastBad = max(lastBad, strrchr(revPass, 'l'));
    if (lastBad) {
        memset(revPass, 'z', lastBad - revPass);
    }
    incr(revPass);
}

static bool valid(const char *fub) {
    bool hasIncrSeq = false;
    int overlap0 = -1, overlap1 = -1;
    for (int i = 0; fub[i]; i++) {
        if (fub[i] == 'i' || fub[i] == 'o' || fub[i] == 'l') { return false; }
        if (2 <= i && fub[i] == fub[i - 1] - 1 && fub[i - 1] == fub[i - 2] - 1) {
            hasIncrSeq = true;
        }
        if (1 <= i && fub[i] == fub[i - 1]) {
            if (overlap0 < 0) {
                overlap0 = i - 1;
            } else if (fub[overlap0] != fub[i]) {
                overlap1 = i - 1;
            }
        }
    }
    return hasIncrSeq && overlap1 > 0;
}

static char *solvePart0([[maybe_unused]] Arena arena, FILE *f) {
    char *buf = nullptr;
    ssize_t bufLen = 0;
    getUntilDelimiter(arena, &buf, &bufLen, '\n', f);
    strrev(buf);
    incr0(buf);
    while(!valid(buf)) {
        incr(buf);
    } 
    strrev(buf);
    return buf;
}

static char *solvePart1([[maybe_unused]] Arena arena, FILE *f) {
    char *buf = nullptr;
    ssize_t bufLen = 0;
    getUntilDelimiter(arena, &buf, &bufLen, '\n', f);
    strrev(buf);
    incr0(buf);
    while(!valid(buf)) {
        incr(buf);
    } 
    incr0(buf);
    while(!valid(buf)) {
        incr(buf);
    } 
    strrev(buf);
    return buf;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputStr("day-11/input-ex0.txt", solvePart0, "hjaaabcc", solvePart1, "hjaabbcd");
    failed += checkInputStr("day-11/input-ex1.txt", solvePart0, "abbcefgg", solvePart1, "abbcffgh");
    failed += checkInputStr("day-11/input-ex2.txt", solvePart0, "abcdffaa", solvePart1, "abcdffbb");
    failed += checkInputStr("day-11/input-ex3.txt", solvePart0, "ghjaabcc", solvePart1, "ghjbbcdd");
    failed += checkInputStr("day-11/input-real0.txt", solvePart0, "cqjxxyzz", solvePart1, "cqkaabcc");
    return failed;
}

daySolver day11 = {11, dayMain, true};
