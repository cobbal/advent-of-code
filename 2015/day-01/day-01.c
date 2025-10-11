#include "day-01.h"

static int64_t solvePart0(FILE *f) {
    int64_t floor = 0;
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }
    }
    return floor;
}

static int64_t solvePart1(FILE *f) {
    int64_t floor = 0;
    int64_t pos = 0;
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }
        pos++;
        if (floor < 0) {
            return pos;
        }
    }
    return -1;
}

static constexpr int dayNumber = 1;
static void dayMain() {
    dayHeader(dayNumber);
    checkInputInt("day-01/input-real0.txt", solvePart0, 280, solvePart1, 1797);
}
daySolver day01 = { dayNumber, dayMain };