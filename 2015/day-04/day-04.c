#include <stdio.h>
#include <stdlib.h>
#include "common/common.h"
#include "common/md5.h"

static int64_t solveCommon(Arena arena, FILE *f, uint8_t mask) {
    ssize_t lineLen = 0;
    char *line = nullptr;
    lineLen = getUntilDelimiter(arena, &line, &lineLen, '\n', f);
    if (lineLen < 0) { return 0; }
    line = arenaRealloc(arena, line, lineLen, lineLen + 128, 1);

    for (int result = 1;; result++) {
        int added = sprintf(line + lineLen, "%d", result);
        MD5Digest digest = md5(line, lineLen + added);
        if (digest.digest[0] == 0x00 && digest.digest[1] == 0x00 && (digest.digest[2] & mask) == 0x00) {
            return result;
        }
    }
}

static int64_t solvePart0([[maybe_unused]] Arena arena, FILE *f) { return solveCommon(arena, f, 0xf0); }
static int64_t solvePart1([[maybe_unused]] Arena arena, FILE *f) { return solveCommon(arena, f, 0xff); }

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-04/input-ex0.txt", solvePart0, 609043, solvePart1, 6742839);
    failed += checkInputInt("day-04/input-real0.txt", solvePart0, 117946, solvePart1, 3938038);
    return failed;
}

daySolver day04 = {4, dayMain, false};
