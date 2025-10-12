#include "common.h"

#include <stdio.h>
#include <stdlib.h>

#include "md5.h"

static int64_t solveCommon(FILE *f, uint8_t mask) {
    ssize_t lineLen = 0;
    char *line = nullptr;
    lineLen = getUntilDelimiter(&line, &lineLen, '\n', f);
    if (lineLen < 0) { return 0; }
    void *tmp = line;
    if ((line = realloc(line, lineLen + 128)) == nullptr) {
        free(tmp);
        return 0;
    }

    int result;
    for (result = 1;; result++) {
        int added = sprintf(line + lineLen, "%d", result);
        MD5Digest digest = md5(line, lineLen + added);
        if (digest.digest[0] == 0x00 && digest.digest[1] == 0x00 && (digest.digest[2] & mask) == 0x00) {
            break;
        }
    }

    free(line);
    return result;
}

static int64_t solvePart0(FILE *f) { return solveCommon(f, 0xf0); }
static int64_t solvePart1(FILE *f) { return solveCommon(f, 0xff); }

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-04/input-ex0.txt", solvePart0, 609043, solvePart1, 6742839);
    failed += checkInputInt("day-04/input-real0.txt", solvePart0, 117946, solvePart1, 3938038);
    return failed;
}

daySolver day04 = {4, dayMain, false};
