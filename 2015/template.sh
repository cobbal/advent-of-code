#!/usr/bin/env bash
set -euo pipefail

iday=$(( $1 ))
sday=$(printf "%02d" "$iday")

echo "temmplating day-$sday"
mkdir -p "day-$sday"

cat >"day-$sday/day-$sday.c" <<EOF
#include "common/common.h"

static int64_t solvePart0(([[maybe_unused]] Arena arena, FILE *f) {
    (void)f;
    return 0;
}

static int64_t solvePart1(([[maybe_unused]] Arena arena, FILE *f) {
    (void)f;
    return 0;
}

static int dayMain() {
    int failed = 0;
    failed += checkInputInt("day-$sday/input-ex0.txt", solvePart0, -1, solvePart1, -1);
    // failed += checkInputInt("day-$sday/input-real0.txt", solvePart0, -1, solvePart1, -1);
    return failed;
}
daySolver day$sday = { $iday, dayMain, true };
EOF

touch "day-$sday/input-ex0.txt"
touch "day-$sday/input-real0.txt"
