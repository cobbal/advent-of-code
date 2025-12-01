#!/usr/bin/env bash
set -euo pipefail

iDay=$(( $1 ))
decDay=$(printf "%02d" "$iDay")

echo "templating day-$decDay"
mkdir -p "day-$decDay"

cat >"day-${decDay}/day-${decDay}.js" <<EOF
"use strict";

import util from '../common/util.js';

function part0(lines) {
    const words = lines.map(s => s.split(' ').filter(s => s.length > 0));
}

function part1(lines) {
}

export function main() {
    return [
        util.checkDay("day-${decDay}/input-ex0.txt", part0, part1, NaN, NaN),
        util.checkDay("day-${decDay}/input-real0.txt", part0, part1, NaN, NaN),
    ];
}
EOF

touch "day-$decDay/input-ex0.txt"
touch "day-$decDay/input-real0.txt"
