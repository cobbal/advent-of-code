"use strict";

import util from '../common/util.js';

function common(lines, digits) {
    return util.sum(
        lines.map(bank => {
            const n = bank.length;
            let nextIndex = 0;
            let number = 0n;
            for (let d = 0; d < digits; d++) {
                let bestDigit = 0;
                for (let i = nextIndex; i <= n - digits + d; i++) {
                    if (bank[i] > bestDigit) {
                        nextIndex = i + 1;
                        bestDigit = bank[i];
                    }
                }
                number = 10n * number + BigInt(bestDigit);
            }
            return number;
        }),
        0n
    );
}

function part0(lines) {
    return common(lines, 2);
}

function part1(lines) {
    return common(lines, 12);
}

export function main() {
    return [
        util.checkDay("day-03/input-ex0.txt", part0, part1, 357n, 3121910778619n),
        util.checkDay("day-03/input-real0.txt", part0, part1, 16842n, 167523425665348n),
    ];
}
