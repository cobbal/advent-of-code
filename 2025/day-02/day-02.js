"use strict";

import util from '../common/util.js';

function parse(lines) {
    return lines
        .join()
        .split(',')
        .filter(s => s.length > 0)
        .map(s => s.split('-').map(Number));
}

function findReps(lo, hi, copies) {
    const found = new Set();
    const loDigits = util.nDigits(lo);
    const hiDigits = util.nDigits(hi);
    for (let n = loDigits; n <= hiDigits; n++) {
        const [subDigits, leftover] = util.divMod(n, copies);
        if (leftover !== 0) { continue; }
        const innerLo = loDigits == n ? lo : Math.pow(10, n - 1);
        const innerHi = hiDigits == n ? hi : Math.pow(10, n) - 1;
        const loPart = util.div(innerLo, Math.pow(10, n - subDigits));
        const hiPart = util.div(innerHi, Math.pow(10, n - subDigits));
        const ones = util.range(copies).reduce(acc => Math.pow(10, subDigits) * acc + 1, 0);
        for (let x = loPart; x <= hiPart; x++) {
            const xx = x * ones;
            if (innerLo <= xx && xx <= innerHi) {
                found.add(BigInt(xx));
            }
        }
    }
    return found;
}

function part0(lines) {
    let found = new Set();
    for (const [lo, hi] of parse(lines)) {
        found = found.union(findReps(lo, hi, 2));
    }
    return util.sum(found, 0n);
}

function part1(lines) {
    let found = new Set();
    for (const [lo, hi] of parse(lines)) {
        for (let copies = 2; copies <= util.nDigits(hi); copies++) {
            found = found.union(findReps(lo, hi, copies));
        }
    }
    return util.sum(found, 0n);
}

export function main() {
    return [
        util.checkDay("day-02/input-ex0.txt", part0, part1, 1227775554n, 4174379265n),
        util.checkDay("day-02/input-real0.txt", part0, part1, 12599655151n, 20942028255n),
    ];
}
