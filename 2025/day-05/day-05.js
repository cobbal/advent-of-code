"use strict";

import util from '../common/util.js';

function parse(lines) {
    const blank = lines.indexOf("");
    const ranges = lines
          .slice(0, blank)
          .map(s => s.split('-').map(s => Number(s)))
          .map(([lo, hi]) => [lo, hi + 1]);
    const inventory = lines.slice(blank + 1).map(s => Number(s));
    return { ranges, inventory };
}

function part0(lines) {
    const { ranges, inventory } = parse(lines);
    return inventory
        .filter(item => ranges.some(([lo, hi]) => lo <= item && item < hi))
        .length;
}

function intervalsSubtract1(iA, iB) {
    const [aLo, aHi] = iA;
    const [bLo, bHi] = iB;
    if (aHi <= bLo || bHi <= aLo) { return [iA]; }
    if (aLo < bLo && bHi < aHi) { return [[aLo, bLo], [bHi, aHi]]; }
    if (bLo <= aLo && aHi <= bHi) { return []; }
    if (aLo < bLo) { return [[aLo, bLo]]; }
    return [[bHi, aHi]];
}

function intervalsSubtract(iAs, iBs) {
    return iBs.reduce((iAs, iB) => iAs.flatMap(iA => intervalsSubtract1(iA, iB)), iAs);
}

function intervalsUnion(iAs) {
    return iAs.flatMap((iA, idx) => intervalsSubtract([iA], iAs.slice(0, idx)));
}

function part1(lines) {
    const { ranges} = parse(lines);
    return util.sum(intervalsUnion(ranges).map(([lo, hi]) => hi - lo));
}

export function main() {
    return [
        util.checkDay("day-05/input-ex0.txt", part0, part1, 3, 14),
        util.checkDay("day-05/input-real0.txt", part0, part1, 623, 353507173555373),
    ];
}
