"use strict";

import util from '../common/util.js';

function parse(lines) {
    const blank = lines.indexOf("");
    const ranges = lines
          .slice(0, blank)
          .map(line => {
              const [lo, hi] = line.split('-');
              return [Number(lo), Number(hi) + 1];
          })
    const inventory = lines.slice(blank + 1).map(line => Number(line));
    return { ranges, inventory };
}

function part0(lines) {
    const { ranges, inventory } = parse(lines);
    return util.count(inventory, item => ranges.some(([lo, hi]) => lo <= item && item < hi));
}

function intervalsSubtract1(iA, iB) {
    const [aLo, aHi] = iA;
    const [bLo, bHi] = iB;
    if (aHi <= bLo || bHi <= aLo) { return [iA]; }
    if (aLo < bLo && bHi < aHi) { return [[aLo, bLo], [bHi, aHi]]; }
    if (aLo < bLo) { return [[aLo, bLo]]; }
    if (bHi < aHi) { return [[bHi, aHi]]; }
    return [];
}

function intervalsSubtract(iAs, iBs) {
    return iBs.reduce((iAs, iB) => iAs.flatMap(iA => intervalsSubtract1(iA, iB)), iAs);
}

function intervalsUnion(iAs) {
    return iAs.flatMap((iA, idx) => intervalsSubtract([iA], iAs.slice(0, idx)));
}

function part1(lines) {
    return util.sum(intervalsUnion(parse(lines).ranges).map(([lo, hi]) => hi - lo));
}

export function main() {
    return [
        util.checkDay("day-05/input-ex0.txt", part0, part1, 3, 14),
        util.checkDay("day-05/input-real0.txt", part0, part1, 623, 353507173555373),
    ];
}
