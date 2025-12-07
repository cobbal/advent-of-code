"use strict";

import util from '../common/util.js';
// import { Grid, gridFromLines } from '../common/grid.js';

function part0(lines) {
    let tachyons = new Set([lines[0].indexOf('S')]);
    let splits = 0;
    for (let line of lines.slice(1)) {
        tachyons = new Set(Array.from(tachyons).flatMap(tachyon => {
            if (line[tachyon] === '.') {
                return [tachyon];
            } else if (line[tachyon] === '^') {
                splits++;
                return [tachyon - 1, tachyon + 1];
            } else {
                throw "oops";
            }
        }));
    }
    return splits;
}

function part1(lines) {
    let tachyons = new Map([[lines[0].indexOf('S'), 1]]);
    for (let line of lines.slice(1)) {
        const newTachyons = new Map();
        function add(index, count) {
            let oldCount = newTachyons.get(index) ?? 0;
            newTachyons.set(index, oldCount + count);
        }
        for (let [index, count] of tachyons) {
            if (line[index] === '.') {
                add(index, count);
            } else if (line[index] === '^') {
                add(index - 1, count);
                add(index + 1, count);
            } else {
                throw "oops";
            }
        }
        tachyons = newTachyons;
    }
    return util.sum(tachyons.values());
}

export function main() {
    return [
        util.checkDay("day-07/input-ex0.txt", part0, part1, 21, 40),
        util.checkDay("day-07/input-real0.txt", part0, part1, 1566, 5921061943075),
    ];
}
