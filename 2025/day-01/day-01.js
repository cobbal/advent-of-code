"use strict";

import util from '../common/util.js';

function part0(lines) {
    const turns = lines.map(line => Number(line.replace('L', '-').replace('R', '')));
    return turns.reduce((acc, turn) => {
        const n = acc.n + turn;
        return { n, z: acc.z + (util.mod(n, 100) === 0) };
    }, {n: 50, z: 0}).z;
}

function part1(lines) {
    const turns = lines.map(line => Number(line.replace('L', '-').replace('R', '')));
    return turns.reduce((acc, turn) => {
        const fullTurns = util.div(Math.abs(turn), 100);
        const unwrapped = acc.n + turn % 100;
        const n = util.mod(unwrapped, 100);
        const partialClick = +(acc.n !== 0 && (n === 0 || unwrapped !== n));
        return { n, z: acc.z + fullTurns + partialClick };
    }, {n: 50, z: 0}).z;
}

export function main() {
    return [
        util.checkDay("day-01/input-ex0.txt", part0, part1, 3, 6),
        util.checkDay("day-01/input-real0.txt", part0, part1, 1011, 5937),
    ];
}
