"use strict";

import util from '../common/util.js';

import solver from '../node_modules/javascript-lp-solver/src/solver.js';

function part0(lines) {
    let sum = 0;
    for (let line of lines) {
        const words = line.split(' ');
        const rawLights = words[0].slice(1, -1);
        const rawWirings = words.slice(1, -1).map(s => s.slice(1, -1).split(',').map(s => Number(s)));

        const lights = Array.from(rawLights).reduce(
            (acc, c, i) => acc | (c === '#' ? 1 << i : 0),
            0
        );
        const wirings = rawWirings.map(w =>
            w.reduce((acc, n) => acc | (1 << n), 0)
        );

        function findMin(lights, i) {
            if (lights === 0) { return 0; }
            if (i >= wirings.length) { return Infinity; }
            return Math.min(
                findMin(lights, i + 1),
                1 + findMin(lights ^ wirings[i], i + 1)
            );
        }
        sum += findMin(lights, 0);
    }
    return sum;
}

// https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
function gauss(mat) {
    mat = mat.map(row => Array.from(row));
    let [m, n] = [mat.length, mat[0].length];
    let h = 0;
    let k = 0;
    while (h < m && k < n) {
        let iMax = h;
        for (let i = h + 1; i < m; i++) {
            if (Math.abs(mat[i][k]) > Math.abs(mat[iMax][k])) {
                iMax = i;
            }
        }
        if (mat[iMax][k] === 0) {
            // no pivot. Next column
            k += 1;
            continue;
        }
        [mat[h], mat[iMax]] = [mat[iMax], mat[h]];
        if (mat[h][k] < 0) {
            mat[h] = mat[h].map(n => -n);
        }
        for (let i = 0; i < m; i++) {
            if (i === h) { continue; }
            const f = mat[i][k] / mat[h][k];
            mat[i][k] = 0;
            for (let j = k + 1; j < n; j++) {
                mat[i][j] -= f * mat[h][j];
            }
        }
        h++;
        k++;
    }
    return mat;
}

function part1(lines) {
    let sum = 0;
    for (let line of lines) {
        const words = line.split(' ');
        const rawWirings = words.slice(1, -1).map(s => s.slice(1, -1).split(',').map(s => Number(s)));
        const joltages = words[words.length - 1].slice(1, -1).split(',').map(s => Number(s));

        const wirings = rawWirings.map(wire => joltages.map((_, i) => wire.indexOf(i) !== -1 ? 1 : 0));
        const matrix = joltages.map((jolt, i) =>
            [...rawWirings.map(wire =>
                wire.indexOf(i) !== -1 ? 1 : 0
            ), jolt]
        );

        const symWire = i => `wire${i}`;
        const symJolt = i => `jolt${i}`;
        let constraints = {};
        let ints = {};
        let variables = {};
        wirings.forEach((wire, i) => {
            constraints[symWire(i)] = { min: 0 };
            ints[symWire(i)] = 1;
            const vars = variables[symWire(i)] = {click: 1};
            wire.forEach((b, j) => {
                if (b > 0) {
                    vars[symJolt(j)] = 1;
                }
            });
        });
        joltages.forEach((jolt, i) => {
            constraints[symJolt(i)] = { min: jolt, max: jolt };
        });

        const model = {
            optimize: 'click',
            opType: 'min',
            constraints,
            variables,
            ints,
        };
        let res = solver.Solve(model);
        console.log(model);
        console.log(res);

        function findMin(jolts, wireIndex) {
            if (jolts.some(x => x < 0)) { return Infinity; }
            if (jolts.every(x => x === 0)) { return 0; }
            if (wireIndex >= wirings.length) { return Infinity; }
            const maxLeft = util.minMax(jolts).max;
            let best = Infinity;
            for (let count = 0; count <= maxLeft; count++) {
                const newJolts = jolts.map((jolt, j) => jolt - count * wirings[wireIndex][j]);
                best = Math.min(best, count + findMin(newJolts, wireIndex + 1));
            }
            return best;
        }
        sum += res.result;
    }
    return sum;
}

export function main() {
    return [
        util.checkDay("day-10/input-ex0.txt", part0, part1, NaN, NaN),
        util.checkDay("day-10/input-real0.txt", part0, part1, NaN, NaN),
    ];
}
