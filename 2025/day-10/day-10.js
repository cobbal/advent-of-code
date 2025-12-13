"use strict";

import util from '../common/util.js';
import { Q } from '../common/q.js';
import solver from '../node_modules/javascript-lp-solver/src/solver.js';

function printMat(mat) {
    console.log();
    let [m, n] = [mat.length, mat[0].length];
    let strings = mat.map(row => row.map(e => `${e}`));
    let colWidths = Array.from(util.range(n).map(j => util.minMax(strings.map(row => row[j].length)).max));
    strings.forEach(row =>
        console.log(row.map((s, j) => s.padStart(colWidths[j])).join(', '))
    );
}

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
function gauss(mat, recolumn) {
    mat = mat.map(row => row.map(n => new Q(n)));
    let [m, n] = [mat.length, mat[0].length];
    let columnTracker = Array.from(util.range(n).map(i => `b${i}`));
    let h = 0;
    let kSrc = 0;
    let kDest = 0;
    while (h < m && kSrc < n) {
        let iMax = h;
        for (let i = h + 1; i < m; i++) {
            if (Q.cmp(mat[i][kSrc].abs(), mat[iMax][kSrc].abs()) > 0) {
                iMax = i;
            }
        }
        if (Q.eq(mat[iMax][kSrc], 0)) {
            // no pivot. Next column
            kSrc++;
            continue;
        }
        [mat[h], mat[iMax]] = [mat[iMax], mat[h]];
        let k = kSrc;
        if (recolumn) {
            for (let i = 0; i < m; i++) {
                [mat[i][kSrc], mat[i][kDest]] = [mat[i][kDest], mat[i][kSrc]];
            }
            [columnTracker[kSrc], columnTracker[kDest]] = [columnTracker[kDest], columnTracker[kSrc]];
            k = kDest;
        }

        const pivotRow = [...mat[h]];
        for (let j = k; j < n; j++) {
            mat[h][j] = mat[h][j].div(pivotRow[k]);
        }
        for (let i = 0; i < m; i++) {
            if (i === h) { continue; }
            const f = mat[i][k].div(pivotRow[k]);
            mat[i][k] = Q.zero;
            for (let j = k + 1; j < n; j++) {
                mat[i][j] = mat[i][j].sub(f.mul(pivotRow[j]));
            }
        }
        h++;
        k++;
        kSrc++;
        kDest++;
    }
    return [
        mat.filter(row => row.some(x => !Q.eq(x, Q.zero))),
        columnTracker
    ];
}

function filterColumns(mat) {
    return mat.map(row =>
        row.filter((_, j) => !Q.eq(mat[0][j], 0))
    );
}

function part1(lines) {
    let sum0 = 0;
    let sum1 = 0;
    let diffs = 0;
    let lineNum = 0;
    for (let line of lines) {
        lineNum++;
        const words = line.split(' ');
        const rawWirings = words.slice(1, -1).map(s => s.slice(1, -1).split(',').map(s => Number(s)));
        const joltages = words[words.length - 1].slice(1, -1).split(',').map(s => Number(s));
        rawWirings.sort((a, b) => b.length - a.length);
        // console.log(rawWirings);

        // b0 (3)      ex 1
        // b1 (1,3)    ex 3
        // b2 (2)      ex 0
        // b3 (2,3)    ex 3
        // b4 (0,2)    ex 1
        // b5 (0,1)    ex 2
        // {3,5,4,7}

        // b0 + b3 - b5 = 2
        // b1 + b5 = 5
        // b2 + b3 - b5 = 1
        // b4 + b5 = 3

        // 1 + 3 - 2 = 2
        // 3 + 2 = 5
        // 0 + 3 - 2 = 1
        // 1 + 2 = 3

        const wirings = rawWirings.map(wire => joltages.map((_, i) => wire.indexOf(i) !== -1 ? 1 : 0));
        const tableau = [
            [1, ...rawWirings.map(wire => 1), 0],
            ...joltages.map(
                (jolt, i) => [
                    0,
                    ...rawWirings.map(wire =>
                        wire.indexOf(i) !== -1 ? 1 : 0
                    ),
                    jolt,
                ]
            ),
        ];
        // console.log(tableau.map(r => r.join(', ')));
        // console.log("=true=>", gauss(tableau, true).map(r => r.join(', ')));
        // console.log(lineNum);
        const [reduced, labels] = gauss(tableau, true);

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

        let method0 = solver.Solve(model).result;
        let method1 = -reduced[0].slice(-1)[0].approx();

        let n = reduced.length;
        let leftIdentity = true
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (!Q.eq(reduced[i][j], i == j ? 1 : 0)) {
                    leftIdentity = false;
                }
            }
        }
        util.assert(leftIdentity);

        if (method0 !== method1) {
            console.log();
            console.log(method0 - method1);
            // printMat(tableau);
            printMat(reduced);
            diffs++;
        }

        sum0 += method0;
        sum1 += method1;
    }
    console.log(sum0, sum1, diffs);
    return sum0;
}

export function main() {
    return [
        util.checkDay("day-10/input-ex0.txt", part0, part1, 7, 33),
        util.checkDay("day-10/input-real0.txt", part0, part1, 500, 19763),
    ];
}
