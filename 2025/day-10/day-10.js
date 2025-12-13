"use strict";

import util from '../common/util.js';
import { Q, mkQ } from '../common/q.js';
import solver from '../node_modules/javascript-lp-solver/src/solver.js';
import Model from '../node_modules/javascript-lp-solver/src/Model.js';

function printTab(tableau, label) {
    let [m, n] = [tableau.length, tableau[0].length];
    tableau = tableau.map(row => row.map((x, j) => j == 0 ? `${x} |` : `${x}`));
    tableau = [tableau[0], tableau[0].map(x => '-'.repeat(x.length)), ...tableau.slice(1)]
    printMat(tableau, label);
}

function printMat(mat, label) {
    console.log();
    if (label !== undefined) {
        console.log(label);
    }
    let [m, n] = [mat.length, mat[0].length];
    let strings = mat.map(row => row.map(e => `${e}`));
    let colWidths = Array.from(util.range(n).map(j => util.minMax(strings.map(row => row[j].length)).max));
    strings.forEach((row, i) => {
        console.log(row.map((s, j) => s.padStart(colWidths[j])).join(', '));
    });
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

function minWithIndex(coll) {
    let index = undefined;
    let value = undefined
    let i = 0;
    for (let x of coll) {
        if (i === 0 || Q.cmp(x, value) < 0) {
            index = i;
            value = x;
        }
        i++;
    }
    return { index, value };
}

function pivot(mat, pivotRowIndex, pivotColIndex) {
    const pivotValue = mat[pivotRowIndex][pivotColIndex];
    for (let j = 0; j < mat[pivotRowIndex].length; j++) {
        mat[pivotRowIndex][j] = mat[pivotRowIndex][j].div(pivotValue);
    }
    // I still don't understand this part of the pivot...
    mat[pivotRowIndex][pivotColIndex] = pivotValue.inv();

    const pivotRow = mat[pivotRowIndex];
    mat.forEach((row, i) => {
        if (i === pivotRowIndex) { return; }
        const coeff = row[pivotColIndex];
        for (let c = 0; c < row.length; c++)  {
            row[c] = row[c].sub(coeff.mul(pivotRow[c]));
        }
        row[pivotColIndex] = coeff.neg().div(pivotValue)
    });
}

function myPhase1(tab) {
    const [m, n] = [tab.length, tab[0].length];
    while (true) {
        const { index: pivotRow, value: rowValue } = minWithIndex(tab.map(row => row[0]));
        if (Q.ge(rowValue, 0)) {
            // Found fasible solution
            return;
        }
        const { index: pivotCol, value: quotientValue } = minWithIndex(util.range(0, n).map(c =>
            (c > 0 && Q.lt(tab[pivotRow][c], 0)) ?
                tab[0][c].div(tab[pivotRow][c]) :
                Infinity
        ));
        if (quotientValue === Infinity) {
            throw "infeasible";
        }
        // console.log(`pivoting on ${tab[pivotRow][pivotCol]} at (${pivotRow}, ${pivotCol})`);
        pivot(tab, pivotRow, pivotCol);
        // printTab(tab, `after pivoting on ${tab[pivotRow][pivotCol]} at (${pivotRow}, ${pivotCol})`);
    }
}

// https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
function gauss(mat, recolumn) {
    mat = mat.map(row => row.map(n => mkQ(n)));
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

function filterZeroRows(mat) {
    return mat.filter(row => row.some(x => !Q.eq(x, 0)))
}

function doMethod1(tableau) {
    tableau = tableau.map(row => row.map(mkQ));
    myPhase1(tableau);
    return [tableau[0][0].approx(), tableau];
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

        const wirings = rawWirings.map(wire => joltages.map((_, i) => wire.indexOf(i) !== -1 ? 1 : 0));
        const tableau = [
            [0, ...rawWirings.map(wire => -1)],
            ...joltages.flatMap((jolt, i) => [
                [-jolt, ...rawWirings.map(wire => wire.indexOf(i) !== -1 ? -1 : 0)],
                [jolt, ...rawWirings.map(wire => wire.indexOf(i) !== -1 ? 1 : 0)],
            ]),
        ];

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
        console.log("==============", method0, "==============");
        let [method1, reduced] = doMethod1(tableau);
        model.ints = undefined;
        const m = new Model().loadJson(model);
        m.tableau.setModel(m);
        // console.log(m);
        // printTab(filterZeroRows(m.tableau.matrix), "init:");
        m.tableau.phase1();
        // printTab(filterZeroRows(m.tableau.matrix), "phase1:");
        m.solve();

        if (method0 !== method1) {
            console.log();
            console.log("DIFFERENCE", method0, method1, method0 - method1);
            // printTab(filterZeroRows(m.tableau.matrix), "REF:");
            printTab(reduced);
            console.log();
            console.log();
            diffs++;
        }

        sum0 += method0;
        sum1 += method1;
    }
    console.log(sum0, sum1, diffs);
    return sum0;
}

export function main() {
    global.printTab = printTab;
    return [
        util.checkDay("day-10/input-ex0.txt", part0, part1, 7, 33),
        util.checkDay("day-10/input-real0.txt", part0, part1, 500, 19763),
    ];
}
