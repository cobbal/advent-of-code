"use strict";

import util from '../common/util.js';
import { Q, mkQ } from '../common/q.js';

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

class Tableau {
    rows;
    cols;
    dims;
    mat;
    vars;
    constructor(mat, vars) {
        [this.rows, this.cols] = this.dims = [mat.length, mat[0].length];
        this.mat = mat.map(row => row.map(mkQ));
        this.vars = vars ?? [
            ...util.range(this.cols).map(c => `c${c}`),
            ...util.range(this.rows).map(r => `r${r}`),
        ];
    }

    copy() {
        return new Tableau(this.mat, [...this.vars]);
    }

    print() {
        let strings = this.mat.map(row => row.map((x, i) => i > 0 ? `${x} ` : `${x} | `));
        strings = [
            ['  ', '| ', ...this.vars.slice(1, this.cols).map(x => `${x} `)],
            ...strings.map((row, i) => [
                i === 0 ? '  ' : `${this.vars[this.cols + i]}   `,
                ...row.map(x => `${x}`)
            ]),
        ]
        console.log();
        let colWidths = [...util.range(this.cols + 1).map(c =>
            util.minMax(strings.map(row => row[c].length)).max
        )];
        strings.forEach((row, i) => {
            console.log(row.map((s, j) => s.padStart(colWidths[j])).join(''));
            if (i == 1) {
                console.log(row.map((s, j) => '-'.repeat(colWidths[j])).join(''));
            }
        });
    }
}

function pivot(tableau, pivotRowIndex, pivotColIndex) {
    [tableau.vars[tableau.cols + pivotRowIndex], tableau.vars[pivotColIndex]] =
        [tableau.vars[pivotColIndex], tableau.vars[tableau.cols + pivotRowIndex]];

    const pivotValue = tableau.mat[pivotRowIndex][pivotColIndex];
    for (let j = 0; j < tableau.mat[pivotRowIndex].length; j++) {
        tableau.mat[pivotRowIndex][j] = tableau.mat[pivotRowIndex][j].div(pivotValue);
    }
    // I still don't understand this part of the pivot...
    tableau.mat[pivotRowIndex][pivotColIndex] = pivotValue.inv();

    const pivotRow = tableau.mat[pivotRowIndex];
    tableau.mat.forEach((row, i) => {
        if (i === pivotRowIndex) { return; }
        const coeff = row[pivotColIndex];
        for (let c = 0; c < row.length; c++)  {
            row[c] = row[c].sub(coeff.mul(pivotRow[c]));
        }
        row[pivotColIndex] = coeff.neg().div(pivotValue)
    });
}

function simplex(tableau) {
    const [m, n] = [tableau.rows, tableau.cols];
    while (true) {
        const { index: pivotRow, projection: rowValue } = Q.argmin(tableau.mat, row => row[0]);
        if (Q.ge(rowValue, 0)) {
            // Found fasible solution
            return true;
        }
        const { index: pivotCol, projection: quotientValue } = Q.argmin(util.range(0, n), c => {
            if (c > 0 && Q.lt(tableau.mat[pivotRow][c], 0)) {
                return tableau.mat[0][c].div(tableau.mat[pivotRow][c]);
            } else {
                return Infinity
            }
        });
        if (quotientValue === Infinity) {
            return false;
        }
        pivot(tableau, pivotRow, pivotCol);
    }
}

function selectCut(tableau, vars) {
    const { index, projection } = Q.argmin(tableau.mat.slice(1), row =>
        row[0].fracPart().sub(Q.half).abs()
    );
    if (Q.eq(projection, Q.half)) {
        return {}
    } else {
        return {
            variable: tableau.vars[tableau.cols + index + 1],
            value: tableau.mat[index + 1][0],
        }
    }
}

function applyCuts(tableau, cuts) {
    tableau = tableau.copy();
    let [m, n] = tableau.dims;
    cuts.forEach((cut, cutIndex) => {
        const constraintRow = new Array(n).fill(Q.zero);
        const sign = (cut.type === 'min') ? -1 : 1;
        const varIdx = tableau.vars.findIndex(v => v === cut.variable);

        if (varIdx < n) {
            constraintRow[0] = cut.value.mul(sign);
            constraintRow[varIdx] = mkQ(sign);
        } else {
            const varRow = tableau.mat[varIdx - n];
            const varValue = varRow[0];
            constraintRow[0] = cut.value.sub(varValue).mul(sign);
            for (let c = 1; c < n; c++) {
                constraintRow[c] = varRow[c].mul(-sign);
            }
        }
        tableau.mat.push(constraintRow);
        tableau.vars.push(`s${cutIndex}`);
    });
    return tableau;
}

// Inspired-by/debugged-with the wonderful https://github.com/JWally/jsLPSolver
function ilpSolve(originalTableau) {
    let bestIntegralSolution = Infinity;
    const branches = [{ evaluation: -Infinity, cuts: [] }];
    // Find feasible initial config
    util.assert(simplex(originalTableau));
    while (branches.length > 0) {
        const branch = branches.pop();
        if (Q.ge(branch.evaluation, bestIntegralSolution)) { continue; }
        const tableau = applyCuts(originalTableau, branch.cuts);
        if (!simplex(tableau)) {
            // not feasible
            continue;
        }
        let evaluation = tableau.mat[0][0];
        const nextCut = selectCut(tableau);
        if (nextCut.variable === undefined) {
            // integral solution found
            bestIntegralSolution = Math.min(bestIntegralSolution, evaluation.approx());
            continue;
        }
        const cutsHi = [];
        const cutsLo = [];
        for (let cut of branch.cuts) {
            if (cut.variable === nextCut.variable) {
                if (cut.type === "min") {
                    cutsLo.push(cut);
                } else {
                    cutsHi.push(cut);
                }
            } else {
                cutsHi.push(cut);
                cutsLo.push(cut);
            }
        }
        const minCut = { type: 'min', variable: nextCut.variable, value: nextCut.value.ceil() };
        const maxCut = { type: 'max', variable: nextCut.variable, value: nextCut.value.floor() }
        cutsHi.push(minCut)
        cutsLo.push(maxCut)
        branches.push({ evaluation, cuts: cutsHi });
        branches.push({ evaluation, cuts: cutsLo });
        branches.sort((a, b) => a.evaluation - b.evaluation);
    }
    return bestIntegralSolution;
}

function part1(lines) {
    let sum = 0;
    for (let line of lines) {
        const words = line.split(' ');
        const rawWirings = words.slice(1, -1).map(s => s.slice(1, -1).split(',').map(s => Number(s)));
        const joltages = words[words.length - 1].slice(1, -1).split(',').map(s => Number(s));

        const tableau = new Tableau([
            [0, ...rawWirings.map(wire => -1)],
            ...joltages.flatMap((jolt, i) => [
                [-jolt, ...rawWirings.map(wire => wire.indexOf(i) !== -1 ? -1 : 0)],
                [jolt, ...rawWirings.map(wire => wire.indexOf(i) !== -1 ? 1 : 0)],
            ]),
        ]);
        // tableau.print();

        sum += ilpSolve(tableau);
    }
    return sum;
}

export function main() {
    return [
        util.checkDay("day-10/input-ex0.txt", part0, part1, 7, 33),
        util.checkDay("day-10/input-real0.txt", part0, part1, 500, 19763),
    ];
}
