"use strict";

import fs from 'node:fs';

export function partition(pred, coll) {
    const yes = [];
    const no = [];
    for (let x of coll) {
        if (pred(x)) {
            yes.push(x);
        } else {
            no.push(x);
        }
    }
    return [yes, no];
}

export function checkDay(path, part0, part1, expected0, expected1) {
    let lines = fs.readFileSync(path, "utf8").split('\n').filter(s => s.length > 0);
    const result0 = part0(lines);
    const result1 = part1(lines);
    let good0 = result0 === expected0;
    let good1 = result1 === expected1;
    const goodStr = b => b ? " \u2705 good" : " \u274c bad ";
    console.log(
        '' +
            (path + ':').padEnd(25, ' ') +
            String(result0).padStart(20, ' ') +
            goodStr(good0) +
            String(result1).padStart(20, ' ') +
            goodStr(good1)
    );
    return +!(good0 && good1);
}

export function sum(coll, start) {
    if (!(coll instanceof Array)) {
        coll = Array.from(coll)
    }
    return coll.reduce((a, b) => a + b, start ?? 0);
}

export function div(x, m) {
    return (x / m) | 0;
}

export function mod(x, m) {
    return ((x % m) + m) % m;
}

export function divMod(x, m) {
    return [(x / m) | 0, ((x % m) + m) % m];
}

export function bigMin(a, b) {
    return a < b ? a : b;
}

export function bigMax(a, b) {
    return a > b ? a : b;
}

function nDigits(n) {
    return String(n).length;
}

// if 1 parameter is passed, it's assumed to be 0..<hi
function* range(lo, hi, step = 1) {
    if (arguments.length === 1) {
        [lo, hi] = [0, lo];
    }
    for (let i = lo; i < hi; i += step) {
        yield i;
    }
}

export function assert(bool, msg) {
    console.assert(bool, msg);
    if (!bool) { throw "Assertion failure"; }
}

export default {
    partition,
    checkDay,
    sum,
    div, mod, divMod,
    bigMin, bigMax,
    nDigits,
    range,
    assert,
};
