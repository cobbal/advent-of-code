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

export function arrayTrim(array, pred) {
    const first = array.findIndex(e => !pred(e));
    const last = array.findLastIndex(e => !pred(e)) + 1;
    return array.slice(first, last);
}

export function checkDay(path, part0, part1, expected0, expected1) {
    let lines = arrayTrim(fs.readFileSync(path, "utf8").split('\n'), e => e === '');
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
    let result = arguments.length < 2 ? 0 : start;
    for (let x of coll) {
        result += x;
    }
    return result;
}

export function product(coll, start) {
    let result = arguments.length < 2 ? 1 : start;
    for (let x of coll) {
        result *= x;
    }
    return result;
}

export function count(coll, pred) {
    var result = 0;
    for (let x of coll) {
        if (pred(x)) {
            result++;
        }
    }
    return result;
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

function* take(iter, n) {
    let yielded = 0;
    if (n > 0) {
        for (let x of iter) {
            yield x;
            yielded++;
            if (yielded >= n) { break; }
        }
    }
}

export function assert(bool, msg) {
    if (!bool) {
        console.assert(bool, msg);
        throw "Assertion failure";
    }
}

export default {
    partition,
    checkDay,
    sum, product, count,
    div, mod, divMod,
    bigMin, bigMax,
    nDigits,
    range, take,
    assert,
    arrayTrim,
};
