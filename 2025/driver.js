#!/usr/bin/env node
"use strict";

import fs from 'node:fs';
import process from 'node:process';
import util from './common/util.js';
import { performance } from 'node:perf_hooks';

const days = await (async () => {
    const allDays = new Map();
    for (var i = 1; i <= 25; i++) {
        const name = "day-" + `${i}`.padStart(2, 0);
        const path = `./${name}/${name}.js`;
        if (!fs.existsSync(path)) {
            continue;
        }
        const dayMain = (await import(path)).main;
        const module = {i, name, main: dayMain};
        allDays.set(i, module);
        allDays.set(-1, module);
    }
    const dayArgs = process.argv.slice(2);

    if (dayArgs.length === 0) {
        return [...allDays].filter(kv => kv[0] > 0).map(kv => kv[1]);
    }

    const [good, bad] = util.partition(s => allDays.has(Number(s)), dayArgs)
    if (bad.length !== 0) {
        console.log(`Unknown days requested: ${JSON.stringify(bad)}`);
        process.exit(1);
    } else {
        return good.map(s => allDays.get(Number(s)));
    }
})();

function timer(desc, fn) {
    const startTime = performance.now();
    const result = fn();
    const endTime = performance.now();
    const ms = (endTime - startTime) | 0;
    console.log(`${desc}: ${ms}ms`);
    return result;
}

function main() {
    function all() {
        let failures = 0;
        for (let day of days) {
            console.log(`=== ${day.name} ===`);
            failures += util.sum(timer("time", () => day.main()));
        }
        return failures;
    }
    const failures = days.length > 1 ? timer("Total time", all) : all();
    process.exit(+!!failures);
}
main();
