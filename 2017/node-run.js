#!/usr/bin/env node

'use strict';

// Adapted from sample code at https://nodejs.org/api/wasi.html
const { readFile } = require('node:fs/promises');
const { WASI } = require('node:wasi');
const { argv, env, cwd } = require('node:process');
const { join } = require('node:path');

const wasi = new WASI({
    version: 'preview1',
    args: [],
    env: {},
    preopens: { ".": "." },
});

(async () => {
    const wasm = await WebAssembly.compile(
        await readFile(join(__dirname, 'build/2017.wasm')),
    );
    const importObj = wasi.getImportObject();
    const oldYield = importObj.wasi_snapshot_preview1.sched_yield;
    importObj.wasi_snapshot_preview1.sched_yield = () => {
        const res = oldYield();
        debugger;
        return res;
    }
    const instance = await WebAssembly.instantiate(wasm, importObj);
    wasi.start(instance);
})();
