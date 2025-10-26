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
        await readFile(join(__dirname, 'build/2016.wasm')),
    );
    const instance = await WebAssembly.instantiate(wasm, wasi.getImportObject());

    debugger;
    wasi.start(instance);
})();
