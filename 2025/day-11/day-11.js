"use strict";

import util from '../common/util.js';

function setPop(set) {
    for (let x of set) {
        set.delete(x);
        return x;
    }
}

function countPaths(graph, from, to) {
    const revGraph = new Map();
    for (let [u, nodes] of graph) {
        for (let v of nodes) {
            util.mapGetOrInsert(revGraph, v, []).push(u);
        }
    }
    const counts = new Map();
    const updateQueue = new Set([from]);
    while (updateQueue.size > 0) {
        let node = setPop(updateQueue);
        let newCount =
            util.sum((revGraph.get(node) ?? []).map(pred => counts.get(pred) ?? 0)) +
            (node === from);
        counts.set(node, newCount);
        (graph.get(node) ?? []).forEach(next => updateQueue.add(next));
    }
    return counts.get(to) ?? 0;
}

function parse(lines) {
    return new Map(
        lines.map(s => {
            const [a, ...b] = s.split(' ');
            return [a.slice(0, -1), b];
        })
    );
}

function part0(lines) {
    return countPaths(parse(lines), 'you', 'out');
}

function part1(lines) {
    const graph = parse(lines);
    let n = 0;
    n += countPaths(graph, 'svr', 'fft') * countPaths(graph, 'fft', 'dac') * countPaths(graph, 'dac', 'out');
    n += countPaths(graph, 'svr', 'dac') * countPaths(graph, 'dac', 'fft') * countPaths(graph, 'fft', 'out');
    return n;
}

export function main() {
    return [
        util.checkDay("day-11/input-ex0.txt", part0, part1, 5, 0),
        util.checkDay("day-11/input-ex1.txt", part0, part1, 0, 2),
        util.checkDay("day-11/input-real0.txt", part0, part1, 470, 384151614084875),
    ];
}
