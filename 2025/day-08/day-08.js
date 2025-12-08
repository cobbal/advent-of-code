"use strict";

import util from '../common/util.js';
import { performance } from 'perf_hooks';

// adapted from https://en.wikipedia.org/wiki/Quicksort
function* lazySort(arr, cmp) {
    function partition(lo, hi) {
        const pivot = arr[hi - 1];
        let pivotIndex = lo;
        for (let j = lo; j < hi - 1; j++) {
            if (cmp(arr[j], pivot) < 0) {
                [arr[pivotIndex], arr[j]] = [arr[j], arr[pivotIndex]];
                pivotIndex++;
            }
        }
        [arr[pivotIndex], arr[hi - 1]] = [arr[hi - 1], arr[pivotIndex]];
        return pivotIndex;
    }
    function *sort(lo, hi) {
        if (lo >= hi) { return; }
        const partIndex = partition(lo, hi);
        yield *sort(lo, partIndex);
        yield arr[partIndex];
        yield *sort(partIndex + 1, hi);
    }
    yield* sort(0, arr.length);
}

function distanceSquared([x0, y0, z0], [x1, y1, z1]) {
    const dx = x0 - x1;
    const dy = y0 - y1;
    const dz = z0 - z1;
    return dx * dx + dy * dy + dz * dz;
}

function counts(nets, minSize) {
    minSize = minSize ?? 1
    const arr = Array.from(Map.groupBy(nets, ([i, n]) => n).values().map(a => a.length).filter(x => x > minSize));
    arr.sort((a, b) => b - a);
    return arr;
}

const cmpDist = ([ai, aj, ad], [bi, bj, bd]) => ad - bd;

function part0(cutoff) {
    return (lines) => {
        const boxes = lines.map(s => s.split(',').map(s => Number(s)));
        const nets = new Map(util.range(boxes.length).map(i => [i, i]));
        let distancePairs = Array.from(
            util.range(boxes.length).flatMap(i =>
                util.range(i + 1, boxes.length).map(j => [i, j, distanceSquared(boxes[i], boxes[j])])));
        for (let [i, j, ] of util.take(lazySort(distancePairs, cmpDist), cutoff)) {
            let iNet = nets.get(i);
            let jNet = nets.get(j);
            for (let [index, kNet] of Array.from(nets)) {
                if (kNet === jNet) {
                    nets.set(index, iNet);
                }
            }
            // console.log(`connecting ${boxes[i]} to ${boxes[j]} : ${counts(nets).slice(0, 10)}`);
        }
        return util.product(counts(nets).slice(0, 3));
    };
}

function part1(lines) {
    const boxes = lines.map(s => s.split(',').map(s => Number(s)));
    const nets = new Map(util.range(boxes.length).map(i => [i, i]));
    const distancePairs = Array.from(
        util.range(boxes.length).flatMap(i =>
            util.range(i + 1, boxes.length).map(j => [i, j, distanceSquared(boxes[i], boxes[j])])));
    let networks = boxes.length;
    for (let [i, j, d] of lazySort(distancePairs, cmpDist)) {
        let iNet = nets.get(i);
        let jNet = nets.get(j);
        if (iNet === jNet) { continue; }
        for (let [index, kNet] of Array.from(nets)) {
            if (kNet === jNet) {
                nets.set(index, iNet);
            }
        }
        networks--;
        if (networks == 1) {
            return boxes[i][0] * boxes[j][0];
        }
    }
}

export function main() {
    return [
        util.checkDay("day-08/input-ex0.txt", part0(10), part1, 40, 25272),
        util.checkDay("day-08/input-real0.txt", part0(1000), part1, 63920, 1026594680),
    ];
}
