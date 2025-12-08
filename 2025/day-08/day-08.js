"use strict";

import util from '../common/util.js';

function distanceSquared([x0, y0, z0], [x1, y1, z1]) {
    const dx = Math.abs(x0 - x1);
    const dy = Math.abs(y0 - y1);
    const dz = Math.abs(z0 - z1);
    return dx * dx + dy * dy + dz * dz;
}

function counts(nets, minSize) {
    minSize = minSize ?? 1
    const arr = Array.from(Map.groupBy(nets, ([i, n]) => n).values().map(a => a.length).filter(x => x > minSize));
    arr.sort((a, b) => b - a);
    return arr;
}

function part0(cutoff) {
    return (lines) => {
        const boxes = lines.map(s => s.split(',').map(s => Number(s)));
        const nets = new Map(util.range(boxes.length).map(i => [i, i]));
        const distancePairs = Array.from(
            util.range(boxes.length).flatMap(i =>
                util.range(i + 1, boxes.length).map(j => [i, j, distanceSquared(boxes[i], boxes[j])])));
        distancePairs.sort(([ai, aj, ad], [bi, bj, bd]) => ad - bd);
        let connections = 0;
        for (let [i, j, d] of distancePairs.slice(0, cutoff)) {
            let iNet = nets.get(i);
            let jNet = nets.get(j);
            // if (iNet === jNet) { continue; }
            connections++;
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
    distancePairs.sort(([ai, aj, ad], [bi, bj, bd]) => ad - bd);
    let networks = boxes.length;
    for (let [i, j, d] of distancePairs) {
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
    return util.product(counts(nets).slice(0, 3));
}

export function main() {
    return [
        util.checkDay("day-08/input-ex0.txt", part0(10), part1, 40, 25272),
        util.checkDay("day-08/input-real0.txt", part0(1000), part1, 63920, 1026594680),
    ];
}
