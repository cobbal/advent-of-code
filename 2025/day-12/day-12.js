"use strict";

import util from '../common/util.js';

// This is such a bad solution. I'm upset that it works.
function part0(lines) {
    const chunks = lines.join(',').split(',,').map(s => s.split(','));
    const shapes = chunks.slice(0, -1).map(c => c.slice(1));
    const sections = chunks[chunks.length - 1].map(s => {
        const words = s.split(' ');
        const [w, h] = words[0].slice(0, -1).split('x').map(s => Number(s));
        return [w, h, words.slice(1).map(s => Number(s))];
    });
    const areas = shapes.map(shape => util.count(shape.join(''), c => c === '#'));
    return util.sum(sections.map(([w, h, presents]) => {
        let area = util.sum(presents.map((i, j) => i * areas[j]));
        return area <= w * h;
    }));
}

function part1(lines) {}

export function main() {
    return [
        // util.checkDay("day-12/input-ex0.txt", part0, part1, 2, undefined),
        util.checkDay("day-12/input-real0.txt", part0, part1, 414, undefined),
    ];
}
