"use strict";

import util from '../common/util.js';
import { gridFromLines } from '../common/grid.js';

function common(lines, repeat) {
    let grid = gridFromLines(lines);
    let progress;
    do {
        progress = false;
        let newGrid = repeat ? grid : grid.copy();
        for (let [y, x] of grid.indices()) {
            if (grid.get(y, x) !== '@') { continue; }
            let neighbors = 0;
            for (let dy = -1; dy <= 1; dy++) {
                for (let dx = -1; dx <= 1; dx++) {
                    neighbors += (grid.getDefault(y + dy, x + dx, '.') === '@');
                }
            }
            if (neighbors < 5) {
                newGrid.set(y, x, 'x');
                progress = true;
            }
        }
        grid = newGrid;
    } while (repeat && progress);
    return grid.count('x');
}

function part0(lines) {
    return common(lines, false);
}

function part1(lines) {
    return common(lines, true);
}

export function main() {
    return [
        util.checkDay("day-04/input-ex0.txt", part0, part1, 13, 43),
        util.checkDay("day-04/input-real0.txt", part0, part1, 1493, 9194),
    ];
}
