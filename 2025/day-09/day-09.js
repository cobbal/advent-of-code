"use strict";

import util from '../common/util.js';
import { Grid, gridFromLines } from '../common/grid.js';

function part0(lines) {
    const corners = lines.map(s => s.split(',').map(s => Number(s)));
    function* areas() {
        for (let [x0, y0] of corners) {
            for (let [x1, y1] of corners) {
                yield (Math.abs(x0 - x1) + 1) * (Math.abs(y0 - y1) + 1);
            }
        }
    }
    return util.minMax(areas()).max;
}


function part1(lines) {
    let yx = (y, x) => 1000000000 * y + x;
    const corners = lines.map(s => s.split(',').map(s => Number(s)));
    const { min: minX, max: maxX } = util.minMax(corners.map(xy => xy[0]));
    const { min: minY, max: maxY } = util.minMax(corners.map(xy => xy[1]));
    const grid = new Map();
    for (let i = 0; i < corners.length; i++) {
        let [x0, y0] = corners[i];
        let [x1, y1] = corners[(i + 1) % corners.length];
        grid.set(yx(y0, x0), '#');
        let dx = Math.sign(x1 - x0);
        let dy = Math.sign(y1 - y0);
        let lx = dy;
        let ly = -dx;

        if (grid.get(yx(y0 + ly, x0 + lx)) === undefined) {
            grid.set(yx(y0 + ly, x0 + lx), 'O');
        }
        if (grid.get(yx(y1 + ly, x1 + lx)) === undefined) {
            grid.set(yx(y1 + ly, x1 + lx), 'O');
        }
        for (let [x, y] = [x0 + dx, y0 + dy]; x != x1 || y != y1; x += dx, y += dy) {
            grid.set(yx(y, x), 'X');
            if (grid.get(yx(y + ly, x + lx)) === undefined) {
                grid.set(yx(y + ly, x + lx), 'O');
            }
        }
    }

    function cast(x, y, dx, dy) {
        for (let distance = 0;; distance += 1, x += dx, y += dy) {
            if (grid.get(yx(y, x)) === 'O') { return distance - 1; }
        }
    }

    let extents = corners.map(([x, y]) =>
        ({
            n: cast(x, y, 0, -1),
            e: cast(x, y, 1, 0),
            s: cast(x, y, 0, 1),
            w: cast(x, y, -1, 0),
        })
    );

    function* areas() {
        let progress = 0;
        let n = corners.length * (corners.length + 1) / 2;
        for (let i = 0; i < corners.length; i++) {
            let [xi, yi] = corners[i];
            let extentsi = extents[i];
            for (let j = i + 1; j < corners.length; j++) {
                let [xj, yj] = corners[j];
                let extentsj = extents[j];
                let width = Math.abs(xi - xj);
                let height = Math.abs(yi - yj);
                const area = (width + 1) * (height + 1);

                // if (area == 24) { debugger; }

                let clear = true;
                if (xi <= xj) {
                    clear &&= (width <= extentsi.e);
                    clear &&= (width <= extentsj.w);
                } else {
                    clear &&= (width <= extentsi.w);
                    clear &&= (width <= extentsj.e);
                }
                if (yi <= yj) {
                    clear &&= (height <= extentsi.s);
                    clear &&= (height <= extentsj.n);
                } else {
                    clear &&= (height <= extentsi.n);
                    clear &&= (height <= extentsj.s);
                }

                if (clear) {
                    yield area;
                }
            }
        }
    }
    return util.minMax(areas()).max;
}

export function main() {
    return [
        util.checkDay("day-09/input-ex0.txt", part0, part1, 50, 24),
        util.checkDay("day-09/input-real0.txt", part0, part1, 4749929916, 1572047142),
    ];
}
