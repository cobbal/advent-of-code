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

function compress(corners) {
    let xs = new Set();
    let ys = new Set();
    for (let [x, y] of corners) {
        [x - 1, x, x + 1].forEach(p => xs.add(p));
        [y - 1, y, y + 1].forEach(p => ys.add(p));
    }
    xs = Array.from(xs).sort((a, b) => a - b);
    ys = Array.from(ys).sort((a, b) => a - b);
    corners = corners.map(([x, y]) => [xs.indexOf(x), ys.indexOf(y)]);
    return { xs, ys, corners };
}

function part1(lines) {
    let yx = (y, x) => 1000000000 * y + x;
    const corners = lines.map(s => s.split(',').map(s => Number(s)));
    const comp = compress(corners);
    const grid = new Map();
    for (let i = 0; i < corners.length; i++) {
        let [x0, y0] = comp.corners[i];
        let [x1, y1] = comp.corners[(i + 1) % corners.length];
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

    function cast(x0, y0, dx, dy) {
        for (let [x, y] = [x0, y0];; x += dx, y += dy) {
            if (grid.get(yx(y, x)) === 'O') {
                let xDist = Math.abs(comp.xs[x] - comp.xs[x0]);
                let yDist = Math.abs(comp.ys[y] - comp.ys[y0]);
                return xDist + yDist - 1;
            }
        }
    }

    let extents = comp.corners.map(([x, y]) =>
        ({
            n: cast(x, y, 0, -1),
            e: cast(x, y, 1, 0),
            s: cast(x, y, 0, 1),
            w: cast(x, y, -1, 0),
        })
    );

    function* areas() {
        for (let i = 0; i < corners.length; i++) {
            let [xi, yi] = corners[i];
            let iExtents = extents[i];
            for (let j = i + 1; j < corners.length; j++) {
                let [xj, yj] = corners[j];
                let jExtents = extents[j];
                let width = Math.abs(xi - xj);
                let height = Math.abs(yi - yj);

                if (iExtents[xi < xj ? 'e' : 'w'] < width) { continue; }
                if (jExtents[xi < xj ? 'w' : 'e'] < width) { continue; }
                if (iExtents[yi < yj ? 's' : 'n'] < height) { continue; }
                if (jExtents[yi < yj ? 'n' : 's'] < height) { continue; }

                yield (width + 1) * (height + 1);
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
