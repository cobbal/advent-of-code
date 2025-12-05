import util from './util.js';

export class Grid {
    constructor(height, width, data) {
        this.height = height;
        this.width = width;
        this.data = data;
    }
    print() {
        for (let line of this.data) {
            console.log(line.join(''))
        }
    }
    copy() {
        return new Grid(this.width, this.height, this.data.map(a => Array.from(a)));
    }
    indexValid(y, x) {
        return 0 <= y && y < this.height &&
            0 <= x && x < this.width;
    }
    get(y, x) {
        util.assert(this.indexValid(y, x));
        return this.data[y + 1][x + 1];
    }
    getUnchecked(y, x) {
        return this.data[y + 1][x + 1];
    }
    set(y, x, newValue) {
        util.assert(this.indexValid(y, x));
        this.data[y + 1][x + 1] = newValue;
    }
    getDefault(y, x, def) {
        return this.indexValid(y, x) ? this.data[y + 1][x + 1] : def
    }
    * indices() {
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                yield [y, x];
            }
        }
    }
    count(c) {
        return Array.from(this.indices().filter(([y, x]) => this.getUnchecked(y, x) === c)).length;
    }
};

export function gridFromLines(lines) {
    const height = lines.length;
    const width = lines[0].length;
    return new Grid(
        height,
        width,
        [
            Array(width).fill(null),
            ...lines.map(s => [null, ...s, null]),
            Array(width).fill(null),
        ]
    );
}
