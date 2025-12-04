import util from './util.js';

export function gridFromLines(lines) {
    return {
        height: lines.length,
        width: lines[0].length,
        data: lines.map(s => Array.from(s)),
        print() {
            for (let line of this.data) {
                console.log(line.join(''))
            }
        },
        copy() {
            return gridFromLines(this.data);
        },
        get(y, x) {
            util.assert(0 <= y && y < this.height);
            util.assert(0 <= x && x < this.width);
            return this.data[y][x];
        },
        set(y, x, newValue) {
            util.assert(0 <= y && y < this.height);
            util.assert(0 <= x && x < this.width);
            this.data[y][x] = newValue;
        },
        getDefault(y, x, def) {
            if (0 <= y && y < this.height && 0 <= x && x < this.width) {
                return this.data[y][x];
            } else {
                return def;
            }
        },
        indices: function* () {
            for (let y = 0; y < this.height; y++) {
                for (let x = 0; x < this.width; x++) {
                    yield [y, x];
                }
            }
        },
        count(c) {
            return Array.from(this.indices().filter(([y, x]) => this.data[y][x] === c)).length;
        },
    }
};
