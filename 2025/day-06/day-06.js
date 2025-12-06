"use strict";

import util from '../common/util.js';

function part0(lines) {
    const words = lines.map(s => s.split(' ').filter(s => s.length > 0));
    const columns = Array.from(util.range(words[0].length).map(x => words.map(line => line[x])));
    return util.sum(
        columns.map(column => {
            const nums = column.slice(0, -1).map(s => Number(s));
            if (column[column.length - 1] == '+') {
                return util.sum(nums);
            } else {
                return util.product(nums);
            }
        })
    );
}

function part1(lines) {
    const width = lines.reduce((acc, line) => Math.max(acc, line.length), 0);
    const columns = Array.from(util.range(width).map(x =>
        lines.map(line => (line[x] == ' ' ? '' : line[x]) ?? '').join(''))
    );
    const groups = columns.join(' ').split('  ').map(s => s.split(' '));
    return util.sum(
        groups.map(group => {
            const num0 = Number(group[0].slice(0, -1));
            const nums = group.slice(1).map(s => Number(s));
            if (group[0][group[0].length - 1] == '+') {
                return util.sum(nums, num0);
            } else {
                return util.product(nums, num0);
            }
        })
    );
}

export function main() {
    return [
        util.checkDay("day-06/input-ex0.txt", part0, part1, 4277556, 3263827),
        util.checkDay("day-06/input-real0.txt", part0, part1, 8108520669952, 11708563470209),
    ];
}
