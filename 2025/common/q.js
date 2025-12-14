"use strict";

import util from "./util.js";

function gcd(a, b) {
    if (a < b) {
        [a, b] = [b, a];
    }
    while (b > 0) {
        [a, b] = [b, a % b];
    }
    return a;
}

export const mkQ = n => n instanceof Q ? n : new Q(n, 1);

export class Q {
    constructor(num, denom = 1) {
        if (num instanceof Q || denom instanceof Q || isNaN(num) || isNaN(denom)) {
            debugger;
        }
        if (denom < 0) {
            num = -num;
            denom = -denom;
        }
        const common = gcd(Math.abs(num), denom);
        this.num = num / common;
        this.denom = denom / common;
    }

    add(rhs) {
        rhs = mkQ(rhs);
        return new Q(this.num * rhs.denom + rhs.num * this.denom, this.denom * rhs.denom);
    }
    sub(rhs) {
        return this.add(mkQ(rhs).neg());
    }
    mul(rhs) {
        rhs = mkQ(rhs);
        return new Q(this.num * rhs.num, this.denom * rhs.denom);
    }
    div(rhs) {
        return this.mul(mkQ(rhs).inv());
    }
    neg() {
        return new Q(-this.num, this.denom);
    }
    inv() {
        return new Q(this.denom, this.num);
    }

    fracPart() {
        return new Q(util.mod(this.num, this.denom), this.denom);
    }

    floor() {
        return this.sub(this.fracPart());
    }

    ceil() {
        // could be faster...
        return this.neg().floor().neg();
    }

    toString() {
        if (this.denom === 1) {
            return `${this.num}`
        } else {
            return `${this.num}/${this.denom}`;
        }
    }
    abs() {
        if (this.num < 0 || this.denom < 0) {
            return new Q(Math.abs(this.num), Math.abs(this.denom));
        } else {
            return this;
        }
    }

    approx() {
        return this.num / this.denom;
    }

    static cmp(lhs, rhs) {
        lhs = mkQ(lhs);
        rhs = mkQ(rhs);
        const left = lhs.num * rhs.denom;
        const right = rhs.num * lhs.denom;
        return left - right;
    }

    static eq(lhs, rhs) {
        return Q.cmp(lhs, rhs) === 0;
    }

    static lt(lhs, rhs) {
        return Q.cmp(lhs, rhs) < 0;
    }

    static gt(lhs, rhs) {
        return Q.cmp(lhs, rhs) > 0;
    }

    static le(lhs, rhs) {
        return Q.cmp(lhs, rhs) <= 0;
    }

    static ge(lhs, rhs) {
        return Q.cmp(lhs, rhs) >= 0;
    }

    static min(lhs, rhs) {
        return Q.le(lhs, rhs) ? lhs : rhs
    }

    static max(lhs, rhs) {
        return Q.ge(lhs, rhs) ? lhs : rhs
    }

    static zero = new Q(0, 1);
    static half = new Q(1, 2);
    static one = new Q(1, 1);

    static argmin(coll, f) {
        let index = undefined;
        let value = undefined
        let projection = undefined;
        let i = 0;
        for (let x of coll) {
            let xProj = f === undefined ? x : f(x, i)
            if (i === 0 || Q.cmp(xProj, projection) < 0) {
                index = i;
                value = x;
                projection = xProj;
            }
            i++;
        }
        return { index, value, projection };
    }
}
