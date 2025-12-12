"use strict";

function gcd(a, b) {
    if (a < b) {
        [a, b] = [b, a];
    }
    while (b > 0) {
        [a, b] = [b, a % b];
    }
    return a;
}

const ensureQ = n => n instanceof Q ? n : new Q(n, 1);

export class Q {
    constructor(num, denom = 1) {
        if (isNaN(num) || isNaN(denom)) {
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
        rhs = ensureQ(rhs);
        return new Q(this.num * rhs.denom + rhs.num * this.denom, this.denom * rhs.denom);
    }
    sub(rhs) {
        return this.add(ensureQ(rhs).neg());
    }
    mul(rhs) {
        rhs = ensureQ(rhs);
        return new Q(this.num * rhs.num, this.denom * rhs.denom);
    }
    div(rhs) {
        return this.mul(ensureQ(rhs).inv());
    }
    neg() {
        return new Q(-this.num, this.denom);
    }
    inv() {
        return new Q(this.denom, this.num);
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
        lhs = ensureQ(lhs);
        rhs = ensureQ(rhs);
        const left = lhs.num * rhs.denom;
        const right = rhs.num * lhs.denom;
        return left - right;
    }

    static eq(lhs, rhs) {
        return Q.cmp(lhs, rhs) === 0;
    }

    static zero = new Q(0, 1);
}
