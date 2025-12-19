#!/usr/bin/env bash
set -euo pipefail

iDay=$(( $1 ))
decDay=$(printf "%02d" "$iDay")
hexDay=$(printf "%02x" "$iDay")

echo "templating day-$decDay"
mkdir -p "day-$decDay"

cat >"day-${decDay}/day-${decDay}.mwat" <<EOF
;; -*- mode: wat -*-
(namespace \$day${decDay}
  (indirect_func part0 (param \$lines (ref null \$list<*>.T)) (result i64)
    (i64 0))

  (indirect_func part1 (param \$lines (ref null \$list<*>.T)) (result i64)
    (i64 0))

  (indirect_func main (result i32)
    (+
      (call \$day-utils.checkI64
        "day-${decDay}/input-ex0.txt"
        (funcref part0) (i64 -1)
        (funcref part1) (i64 -1))
      (call \$day-utils.checkI64
        "day-${decDay}/input-real0.txt"
        (funcref part0) (i64 -1)
        (funcref part1) (i64 -1))
      0)))
EOF

touch "day-$decDay/input-ex0.txt"
touch "day-$decDay/input-real0.txt"
