#!/usr/bin/env bash
set -euo pipefail

iDay=$(( $1 ))
decDay=$(printf "%02d" "$iDay")
hexDay=$(printf "%02x" "$iDay")

echo "templating day-$decDay"
mkdir -p "day-$decDay"

cat >"day-${decDay}/day-${decDay}.wat" <<EOF
;; -*- mode: wat -*-
(namespace \$day${decDay}
  (indirect_func part0 (param \$filename i32) (result i64)
    (locals (ref null $list<*>.T) $lines)
    (locals (ref $String) $line)
    (local.set $lines (call $split (call $readFile $filename) #\newline 0))

    0)

  (indirect_func part1 (param \$filename i32) (result i64)
    0)

  (indirect_func main (result i32)
    (+
      (call \$day-utils.checkI64
        "day-${decDay}/input-ex0.txt"
        (funcref part0) (i64 -1)
        (funcref part1) (i64 -1))
      (call \$day-utils.checkI64
        "${decDay}-01/input-real0.txt"
        (funcref part0) (i64 -1)
        (funcref part1) (i64 -1))
      0)))
EOF

touch "day-$decDay/input-ex0.txt"
touch "day-$decDay/input-real0.txt"
