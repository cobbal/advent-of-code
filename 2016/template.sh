#!/usr/bin/env bash
set -euo pipefail

iday=$(( $1 ))
sday=$(printf "%02d" "$iday")

echo "temmplating day-$sday"
mkdir -p "day-$sday"

cat >"day-$sday/day-$sday.wat" <<EOF
(data (i32.const 0x${dayHex}_8000) "day-01/input-ex0.txt")
(data (i32.const 0x${dayHex}_8020) "day-01/input-real0.txt")

(elem (i32.const 0x${dayHex}0) \$day${sday}.part0 \$day${sday}.part1)

(func \$day${sday}.part0 (param \$filename i32) (result i64)
  (i64.const 0))

(func \$day${sday}.part1 (param \$filename i32) (result i64)
  (i64.const 0))

(elem (table \$mains) (i32.const 1) \$day${sday}.main)
(func \$day${sday}.main (result i32)
  (i32.const 0)
  (i32.add
    (call \$checkInputI64
      (i32.const 0x${dayHex}_8000)
      (i32.const 0x${dayHex}0) (i64.const -1)
      (i32.const 0x${dayHex}1) (i64.const -1)))
  (i32.add
    (call \$checkInputI64
      (i32.const 0x${dayHex}_8020)
      (i32.const 0x${dayHex}0) (i64.const -1)
      (i32.const 0x${dayHex}1) (i64.const -1)))
  nop)
EOF

touch "day-$sday/input-ex0.txt"
touch "day-$sday/input-real0.txt"
