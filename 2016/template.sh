#!/usr/bin/env bash
set -euo pipefail

iDay=$(( $1 ))
decDay=$(printf "%02d" "$iDay")
hexDay=$(printf "%02x" "$iDay")

echo "temmplating day-$decDay"
mkdir -p "day-$decDay"

cat >"day-${decDay}/day-${decDay}.wat" <<EOF
(data (i32.const 0x${hexDay}_8000) "day-${decDay}/input-ex0.txt")
(data (i32.const 0x${hexDay}_8020) "day-${decDay}/input-real0.txt")

(elem (i32.const 0x${hexDay}0) \$day${decDay}.part0 \$day${decDay}.part1)

(func \$day${decDay}.part0 (param \$filename i32) (result i64)
  (i64.const 0))

(func \$day${decDay}.part1 (param \$filename i32) (result i64)
  (i64.const 0))

(elem (table \$mains) (i32.const ${iDay}) \$day${decDay}.main)
(func \$day${decDay}.main (result i32)
  (i32.const 0)
  (i32.add
    (call \$checkInputI64
      (i32.const 0x${hexDay}_8000)
      (i32.const 0x${hexDay}0) (i64.const -1)
      (i32.const 0x${hexDay}1) (i64.const -1)))
  (i32.add
    (call \$checkInputI64
      (i32.const 0x${hexDay}_8020)
      (i32.const 0x${hexDay}0) (i64.const -1)
      (i32.const 0x${hexDay}1) (i64.const -1)))
  nop)
EOF

touch "day-$decDay/input-ex0.txt"
touch "day-$decDay/input-real0.txt"
