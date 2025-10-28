(data (i32.const 0x2_0020) "day-02/input-ex0.txt")
(data (i32.const 0x2_0040) "                1985")
(data (i32.const 0x2_0060) "                5DB3")

(data (i32.const 0x2_0120) "day-02/input-real0.txt")
(data (i32.const 0x2_0140) "               78985")
(data (i32.const 0x2_0160) "               57DD8")

(data (i32.const 0x2_0200)
  "..1..\00"
  ".234.\00"
  "56789\00"
  ".ABC.\00"
  "..D..\00")

(elem (i32.const 0x020) $day02.part0 $day02.part1)

(func $day02.part0 (param $filename i32) (result i32)
  (local $y i32)
  (local $x i32)
  (local $lines i32)
  (local $line i32)
  (local $c i32)
  (local $result i64)
  (local $digit i32)

  (local.set $y (i32.const 1))
  (local.set $x (i32.const 1))

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (loop $charLoop
          (if (local.tee $c (i32.load8_u (local.get $line)))
            (then
              (if (i32.eq (local.get $c) (i32.const 0x55 (;'U';)))
                (then (local.set $y (i32.sub (local.get $y) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x52 (;'R';)))
                (then (local.set $x (i32.add (local.get $x) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x44 (;'D';)))
                (then (local.set $y (i32.add (local.get $y) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x4c (;'L';)))
                (then (local.set $x (i32.sub (local.get $x) (i32.const 1)))))
              (local.set $x (call $i32.max (i32.const 0) (call $i32.min (local.get $x) (i32.const 2))))
              (local.set $y (call $i32.max (i32.const 0) (call $i32.min (local.get $y) (i32.const 2))))
              (local.set $line (i32.add (local.get $line) (i32.const 1)))
              (br $charLoop))))
        (local.set $digit
          (i32.add (i32.const 1)
            (i32.add
              (i32.mul (i32.const 3) (local.get $y))
              (local.get $x))))
        (local.set $result
          (i64.add
            (i64.mul (local.get $result) (i64.const 10))
            (i64.extend_i32_s (local.get $digit))))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (call $formatI64 (local.get $result)))

(func $day02.part1 (param $filename i32) (result i32)
  (local $y i32)
  (local $x i32)
  (local $newY i32)
  (local $newX i32)
  (local $lines i32)
  (local $lineCount i32)
  (local $line i32)
  (local $c i32)
  (local $result i32)
  (local $ptr i32)
  (local $grid i32)

  (local.set $y (i32.const 2))
  (local.set $x (i32.const 0))

  (local.set $lines
    (local.set $lineCount
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $result (call $formatI64 (i64.const 0)))
  (local.set $ptr (i32.sub (i32.add (local.get $result) (i32.const 20)) (local.get $lineCount)))

  (local.set $grid (call $grid.create (i32.const 5) (i32.const 5)))
  (memory.copy
    (i32.add (local.get $grid) (i32.const 8))
    (i32.const 0x2_0200)
    (i32.const 30))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (loop $charLoop
          (if (local.tee $c (i32.load8_u (local.get $line)))
            (then
              (local.set $newX (local.get $x))
              (local.set $newY (local.get $y))

              (if (i32.eq (local.get $c) (i32.const 0x55 (;'U';)))
                (then (local.set $newY (i32.sub (local.get $y) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x52 (;'R';)))
                (then (local.set $newX (i32.add (local.get $x) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x44 (;'D';)))
                (then (local.set $newY (i32.add (local.get $y) (i32.const 1)))))
              (if (i32.eq (local.get $c) (i32.const 0x4c (;'L';)))
                (then (local.set $newX (i32.sub (local.get $x) (i32.const 1)))))

              (local.set $c
                (call $grid.getSafe (local.get $grid) (local.get $newY) (local.get $newX) (i32.const 0x2e)))
              (if (i32.ne (local.get $c) (i32.const 0x2e))
                (then
                  (local.set $x (local.get $newX))
                  (local.set $y (local.get $newY))))
              (local.set $line (i32.add (local.get $line) (i32.const 1)))
              (br $charLoop))))
        (i32.store8 (local.get $ptr)
          (call $grid.get (local.get $grid) (local.get $y) (local.get $x)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.get $result))

(elem (table $mains) (i32.const 2) $day02.main)
(func $day02.main (result i32)
  (call $checkInputStr
    (i32.const 0x2_0020)
    (i32.const 0x020) (i32.const 0x2_0040)
    (i32.const 0x021) (i32.const 0x2_0060))
  (call $checkInputStr
    (i32.const 0x2_0120)
    (i32.const 0x020) (i32.const 0x2_0140)
    (i32.const 0x021) (i32.const 0x2_0160))
  i32.add
)
