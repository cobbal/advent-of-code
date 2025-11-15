(data (i32.const 0x14_8000) "day-20/input-ex0.txt")
(data (i32.const 0x14_8020) "day-20/input-real0.txt")

(global $day20.fst.cmp_u i32 (i32.const 0x140))
(elem (table $fns) (i32.const 0x140) $day20.fst.cmp_u $day20.part0 $day20.part1)

(func $day20.fst.cmp_u (param $a i32) (param $b i32) (result i32)
  (local.set $a (i32.load (local.get $a)))
  (local.set $b (i32.load (local.get $b)))
  (if (i32.lt_u (local.get $a) (local.get $b))
    (then (return (i32.const -1))))
  (i32.gt_u (local.get $a) (local.get $b)))

(func $day20.common (param $filename i32) (result i64 i64)
  (local $lines i32)
  (local $n i32)
  (local $line i32)
  (local $words i32)
  (local $start i64)
  (local $end i64)
  (local $ptr i32)
  (local $ranges i32)
  (local $nextSpace i64)
  (local $lowest i64)
  (local $safe i64)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $ptr
    (local.tee $ranges
      (call $malloc (i32.shl (i32.add (local.get $n) (i32.const 1)) (i32.const 2)))))

  (loop $loop.lines
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $words
          (drop (call $splitDestructively (local.get $line) (i32.const 0x2d) (i32.const 0))))
        (i32.store
          (local.get $ptr)
          (call $cons
            (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 0))))
            (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4))))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (br $loop.lines))))

  (call $sort.i32 (global.get $day20.fst.cmp_u) (local.get $ranges) (local.get $n))

  (local.set $ptr (local.get $ranges))
  (loop $search
    (if (i32.load (local.get $ptr))
      (then
        (local.set $start (i64.extend_i32_u (call $car (i32.load (local.get $ptr)))))
        (local.set $end (i64.extend_i32_u (call $cdr (i32.load (local.get $ptr)))))

        (if (i64.lt_u (local.get $nextSpace) (local.get $start))
          (then
            (if (i64.eqz (local.get $lowest))
              (then (local.set $lowest (local.get $nextSpace))))
            (local.set $safe
              (i64.add
                (local.get $safe)
                (i64.sub (local.get $start) (local.get $nextSpace))))))
        (if (i64.le_u (local.get $nextSpace) (local.get $end))
          (then (local.set $nextSpace (i64.add (local.get $end) (i64.const 1)))))

        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (br $search))))

  (local.set $safe
    (i64.add
      (local.get $safe)
      (i64.sub (i64.const 0x1_0000_0000) (local.get $nextSpace))))

  (local.get $lowest) (local.get $safe))

(func $day20.part0 (param $filename i32) (result i64)
  (return (drop (call $day20.common (local.get $filename)))))

(func $day20.part1 (param $filename i32) (result i64)
  (drop (return (call $day20.common (local.get $filename)))))

(elem (table $mains) (i32.const 20) $day20.main)
(func $day20.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x14_8000)
      (i32.const 0x141) (i64.const 3)
      (i32.const 0x142) (i64.const 2)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x14_8020)
      (i32.const 0x141) (i64.const 22887907)
      (i32.const 0x142) (i64.const 109)))
  nop)
