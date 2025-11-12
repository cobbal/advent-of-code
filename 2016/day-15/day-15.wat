(data (i32.const 0x0f_8000) "day-15/input-ex0.txt")
(data (i32.const 0x0f_8020) "day-15/input-real0.txt")

(elem (table $fns) (i32.const 0x0f0) $day15.part0 $day15.part1)

(func $day15.common (param $filename i32) (param $extra i32) (result i64)
  (local $lines i32)
  (local $n i32)
  (local $line i32)
  (local $words i32)
  (local $wheels i32)
  (local $ptr i32)
  (local $i i32)
  (local $mod i32)
  (local $start i32)
  (local $t i32)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))
  (if (local.get $extra)
    (then (local.set $n (i32.add (local.get $n) (i32.const 1)))))

  (local.set $wheels
    (local.tee $ptr
      (call $malloc (i32.mul (local.get $n) (i32.const 8)))))

  (loop $lines
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $words (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))

        (local.set $mod
          (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 12)))))
        (local.set $start
          (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 44)))))

        (i32.store (local.get $ptr) (local.get $mod))
        (i32.store
          (i32.add (local.get $ptr) (i32.const 4))
          (i32.add (local.get $start) (local.get $i)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 8)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lines))))

  (if (local.get $extra)
    (then
      (i32.store (local.get $ptr) (i32.const 11))
      (i32.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $i))))

  (local.set $t (i32.const -1))
  (loop $loop.t
    (local.set $t (i32.add (local.get $t) (i32.const 1)))
    (local.set $i (i32.const 0))
    (loop $loop.i
      (local.set $ptr (i32.add (local.get $wheels) (i32.mul (local.get $i) (i32.const 8))))
      (local.set $mod (i32.load (local.get $ptr)))
      (local.set $start (i32.load (i32.add (local.get $ptr) (i32.const 4))))

      (br_if $loop.t (i32.rem_u (i32.add (local.get $start) (local.get $t)) (local.get $mod)))

      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $loop.i (i32.lt_s (local.get $i) (local.get $n)))))
  (i64.extend_i32_s (local.get $t)))

(func $day15.part0 (param $filename i32) (result i64)
  (call $day15.common (local.get $filename) (i32.const 0)))

(func $day15.part1 (param $filename i32) (result i64)
  (call $day15.common (local.get $filename) (i32.const 1)))

(elem (table $mains) (i32.const 15) $day15.main)
(func $day15.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0f_8000)
      (i32.const 0x0f0) (i64.const 5)
      (i32.const 0x0f1) (i64.const 85)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0f_8020)
      (i32.const 0x0f0) (i64.const 376777)
      (i32.const 0x0f1) (i64.const 3903937)))
  nop)
