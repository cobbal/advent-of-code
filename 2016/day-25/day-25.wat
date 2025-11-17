(data (i32.const 0x19_8020) "day-25/input-real0.txt")

(elem (table $fns) (i32.const 0x190) $day25.part0 $day25.part1)

(func $day25.isBlinky (param $i i32) (result i32)
  (loop $loop
    (if (i32.ne (i32.and (local.get $i) (i32.const 0x3)) (i32.const 2))
      (then (return (i32.const 0))))
    (local.set $i (i32.shr_u (local.get $i) (i32.const 2)))
    (br_if $loop (local.get $i)))
  (i32.const 1))

(func $day25.part0 (param $filename i32) (result i64)
  (local $words i32)
  (local $a0 i32)
  (local $b0 i32)
  (local $c0 i32)

  (local.set $words
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x20)
        (i32.const 0))))
  (local.set $c0 (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 12)))))
  (local.set $b0 (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 20)))))

  (loop $loop
    (local.set $a0 (i32.add (local.get $a0) (i32.const 1)))
    (br_if $loop
      (i32.eqz
        (call $day25.isBlinky (i32.add (local.get $a0) (i32.mul (local.get $c0) (local.get $b0)))))))
  (i64.extend_i32_s (local.get $a0)))

(func $day25.part1 (param $filename i32) (result i64)
  (i64.const 0))

(elem (table $mains) (i32.const 25) $day25.main)
(func $day25.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x19_8020)
      (i32.const 0x190) (i64.const 196)
      (i32.const 0x191) (i64.const 0)))
  nop)
