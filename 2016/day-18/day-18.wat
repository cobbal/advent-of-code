(data (i32.const 0x12_8000) "day-18/input-ex0.txt")
(data (i32.const 0x12_8020) "day-18/input-real0.txt")

(elem (table $fns) (i32.const 0x120) $day18.part0.ex $day18.part0.real $day18.part1)

(func $day18.common (param $filename i32) (param $height i32) (result i64)
  (local $row i32)
  (local $rowPrev i32)
  (local $width i32)
  (local $grid i32)
  (local $y i32)
  (local $x i32)
  (local $yp i32)
  (local $mask i32)
  (local $safe i32)

  (local.set $row
    (i32.load
      (drop
        (call $splitDestructively
          (drop (call $readFile (local.get $filename)))
          (i32.const 0x0a)
          (i32.const 0)))))
  (local.set $width (call $strlen (local.get $row)))
  (local.set $rowPrev (call $malloc (i32.add (local.get $width) (i32.const 2))))

  (loop $loop.init
    (if
      (i32.eqz
        (local.tee $mask
          (i32.eq (i32.load8_u (i32.add (local.get $row) (local.get $x))) (i32.const 0x5e (;'^';)))))
      (then (local.set $safe (i32.add (local.get $safe) (i32.const 1)))))
    (i32.store8
      (i32.add (local.get $rowPrev) (i32.add (local.get $x) (i32.const 1)))
      (local.get $mask))
    (local.set $x (i32.add (local.get $x) (i32.const 1)))
    (br_if $loop.init (i32.lt_s (local.get $x) (local.get $width))))

  (local.set $rowPrev (i32.add (local.get $rowPrev) (i32.const 1)))
  (local.set $row (i32.add (call $malloc (i32.add (local.get $width) (i32.const 2))) (i32.const 1)))

  (local.set $y (i32.const 1))
  (loop $loop.y
    (local.set $y (i32.add (local.get $y) (i32.const 1)))

    (local.set $x (i32.const -1))
    (loop $loop.x
      (local.set $mask
        (i32.and
          (i32.const 0x010101)
          (i32.load (i32.add (local.get $rowPrev) (local.get $x)))))

      (i32.const 0)
      (i32.or (i32.eq (local.get $mask) (i32.const 0x010100)))
      (i32.or (i32.eq (local.get $mask) (i32.const 0x000101)))
      (i32.or (i32.eq (local.get $mask) (i32.const 0x010000)))
      (i32.or (i32.eq (local.get $mask) (i32.const 0x000001)))
      (local.set $mask)

      (i32.store8
        (i32.add
          (local.get $row)
          (local.tee $x (i32.add (local.get $x) (i32.const 1))))
        (local.get $mask))

      (if (i32.eqz (local.get $mask))
        (then (local.set $safe (i32.add (local.get $safe) (i32.const 1)))))

      (br_if $loop.x (i32.lt_s (local.get $x) (i32.sub (local.get $width) (i32.const 1)))))

    (local.get $row)
    (local.get $rowPrev)
    (local.set $row)
    (local.set $rowPrev)

    (br_if $loop.y (i32.lt_s (local.get $y) (local.get $height))))
  (i64.extend_i32_s (local.get $safe)))

(func $day18.part0.ex (param $filename i32) (result i64)
  (call $debugger)
  (call $day18.common (local.get $filename) (i32.const 10)))

(func $day18.part0.real (param $filename i32) (result i64)
  (call $day18.common (local.get $filename) (i32.const 40)))

(func $day18.part1 (param $filename i32) (result i64)
  (call $day18.common (local.get $filename) (i32.const 400000)))

(elem (table $mains) (i32.const 18) $day18.main)
(func $day18.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x12_8000)
      (i32.const 0x120) (i64.const 38)
      (i32.const 0x122) (i64.const 1935478)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x12_8020)
      (i32.const 0x121) (i64.const 2005)
      (i32.const 0x122) (i64.const 20008491)))
  nop)
