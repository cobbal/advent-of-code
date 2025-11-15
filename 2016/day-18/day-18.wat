(data (i32.const 0x12_8000) "day-18/input-ex0.txt")
(data (i32.const 0x12_8020) "day-18/input-real0.txt")

(elem (table $fns) (i32.const 0x120) $day18.part0.ex $day18.part0.real $day18.part1)

(func $day18.common (param $filename i32) (param $height i32) (result i64)
  (local $row0 i32)
  (local $width i32)
  (local $grid i32)
  (local $y i32)
  (local $x i32)
  (local $yp i32)
  (local $mask i32)
  (local $safe i32)

  (local.set $row0
    (i32.load
      (drop
        (call $splitDestructively
          (drop (call $readFile (local.get $filename)))
          (i32.const 0x0a)
          (i32.const 0)))))

  (local.set $width (call $strlen (local.get $row0)))
  (local.set $grid
    (call $grid.create (local.get $height) (local.get $width)))
  (call $grid.fill (local.get $grid) (i32.const 0x5f (; '_' ;)))
  (memory.copy
    (call $grid._ptr (local.get $grid) (i32.const 0) (i32.const 0))
    (local.get $row0)
    (local.get $width))

  (local.set $y (i32.const 1))
  (loop $loop.y
    (local.set $x (local.get $width))
    (loop $loop.x
      (local.set $x (i32.sub (local.get $x) (i32.const 1)))

      (local.set $yp (i32.sub (local.get $y) (i32.const 1)))
      (i32.const 0)
      (i32.add
        (i32.shl
          (i32.eq
            (call $grid.getSafe
              (local.get $grid)
              (local.get $yp) (i32.sub (local.get $x) (i32.const 1))
              (i32.const 0x2e (; '.' ;)))
            (i32.const 0x5e (; '^' ;)))
          (i32.const 2)))
      (i32.add
        (i32.shl
          (i32.eq
            (call $grid.getSafe
              (local.get $grid)
              (local.get $yp) (local.get $x)
              (i32.const 0x2e (; '.' ;)))
            (i32.const 0x5e (; '^' ;)))
          (i32.const 1)))
      (i32.add
        (i32.shl
          (i32.eq
            (call $grid.getSafe
              (local.get $grid)
              (local.get $yp) (i32.add (local.get $x) (i32.const 1))
              (i32.const 0x2e (; '.' ;)))
            (i32.const 0x5e (; '^' ;)))
          (i32.const 0)))
      (local.set $mask)

      (i32.const 0)
      (i32.or (i32.eq (local.get $mask) (i32.const 6)))
      (i32.or (i32.eq (local.get $mask) (i32.const 3)))
      (i32.or (i32.eq (local.get $mask) (i32.const 4)))
      (i32.or (i32.eq (local.get $mask) (i32.const 1)))
      (i32.mul (i32.const 0x30))
      (i32.add (i32.const 0x2e))
      (local.set $mask)

      (call $grid.set (local.get $grid) (local.get $y) (local.get $x) (local.get $mask))

      (br_if $loop.x (local.get $x)))
    (local.set $y (i32.add (local.get $y) (i32.const 1)))
    (br_if $loop.y (i32.lt_s (local.get $y) (local.get $height))))

  (local.set $y (local.get $height))
  (loop $count.y
    (local.set $y (i32.sub (local.get $y) (i32.const 1)))
    (local.set $x (local.get $width))
    (loop $count.x
      (local.set $x (i32.sub (local.get $x) (i32.const 1)))
      (if (i32.eq (call $grid.get (local.get $grid) (local.get $y) (local.get $x)) (i32.const 0x2e))
        (then (local.set $safe (i32.add (local.get $safe) (i32.const 1)))))
      (br_if $count.x (local.get $x)))
    (br_if $count.y (local.get $y)))

  ;; (call $grid.print (local.get $grid))

  (i64.extend_i32_s (local.get $safe)))

(func $day18.part0.ex (param $filename i32) (result i64)
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
