(data (i32.const 0x0d_8000) "day-13/input-ex0.txt")
(data (i32.const 0x0d_8020) "day-13/input-real0.txt")

(data (i32.const 0x0d_0000) "#")
(data (i32.const 0x0d_0010) ".")

(elem (table $fns) (i32.const 0x0d0) $day13.part0.ex $day13.part1 $day13.part0.real)

(func $isWall (param $key i32) (param $y i32) (param $x i32) (result i32)
  (if (i32.lt_s (local.get $x) (i32.const 0))
    (then (return (i32.const 1))))
  (if (i32.lt_s (local.get $y) (i32.const 0))
    (then (return (i32.const 1))))
  (i32.const 0)
  (i32.add (i32.mul (local.get $x) (local.get $x)))
  (i32.add (i32.mul (i32.const 3) (local.get $x)))
  (i32.add (i32.mul (i32.const 2) (i32.mul (local.get $x) (local.get $y))))
  (i32.add (local.get $y))
  (i32.add (i32.mul (local.get $y) (local.get $y)))
  (i32.add (local.get $key))
  (i32.popcnt)
  (i32.and (i32.const 1)))

(func $day13.try (param $set i32) (param $key i32) (param $y i32) (param $x i32) (result i32)
  (if (call $isWall (local.get $key) (local.get $y) (local.get $x))
    (then (return (local.get $set))))
  (call $set.insert
    (local.get $set)
    (i32.or
      (i32.shl (local.get $y) (i32.const 16))
      (local.get $x))))

(func $day13.expand (param $key i32) (param $set i32) (param $newSet i32) (result i32)
  (local $n i32)
  (local $x i32)
  (local $y i32)

  (local.set $n (call $map.count (local.get $set)))
  (local.set $set (call $map.elements (local.get $set)))

  (if (i32.eqz (local.get $n))
    (then (return (i32.const -2))))

  (loop $loop.pos
    (if (local.get $n)
      (then
        (local.set $x (i32.load (local.get $set)))
        (local.set $y (i32.shr_u (local.get $x) (i32.const 16)))
        (local.set $x (i32.and (local.get $x) (i32.const 0xffff)))

        (local.get $newSet)
        (call $day13.try (local.get $key) (i32.add (local.get $y) (i32.const 1)) (local.get $x))
        (call $day13.try (local.get $key) (i32.sub (local.get $y) (i32.const 1)) (local.get $x))
        (call $day13.try (local.get $key) (local.get $y) (i32.add (local.get $x) (i32.const 1)))
        (call $day13.try (local.get $key) (local.get $y) (i32.sub (local.get $x) (i32.const 1)))
        (local.set $newSet)

        (local.set $set (i32.add (local.get $set) (i32.const 8)))
        (local.set $n (i32.sub (local.get $n) (i32.const 1)))
        (br $loop.pos))))
  (local.get $newSet))


(func $day13.bfs (param $key i32) (param $positions i32) (param $goal i32) (param $depth i32) (result i32)
  (if (i32.eqz (call $map.count (local.get $positions)))
    (then (return (i32.const -2))))
  (if (call $set.contains (local.get $positions) (local.get $goal))
    (then (return (local.get $depth))))
  (return_call $day13.bfs
    (local.get $key)
    (call $day13.expand
      (local.get $key)
      (local.get $positions)
      (call $set.empty (global.get $fns.i32.cmp_s)))
    (local.get $goal)
    (i32.add (local.get $depth) (i32.const 1))))

(func $day13.part0.common (param $filename i32) (param $goalX i32) (param $goalY i32) (result i64)
  (local $key i32)
  (local $y i32)
  (local $x i32)
  (local.set $key (call $parseI32 (drop (call $readFile (local.get $filename)))))

  (i64.extend_i32_s
    (call $day13.bfs
      (local.get $key)
      (call $set.singleton (global.get $fns.i32.cmp_s) (i32.const 0x0001_0001))
      (i32.or
        (i32.shl (local.get $goalY) (i32.const 16))
        (local.get $goalX))
      (i32.const 0))))

(func $day13.part0.ex (param $filename i32) (result i64)
  (call $day13.part0.common (local.get $filename) (i32.const 7) (i32.const 4)))

(func $day13.part0.real (param $filename i32) (result i64)
  (call $day13.part0.common (local.get $filename) (i32.const 31) (i32.const 39)))

(func $day13.part1 (param $filename i32) (result i64)
  (local $key i32)
  (local $i i32)
  (local $set i32)

  (local.set $key (call $parseI32 (drop (call $readFile (local.get $filename)))))
  (local.set $set (call $set.singleton (global.get $fns.i32.cmp_s) (i32.const 0x0001_0001)))

  (loop $loop
    (if (i32.lt_s (local.get $i) (i32.const 50))
      (then
        (local.set $set
          (call $day13.expand (local.get $key) (local.get $set) (local.get $set)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop))))

  (i64.extend_i32_s (call $set.count (local.get $set))))

(elem (table $mains) (i32.const 13) $day13.main)
(func $day13.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0d_8000)
      (i32.const 0x0d0) (i64.const 11)
      (i32.const 0x0d1) (i64.const 151)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0d_8020)
      (i32.const 0x0d2) (i64.const 82)
      (i32.const 0x0d1) (i64.const 138)))
  nop)
