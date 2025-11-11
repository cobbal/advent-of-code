(data (i32.const 0x1_0020) "day-01/input-ex0.txt")
(data (i32.const 0x1_0040) "day-01/input-ex1.txt")
(data (i32.const 0x1_0060) "day-01/input-ex2.txt")
(data (i32.const 0x1_0080) "day-01/input-ex3.txt")
(data (i32.const 0x1_00a0) "day-01/input-real0.txt")

(elem (table $fns) (i32.const 0x010) $day01.part0 $day01.part1)

(func $day01.parse (param $filename i32) (result i32 i32)
  (local $words i32)
  (local $wordsLen i32)
  (local $word i32)
  (local $dir i32)
  (local $dist i32)
  (local $result i32)
  (local $resultPtr i32)

  (local.set $words
    (local.set $wordsLen
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x20)
        (i32.const 0))))

  (local.set $resultPtr
    (local.tee $result
      (call $malloc
        (i32.mul
          (i32.const 8)
          (local.get $wordsLen)))))

  (loop $loop
    (if (local.tee $word (i32.load (local.get $words)))
      (then
        (local.set $dir
          (i32.rem_u
            (i32.add
              (local.get $dir)
              (i32.add (i32.const 1)
                (i32.mul (i32.const 2)
                  (i32.eq (i32.load8_u (local.get $word)) (i32.const 0x4c)))))
            (i32.const 4)))
        (local.set $dist
          (call $parseI32 (i32.add (local.get $word) (i32.const 1))))
        (i32.store (local.get $resultPtr) (local.get $dir))
        (i32.store (i32.add (local.get $resultPtr) (i32.const 4)) (local.get $dist))
        (local.set $words (i32.add (local.get $words) (i32.const 4)))
        (local.set $resultPtr (i32.add (local.get $resultPtr) (i32.const 8)))
        (br $loop))))
  (local.get $result) (local.get $wordsLen))


(func $day01.part0 (param $filename i32) (result i64)
  (local $parse i32)
  (local $count i32)
  (local $dir i32)
  (local $dist i32)
  (local $x i32)
  (local $y i32)

  (local.set $parse
    (local.set $count
      (call $day01.parse (local.get $filename))))

  (loop $loop
    (if (local.get $count)
      (then
        (local.set $dir (i32.load (local.get $parse)))
        (local.set $dist (i32.load (i32.add (local.get $parse) (i32.const 4))))
        (local.set $parse (i32.add (local.get $parse) (i32.const 8)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1)))
        (if (i32.eq (local.get $dir) (i32.const 0))
          (then (local.set $y (i32.sub (local.get $y) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 1))
          (then (local.set $x (i32.add (local.get $x) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 2))
          (then (local.set $y (i32.add (local.get $y) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 3))
          (then (local.set $x (i32.sub (local.get $x) (local.get $dist)))))
        (br $loop))))
  (i64.extend_i32_s
    (i32.add (call $i32.abs (local.get $x)) (call $i32.abs (local.get $y)))))

(func $day01.extrema (param $parse i32) (param $count i32) (result i32 i32 i32 i32)
  (local $dir i32)
  (local $dist i32)
  (local $x i32)
  (local $y i32)
  (local $minX i32)
  (local $maxX i32)
  (local $minY i32)
  (local $maxY i32)
  (loop $loop
    (if (local.get $count)
      (then
        (local.set $dir (i32.load (local.get $parse)))
        (local.set $dist (i32.load (i32.add (local.get $parse) (i32.const 4))))
        (local.set $parse (i32.add (local.get $parse) (i32.const 8)))
        (local.set $count (i32.sub (local.get $count) (i32.const 1)))
        (if (i32.eq (local.get $dir) (i32.const 0))
          (then (local.set $y (i32.sub (local.get $y) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 1))
          (then (local.set $x (i32.add (local.get $x) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 2))
          (then (local.set $y (i32.add (local.get $y) (local.get $dist)))))
        (if (i32.eq (local.get $dir) (i32.const 3))
          (then (local.set $x (i32.sub (local.get $x) (local.get $dist)))))
        (local.set $minX (call $i32.min (local.get $minX) (local.get $x)))
        (local.set $maxX (call $i32.max (local.get $maxX) (local.get $x)))
        (local.set $minY (call $i32.min (local.get $minY) (local.get $y)))
        (local.set $maxY (call $i32.max (local.get $maxY) (local.get $y)))
        (br $loop))))
  (local.get $minX) (local.get $maxX) (local.get $minY) (local.get $maxY))

(func $day01.part1 (param $filename i32) (result i64)
  (local $parse i32)
  (local $count i32)
  (local $dir i32)
  (local $dist i32)
  (local $x i32)
  (local $y i32)
  (local $dx i32)
  (local $dy i32)
  (local $w i32)
  (local $h i32)
  (local $minX i32)
  (local $maxX i32)
  (local $minY i32)
  (local $maxY i32)
  (local $grid i32)

  (local.set $parse
    (local.set $count
      (call $day01.parse (local.get $filename))))

  (local.set $minX
    (local.set $maxX
      (local.set $minY
        (local.set $maxY
          (call $day01.extrema (local.get $parse) (local.get $count))))))

  (local.set $x (i32.sub (i32.const 0) (local.get $minX)))
  (local.set $y (i32.sub (i32.const 0) (local.get $minY)))
  (local.set $w (i32.add (i32.sub (local.get $maxX) (local.get $minX)) (i32.const 1)))
  (local.set $h (i32.add (i32.sub (local.get $maxY) (local.get $minY)) (i32.const 1)))

  (local.set $grid (call $grid.create (local.get $h) (local.get $w)))
  (call $grid.fill (local.get $grid) (i32.const 0x2e (;'.';)))
  (call $grid.set (local.get $grid) (local.get $y) (local.get $x) (i32.const 0x30))

  (block $break
    (loop $loop
      (if (local.get $count)
        (then
          (local.set $dir (i32.load (local.get $parse)))
          (local.set $dist (i32.load (i32.add (local.get $parse) (i32.const 4))))
          (local.set $parse (i32.add (local.get $parse) (i32.const 8)))
          (local.set $count (i32.sub (local.get $count) (i32.const 1)))
          (if (i32.eq (local.get $dir) (i32.const 0))
            (then (local.set $dy (i32.const -1)) (local.set $dx (i32.const 0))))
          (if (i32.eq (local.get $dir) (i32.const 1))
            (then (local.set $dy (i32.const 0)) (local.set $dx (i32.const 1))))
          (if (i32.eq (local.get $dir) (i32.const 2))
            (then (local.set $dy (i32.const 1)) (local.set $dx (i32.const 0))))
          (if (i32.eq (local.get $dir) (i32.const 3))
            (then (local.set $dy (i32.const 0)) (local.set $dx (i32.const -1))))

          (loop $step
            (if (local.get $dist)
              (then
                (local.set $dist (i32.sub (local.get $dist) (i32.const 1)))
                (local.set $y (i32.add (local.get $y) (local.get $dy)))
                (local.set $x (i32.add (local.get $x) (local.get $dx)))
                (if (i32.ne (i32.const 0x2e) (call $grid.get (local.get $grid) (local.get $y) (local.get $x)))
                  (then (br $break)))
                (call $grid.set (local.get $grid) (local.get $y) (local.get $x) (i32.const 0x78))
                (br $step))))

          (br $loop)))))
  ;; (call $grid.print (local.get $grid))
  (i64.extend_i32_s
    (i32.add
      (call $i32.abs (i32.add (local.get $minX) (local.get $x)))
      (call $i32.abs (i32.add (local.get $minY) (local.get $y))))))

(elem (table $mains) (i32.const 1) $day01.main)
(func $day01.main (result i32)
  (call $checkInputI64
    (i32.const 0x1_0020)
    (i32.const 0x010) (i64.const 5)
    (i32.const 0x011) (i64.const 5))
  (call $checkInputI64
    (i32.const 0x1_0040)
    (i32.const 0x010) (i64.const 2)
    (i32.const 0x011) (i64.const 2))
  (call $checkInputI64
    (i32.const 0x1_0060)
    (i32.const 0x010) (i64.const 12)
    (i32.const 0x011) (i64.const 12))
  (call $checkInputI64
    (i32.const 0x1_0080)
    (i32.const 0x010) (i64.const 8)
    (i32.const 0x011) (i64.const 4))
  (call $checkInputI64
    (i32.const 0x1_00a0)
    (i32.const 0x010) (i64.const 273)
    (i32.const 0x011) (i64.const 115))
  i32.add i32.add i32.add i32.add)
