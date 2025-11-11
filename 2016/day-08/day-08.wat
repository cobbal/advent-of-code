(data (i32.const 0x08_8000) "day-08/input-ex0.txt")
(data (i32.const 0x08_8020) "                   6")
(data (i32.const 0x08_8040)
  "\n"
  ".#..#.#\n"
  "#.#....\n"
  ".#.....\n")
(data (i32.const 0x08_9000) "day-08/input-real0.txt")
(data (i32.const 0x08_9020) "                 115")
(data (i32.const 0x08_9040)
  "\n"
  "####.####.####.#...##..#.####.###..####..###...##.\n"
  "#....#....#....#...##.#..#....#..#.#......#.....#.\n"
  "###..###..###...#.#.##...###..#..#.###....#.....#.\n"
  "#....#....#......#..#.#..#....###..#......#.....#.\n"
  "#....#....#......#..#.#..#....#.#..#......#..#..#.\n"
  "####.#....####...#..#..#.#....#..#.#.....###..##..\n")

(data (i32.const 0x08_0000) "rect")
(data (i32.const 0x08_0010) "row")
(data (i32.const 0x08_0020) "column")

(elem (table $fns) (i32.const 0x080) $day08.part0.ex $day08.part1.ex $day08.part0.real $day08.part1.real)

(func $day08.part0.common (param $filename i32) (param $h i32) (param $w i32) (result i32)
  (local $grid i32)
  (local $lines i32)
  (local $line i32)
  (local $words i32)
  (local $word i32)
  (local $x i32)
  (local $y i32)
  (local $delta i32)
  (local $scratch i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $grid (call $grid.create (local.get $h) (local.get $w)))
  (call $grid.fill (local.get $grid) (i32.const 0x2e (; '.' ;)))
  ;; (call $grid.print (local.get $grid))
  (local.set $scratch (call $malloc (i32.add (call $i32.max (local.get $h) (local.get $w)) (i32.const 1))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        ;; (call $printStr.nl (local.get $line))
        (local.set $words
          (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))
        (local.set $word (i32.load (i32.add (local.get $words) (i32.const 4))))
        (block $continue
          (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x08_0000)))
            (then
            ;; rect _x_
              (local.set $words
                (drop (call $splitDestructively (local.get $word) (i32.const 0x78 (; 'x' ;)) (i32.const 0))))
              (local.set $w (call $parseI32 (i32.load (local.get $words))))
              (local.set $h (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $y (i32.const 0))
              (loop $loopY
                (if (i32.lt_u (local.get $y) (local.get $h))
                  (then
                    (local.set $x (i32.const 0))
                    (loop $loopX
                      (if (i32.lt_u (local.get $x) (local.get $w))
                        (then
                          (call $grid.set (local.get $grid) (local.get $y) (local.get $x) (i32.const 0x23))
                          (local.set $x (i32.add (local.get $x) (i32.const 1)))
                          (br $loopX))))
                    (local.set $y (i32.add (local.get $y) (i32.const 1)))
                    (br $loopY))))
              (br $continue)))

          ;; parse commonality in row/column commands
          (local.set $y
            (call $parseI32
              (i32.add
                (i32.const 2)
                (i32.load (i32.add (local.get $words) (i32.const 8))))))
          (local.set $x (local.get $y))
          (local.set $delta (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 16)))))
          (local.set $h (call $grid.getHeight (local.get $grid)))
          (local.set $w (call $grid.getWidth (local.get $grid)))

          (if (i32.eqz (call $strcmp (local.get $word) (i32.const 0x08_0010)))
            (then
            ;; rotate row y=_ by _
              (local.set $x (i32.const 0))
              (memory.copy
                (local.get $scratch)
                (call $grid._ptr (local.get $grid) (local.get $y) (i32.const 0))
                (local.get $w))

              (loop $loopX
                (if (i32.lt_u (local.get $x) (local.get $w))
                  (then
                    (call $grid.setWrap
                      (local.get $grid)
                      (local.get $y)
                      (i32.add (local.get $x) (local.get $delta))
                      (i32.load8_u (i32.add (local.get $scratch) (local.get $x))))

                    (local.set $x (i32.add (local.get $x) (i32.const 1)))
                    (br $loopX))))
              (br $continue)))
          (if (i32.eqz (call $strcmp (local.get $word) (i32.const 0x08_0020)))
            (then
            ;; rotate column x=_ by _
              (local.set $y (i32.const 0))
              (loop $copyColumn
                (if (i32.lt_u (local.get $y) (local.get $h))
                  (then
                    (i32.store8
                      (i32.add (local.get $scratch) (local.get $y))
                      (call $grid.get (local.get $grid) (local.get $y) (local.get $x)))
                    (local.set $y (i32.add (local.get $y) (i32.const 1)))
                    (br $copyColumn))))

              (local.set $y (i32.const 0))
              (loop $loopY
                (if (i32.lt_u (local.get $y) (local.get $h))
                  (then
                    (call $grid.setWrap
                      (local.get $grid)
                      (i32.add (local.get $y) (local.get $delta))
                      (local.get $x)
                      (i32.load8_u (i32.add (local.get $scratch) (local.get $y))))

                    (local.set $y (i32.add (local.get $y) (i32.const 1)))
                    (br $loopY))))
              (br $continue)))
          ;; unknown command
          (unreachable))
        ;; (call $grid.print (local.get $grid))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))
  (local.get $grid))

(func $day08.part0.ex (param $filename i32) (result i32)
  (local $grid i32)
  (local.set $grid
    (call $day08.part0.common (local.get $filename) (i32.const 3) (i32.const 7)))
  (call $formatI64 (i64.extend_i32_s (call $grid.count (local.get $grid) (i32.const 0x23)))))
(func $day08.part1.ex (param $filename i32) (result i32)
  (local $grid i32)
  (local.set $grid
    (call $day08.part0.common (local.get $filename) (i32.const 3) (i32.const 7)))
  (call $grid.format (local.get $grid)))

(func $day08.part0.real (param $filename i32) (result i32)
  (local $grid i32)
  (local.set $grid
    (call $day08.part0.common (local.get $filename) (i32.const 6) (i32.const 50)))
  (call $formatI64 (i64.extend_i32_s (call $grid.count (local.get $grid) (i32.const 0x23)))))
(func $day08.part1.real (param $filename i32) (result i32)
  (local $grid i32)
  (local.set $grid
    (call $day08.part0.common (local.get $filename) (i32.const 6) (i32.const 50)))
  (call $grid.format (local.get $grid)))

(elem (table $mains) (i32.const 8) $day08.main)
(func $day08.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x08_8000)
      (i32.const 0x080) (i32.const 0x08_8020)
      (i32.const 0x081) (i32.const 0x08_8040)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x08_9000)
      (i32.const 0x082) (i32.const 0x08_9020)
      (i32.const 0x083) (i32.const 0x08_9040)))
  nop)
