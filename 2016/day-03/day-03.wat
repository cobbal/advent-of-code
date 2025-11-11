(data (i32.const 0x03_8000) "day-03/input-ex0.txt")
(data (i32.const 0x03_8020) "day-03/input-real0.txt")

(elem (table $fns) (i32.const 0x030) $day03.part0 $day03.part1)

(func $day03.isTri (param $a i32) (param $b i32) (param $c i32) (result i32)
  (local $longestSide i32)
  (local.set $longestSide (call $i32.max (local.get $a) (call $i32.max (local.get $b) (local.get $c))))
  (i32.gt_s
    (i32.add (local.get $a) (i32.add (local.get $b) (local.get $c)))
    (i32.shl (local.get $longestSide) (i32.const 1))))

(func $day03.part0 (param $filename i32) (result i64)
  (local $lines i32)
  (local $linesLen i32)
  (local $words i32)
  (local $wordsLen i32)
  (local $result i64)

  (local.set $lines
    (local.set $linesLen
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.get $linesLen)
      (then
        (local.set $words
          (local.set $wordsLen
            (call $splitDestructively
              (i32.load (local.get $lines))
              (i32.const 0x20)
              (i32.const 0))))
        (call $assert (i32.eq (local.get $wordsLen) (i32.const 3)))

        (if
          (call $day03.isTri
            (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 0))))
            (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4))))
            (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 8)))))
            (then (local.set $result (i64.add (local.get $result) (i64.const 1)))))

        (local.set $linesLen (i32.sub (local.get $linesLen) (i32.const 1)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.get $result))

(func $day03.part1 (param $filename i32) (result i64)
  (local $lines i32)
  (local $linesLen i32)
  (local $words0 i32)
  (local $words1 i32)
  (local $words2 i32)
  (local $wordsLen i32)
  (local $result i64)

  (local.set $lines
    (local.set $linesLen
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))
  (call $assert_not (i32.rem_u (local.get $linesLen) (i32.const 3)))

  (loop $lineLoop
    (if (local.get $linesLen)
      (then
        (local.set $words0
          (local.set $wordsLen
            (call $splitDestructively
              (i32.load (i32.add (local.get $lines) (i32.const 0)))
              (i32.const 0x20)
              (i32.const 0))))
        (call $assert (i32.eq (local.get $wordsLen) (i32.const 3)))

        (local.set $words1
          (local.set $wordsLen
            (call $splitDestructively
              (i32.load (i32.add (local.get $lines) (i32.const 4)))
              (i32.const 0x20)
              (i32.const 0))))
        (call $assert (i32.eq (local.get $wordsLen) (i32.const 3)))

        (local.set $words2
          (local.set $wordsLen
            (call $splitDestructively
              (i32.load (i32.add (local.get $lines) (i32.const 8)))
              (i32.const 0x20)
              (i32.const 0))))
        (call $assert (i32.eq (local.get $wordsLen) (i32.const 3)))

        (if
          (call $day03.isTri
            (call $parseI32 (i32.load (i32.add (local.get $words0) (i32.const 0))))
            (call $parseI32 (i32.load (i32.add (local.get $words1) (i32.const 0))))
            (call $parseI32 (i32.load (i32.add (local.get $words2) (i32.const 0)))))
            (then (local.set $result (i64.add (local.get $result) (i64.const 1)))))

        (if
          (call $day03.isTri
            (call $parseI32 (i32.load (i32.add (local.get $words0) (i32.const 4))))
            (call $parseI32 (i32.load (i32.add (local.get $words1) (i32.const 4))))
            (call $parseI32 (i32.load (i32.add (local.get $words2) (i32.const 4)))))
            (then (local.set $result (i64.add (local.get $result) (i64.const 1)))))

        (if
          (call $day03.isTri
            (call $parseI32 (i32.load (i32.add (local.get $words0) (i32.const 8))))
            (call $parseI32 (i32.load (i32.add (local.get $words1) (i32.const 8))))
            (call $parseI32 (i32.load (i32.add (local.get $words2) (i32.const 8)))))
            (then (local.set $result (i64.add (local.get $result) (i64.const 1)))))

        (local.set $linesLen (i32.sub (local.get $linesLen) (i32.const 3)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 12)))
        (br $lineLoop))))

  (local.get $result))

(elem (table $mains) (i32.const 3) $day03.main)
(func $day03.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x03_8000)
      (i32.const 0x030) (i64.const 2)
      (i32.const 0x031) (i64.const 1)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x03_8020)
      (i32.const 0x030) (i64.const 982)
      (i32.const 0x031) (i64.const 1826)))
  nop)
