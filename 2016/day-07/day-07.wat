(data (i32.const 0x07_8000) "day-07/input-ex0.txt")
(data (i32.const 0x07_8020) "day-07/input-ex1.txt")
(data (i32.const 0x07_8040) "day-07/input-real0.txt")

(elem (i32.const 0x070) $day07.part0 $day07.part1)

(func $day07.part0 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $c0 i32) 
  (local $c1 i32)
  (local $c2 i32)
  (local $c3 i32)
  (local $hasAbba i32)
  (local $inBracket i32)
  (local $result i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $hasAbba (i32.const 0))
        (block $fail
          (loop $charLoop
            (local.set $c0 (i32.load8_u (i32.add (local.get $line) (i32.const 0))))
            (local.set $c1 (i32.load8_u (i32.add (local.get $line) (i32.const 1))))
            (local.set $c2 (i32.load8_u (i32.add (local.get $line) (i32.const 2))))
            (local.set $c3 (i32.load8_u (i32.add (local.get $line) (i32.const 3))))
            (if (local.get $c3)
              (then
                (block $continue
                  (if (i32.eq (local.get $c0) (i32.const 0x5b (; '[' ;)))
                    (then
                      (local.set $inBracket (i32.const 1))
                      (br $continue)))
                  (if (i32.eq (local.get $c0) (i32.const 0x5d (; ']' ;)))
                    (then
                      (local.set $inBracket (i32.const 0))
                      (br $continue)))

                  (if
                    (i32.and
                      (i32.eq (local.get $c0) (local.get $c3))
                      (i32.and
                        (i32.eq (local.get $c1) (local.get $c2))
                        (i32.ne (local.get $c0) (local.get $c1))))
                      (then
                    (if (local.get $inBracket)
                      (then (br $fail))
                      (else (local.set $hasAbba (i32.const 1)))))))
                (local.set $line (i32.add (local.get $line) (i32.const 1)))
                (br $charLoop))))
          (local.set $result (i32.add (local.get $result) (local.get $hasAbba))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (i64.extend_i32_s (local.get $result)))

(func $day07.part1 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $c0 i32) 
  (local $c1 i32)
  (local $c2 i32)
  (local $inBracket i32)
  (local $result i32)
  (local $abaTable i32)
  (local $i i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))
  (local.set $abaTable (call $malloc (i32.const 0x1_0000)))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (memory.fill (local.get $abaTable) (i32.const 0) (i32.const 0x1_0000))
        (loop $charLoop
          (local.set $c0 (i32.load8_u (i32.add (local.get $line) (i32.const 0))))
          (local.set $c1 (i32.load8_u (i32.add (local.get $line) (i32.const 1))))
          (local.set $c2 (i32.load8_u (i32.add (local.get $line) (i32.const 2))))
          (if (local.get $c2)
            (then
              (block $continue
                (if (i32.eq (local.get $c0) (i32.const 0x5b (; '[' ;)))
                  (then
                    (local.set $inBracket (i32.const 1))
                    (br $continue)))
                (if (i32.eq (local.get $c0) (i32.const 0x5d (; ']' ;)))
                  (then
                    (local.set $inBracket (i32.const 0))
                    (br $continue)))

                (if
                  (i32.and
                    (i32.eq (local.get $c0) (local.get $c2))
                    (i32.and
                      (i32.ne (local.get $c1) (i32.const 0x5b))
                      (i32.and
                        (i32.ne (local.get $c1) (i32.const 0x5d))
                        (i32.ne (local.get $c0) (local.get $c1)))))
                    (then
                  (if (local.get $inBracket)
                    (then (local.set $i (i32.add (i32.shl (local.get $c1) (i32.const 8)) (local.get $c0))))
                    (else (local.set $i (i32.add (i32.shl (local.get $c0) (i32.const 8)) (local.get $c1)))))
                  (local.set $i (i32.add (local.get $i) (local.get $abaTable)))
                  (i32.store8 (local.get $i)
                    (i32.or
                      (i32.load8_u (local.get $i))
                      (i32.shl (i32.const 1) (local.get $inBracket)))))))
              (local.set $line (i32.add (local.get $line) (i32.const 1)))
              (br $charLoop))))

        (local.set $i (i32.const 0))
        (loop $findLoop
          (if (i32.lt_u (local.get $i) (i32.const 0x1_0000))
            (then
              (if (i32.eq (i32.load8_u (i32.add (local.get $abaTable) (local.get $i))) (i32.const 3))
                (then
                  (local.set $result (i32.add (local.get $result) (i32.const 1))))
                (else
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $findLoop))))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (i64.extend_i32_s (local.get $result)))

(elem (table $mains) (i32.const 7) $day07.main)
(func $day07.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x07_8000)
      (i32.const 0x070) (i64.const 2)
      (i32.const 0x071) (i64.const 0)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x07_8020)
      (i32.const 0x070) (i64.const 0)
      (i32.const 0x071) (i64.const 3)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x07_8040)
      (i32.const 0x070) (i64.const 118)
      (i32.const 0x071) (i64.const 260)))
  nop)
