(data (i32.const 0x04_8000) "day-04/input-ex0.txt")
(data (i32.const 0x04_8020) "day-04/input-real0.txt")

(data (i32.const 0x04_0000) "northpole-object-storage")

(elem (i32.const 0x040) $day04.part0 $day04.part1)

(func $day04.part0 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $result i64)
  (local $checksum i32)
  (local $sector i32)
  (local $histogram i32)
  (local $offset i32)
  (local $c i32)
  (local $i i32)
  (local $readoutPtr i32)

  (local.set $histogram (call $malloc (i32.const 0x400)))

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (memory.fill (local.get $histogram) (i32.const 0) (i32.const 0x400))
        (local.set $c (i32.const 0x61 (; 'a' ;)))
        (loop $markLetter
          (if (i32.le_u (local.get $c) (i32.const 0x7a (;'z';)))
            (then
              (i32.store
                (i32.add (local.get $histogram)
                  (i32.mul (i32.const 4) (local.get $c)))
                (i32.or
                  (local.get $c)
                  (i32.const 0x1000_0000)))
              (local.set $c (i32.add (local.get $c) (i32.const 1)))
              (br $markLetter))))

        (local.set $checksum (call $strrchr (local.get $line) (i32.const 0x5b (;'[';))))
        (call $assert (local.get $checksum))
        (i32.store8 (local.get $checksum) (i32.const 0))
        (local.set $checksum (i32.add (local.get $checksum) (i32.const 1)))
        (local.set $sector
          (call $parseI32
            (i32.add
              (call $strrchr (local.get $line) (i32.const 0x2d (;'-';)))
              (i32.const 1))))

        (loop $charLoop
          (if (local.tee $offset (i32.mul (i32.const 4) (i32.load8_u (local.get $line))))
            (then
              (i32.store
                (i32.add (local.get $histogram) (local.get $offset))
                (i32.add (i32.const -0x100)
                  (i32.load (i32.add (local.get $histogram) (local.get $offset)))))
              (local.set $line (i32.add (local.get $line) (i32.const 1)))
              (br $charLoop))))

        (call $sort.i32_s
          (local.tee
            $readoutPtr
            (i32.add (local.get $histogram) (i32.const 0x184 (; 'a' * 4 ;))))
          (i32.const 26))

        (local.set $i (i32.const 5))

        (block $bogus
          (loop $checkChecksum
            (if (local.get $i)
              (then
                (if
                  (i32.ne
                    (i32.load8_u (local.get $readoutPtr))
                    (i32.load8_u (local.get $checksum)))
                    (then (br $bogus)))
                (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                (local.set $readoutPtr (i32.add (local.get $readoutPtr) (i32.const 4)))
                (local.set $checksum (i32.add (local.get $checksum) (i32.const 1)))
                (br $checkChecksum))))
          ;; checksum is good
          (local.set $result (i64.add (local.get $result) (i64.extend_i32_s (local.get $sector)))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.get $result))

(func $day04.part1 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $checksum i32)
  (local $sector i32)
  (local $offset i32)
  (local $ptr i32)
  (local $c i32)
  (local $i i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $checksum
          (i32.add 
            (call $strrchr (local.get $line) (i32.const 0x5b (;'[';)))
            (i32.const 1)))

        (local.set $sector
          (call $parseI32
            (i32.add
              (local.tee $ptr (call $strrchr (local.get $line) (i32.const 0x2d (;'-';))))
              (i32.const 1))))
        (call $assert (local.get $ptr))
        (i32.store8 (local.get $ptr) (i32.const 0))

        (local.set $ptr (local.get $line))
        (loop $charLoop
          (if (local.tee $c (i32.load8_u (local.get $ptr)))
            (then
              (if (i32.lt_u (local.tee $i (i32.sub (local.get $c) (i32.const 0x61))) (i32.const 26))
                (then
                  (i32.store8
                    (local.get $ptr)
                    (i32.add
                      (i32.const 0x61
                        (i32.rem_u
                          (i32.add (local.get $i) (local.get $sector))
                          (i32.const 26)))))))
              (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
              (br $charLoop))))

        (if (i32.eqz (call $strcmp (local.get $line) (i32.const 0x04_0000)))
          (then (return (i64.extend_i32_s (local.get $sector)))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))
  (i64.const 0))

(elem (table $mains) (i32.const 4) $day04.main)
(func $day04.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x04_8000)
      (i32.const 0x040) (i64.const 1514)
      (i32.const 0x041) (i64.const 0)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x04_8020)
      (i32.const 0x040) (i64.const 185371)
      (i32.const 0x041) (i64.const 984)))
  nop)
