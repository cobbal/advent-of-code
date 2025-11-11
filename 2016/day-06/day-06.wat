(data (i32.const 0x06_8000) "day-06/input-ex0.txt")
(data (i32.const 0x06_8020) "easter              ")
(data (i32.const 0x06_8040) "advent              ")
(data (i32.const 0x06_8100) "day-06/input-real0.txt")
(data (i32.const 0x06_8120) "dzqckwsd            ")
(data (i32.const 0x06_8140) "lragovly            ")

(elem (table $fns) (i32.const 0x060) $day06.part0 $day06.part1)

(func $day06.part0 (param $filename i32) (result i32)
  (local $lines i32)
  (local $line i32)
  (local $histogram i32)
  (local $n i32)
  (local $rowPtr i32)
  (local $col i32)
  (local $p i32)
  (local $bestCount i32)
  (local $ch i32)
  (local $result i32)
  (local $count i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $n (call $strlen (i32.load (local.get $lines))))
  (local.set $histogram (call $malloc (i32.const 0x400)))
  (local.set $result (call $malloc (i32.const 21)))
  (memory.fill (local.get $result) (i32.const 0x20) (i32.const 20))

  (loop $colLoop
    (if (i32.lt_u (local.get $col) (local.get $n))
      (then
        (memory.fill (local.get $histogram) (i32.const 0) (i32.const 0x400))
        (local.set $rowPtr (local.get $lines))
        (loop $rowLoop
          (if (local.tee $line (i32.load (local.get $rowPtr)))
            (then
              (local.set $p
                (i32.add
                  (local.get $histogram)
                  (i32.shl
                    (i32.load8_u (i32.add (local.get $line) (local.get $col)))
                    (i32.const 2))))
              (i32.store (local.get $p) (i32.add (i32.load (local.get $p)) (i32.const 1)))
              (local.set $rowPtr (i32.add (local.get $rowPtr) (i32.const 4)))
              (br $rowLoop))))

        (local.set $bestCount (i32.const -1))
        (local.set $ch (i32.const 0x61))
        (loop $bestLoop
          (if (i32.lt_u (local.get $ch) (i32.const 0x7b))
            (then
              (local.set $count
                (i32.load
                  (i32.add
                    (local.get $histogram)
                    (i32.shl (local.get $ch) (i32.const 2)))))
              (if (i32.gt_s (local.get $count) (local.get $bestCount))
                (then
                  (local.set $bestCount (local.get $count))
                  (i32.store8 (i32.add (local.get $result) (local.get $col)) (local.get $ch))))
              (local.set $ch (i32.add (local.get $ch) (i32.const 1)))
              (br $bestLoop))))

        (local.set $col (i32.add (local.get $col) (i32.const 1)))
        (br $colLoop))))
  (local.get $result))

(func $day06.part1 (param $filename i32) (result i32)
  (local $lines i32)
  (local $line i32)
  (local $histogram i32)
  (local $n i32)
  (local $rowPtr i32)
  (local $col i32)
  (local $p i32)
  (local $bestCount i32)
  (local $ch i32)
  (local $result i32)
  (local $count i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $n (call $strlen (i32.load (local.get $lines))))
  (local.set $histogram (call $malloc (i32.const 0x400)))
  (local.set $result (call $malloc (i32.const 21)))
  (memory.fill (local.get $result) (i32.const 0x20) (i32.const 20))

  (loop $colLoop
    (if (i32.lt_u (local.get $col) (local.get $n))
      (then
        (memory.fill (local.get $histogram) (i32.const 0) (i32.const 0x400))
        (local.set $rowPtr (local.get $lines))
        (loop $rowLoop
          (if (local.tee $line (i32.load (local.get $rowPtr)))
            (then
              (local.set $p
                (i32.add
                  (local.get $histogram)
                  (i32.shl
                    (i32.load8_u (i32.add (local.get $line) (local.get $col)))
                    (i32.const 2))))
              (i32.store (local.get $p) (i32.add (i32.load (local.get $p)) (i32.const 1)))
              (local.set $rowPtr (i32.add (local.get $rowPtr) (i32.const 4)))
              (br $rowLoop))))

        (local.set $bestCount (i32.const -1))
        (local.set $ch (i32.const 0x61))
        (loop $bestLoop
          (if (i32.lt_u (local.get $ch) (i32.const 0x7b))
            (then
              (local.set $count
                (i32.load
                  (i32.add
                    (local.get $histogram)
                    (i32.shl (local.get $ch) (i32.const 2)))))
              (if (local.get $count)
                (then
                  (if (i32.lt_u (local.get $count) (local.get $bestCount))
                    (then
                      (local.set $bestCount (local.get $count))
                      (i32.store8 (i32.add (local.get $result) (local.get $col)) (local.get $ch))))))
              (local.set $ch (i32.add (local.get $ch) (i32.const 1)))
              (br $bestLoop))))

        (local.set $col (i32.add (local.get $col) (i32.const 1)))
        (br $colLoop))))
  (local.get $result))

(elem (table $mains) (i32.const 6) $day06.main)
(func $day06.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x06_8000)
      (i32.const 0x060) (i32.const 0x06_8020)
      (i32.const 0x061) (i32.const 0x06_8040)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x06_8100)
      (i32.const 0x060) (i32.const 0x06_8120)
      (i32.const 0x061) (i32.const 0x06_8140)))
  nop)
