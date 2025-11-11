(data (i32.const 0x09_8000) "day-09/input-ex0.txt")
(data (i32.const 0x09_8020) "day-09/input-ex1.txt")
(data (i32.const 0x09_8040) "day-09/input-real0.txt")

(elem (table $fns) (i32.const 0x090) $day09.part0 $day09.part1)

(func $day09.decompress (param $s i32) (param $end i32) (param $recur i32) (result i64)
  (local $ch i32)
  (local $ptr i32)
  (local $len i64)
  (local $reps i32)
  (local $subEnd i32)
  (local $result i64)

  (loop $count
    (if (i32.lt_u (local.get $s) (local.get $end))
      (then
        (local.set $ch (i32.load8_u (local.get $s)))
        (if (i32.eq (local.get $ch) (i32.const 0x28 (; '(' ;)))
          (then
            (local.set $s (i32.add (local.get $s) (i32.const 1)))
            (local.set $len (i64.extend_i32_s (call $parseI32 (local.get $s))))
            (local.set $s
              (i32.add
                (call $strchr (local.get $s) (i32.const 0x78 (; 'x' ;)))
                (i32.const 1)))
            (local.set $reps (call $parseI32 (local.get $s)))
            (local.set $s
              (i32.add
                (call $strchr (local.get $s) (i32.const 0x29 (; ')' ;)))
                (i32.const 1)))
            (local.set $subEnd (i32.add (local.get $s) (i32.wrap_i64 (local.get $len))))
            (if (local.get $recur)
              (then
                (local.set $len
                  (call $day09.decompress (local.get $s) (local.get $subEnd) (local.get $recur)))))
            (local.set $s (local.get $subEnd))
            (local.set $result
              (i64.add
                (local.get $result)
                (i64.mul
                  (local.get $len)
                  (i64.extend_i32_s (local.get $reps))))))
          (else
            (local.set $result (i64.add (local.get $result) (i64.const 1)))
            (local.set $s (i32.add (local.get $s) (i32.const 1)))))
        (br $count))))
  (local.get $result))

(func $day09.part0 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $result i64)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $result
          (i64.add
            (local.get $result)
            (call $day09.decompress
              (local.get $line)
              (i32.add (local.get $line) (call $strlen (local.get $line)))
              (i32.const 0))))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.get $result))

(func $day09.part1 (param $filename i32) (result i64)
  (local $lines i32)
  (local $line i32)
  (local $result i64)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $result
          (i64.add
            (local.get $result)
            (call $day09.decompress
              (local.get $line)
              (i32.add (local.get $line) (call $strlen (local.get $line)))
              (i32.const 1))))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.get $result))


(elem (table $mains) (i32.const 9) $day09.main)
(func $day09.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x09_8000)
      (i32.const 0x090) (i64.const 57)
      (i32.const 0x091) (i64.const 56)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x09_8020)
      (i32.const 0x090) (i64.const 589)
      (i32.const 0x091) (i64.const 242394)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x09_8040)
      (i32.const 0x090) (i64.const 112830)
      (i32.const 0x091) (i64.const 10931789799)))
  nop)
