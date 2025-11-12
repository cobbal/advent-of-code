(data (i32.const 0x10_8000) "day-16/input-ex0.txt")
(data (i32.const 0x10_8020) "day-16/input-real0.txt")

(data (i32.const 0x10_9000) "01100               ")
(data (i32.const 0x10_9020) "01100               ")
(data (i32.const 0x10_9040) "01110011101111011   ")
(data (i32.const 0x10_9060) "11001111011000111   ")

(elem (table $fns) (i32.const 0x100) $day16.part0.ex $day16.part0.real $day16.part1.ex $day16.part1.real)

(func $day16.expand (param $str i32) (result i32)
  (local $n i32)
  (local $result i32)
  (local $ptr i32)

  (local.set $n (call $strlen (local.get $str)))
  (local.set $result (call $malloc (i32.add (i32.mul (local.get $n) (i32.const 2)) (i32.const 2))))
  (memory.copy (local.get $result) (local.get $str) (local.get $n))
  (i32.store8 (i32.add (local.get $result) (local.get $n)) (i32.const 0x30))

  (local.set $str (local.get $result))
  (local.set $ptr (i32.add (local.get $result) (i32.mul (local.get $n) (i32.const 2))))
  (loop $loop
    (i32.store8 (local.get $ptr) (i32.xor (i32.const 1) (i32.load8_u (local.get $str))))
    (local.set $str (i32.add (local.get $str) (i32.const 1)))
    (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
    (br_if $loop (i32.lt_u (local.get $str) (local.get $ptr))))
  (local.get $result))

(func $day16.checksum (param $str i32) (result i32)
  (local $n i32)
  (local $result i32)
  (local $ptr i32)

  (local.set $n (call $strlen (local.get $str)))
  (local.set $result
    (local.tee $ptr
      (call $malloc (i32.add (i32.shr_u (local.get $n) (i32.const 1)) (i32.const 1)))))

  (loop $loop
    (i32.store8
      (local.get $ptr)
      (i32.add
        (i32.const 0x30)
        (i32.eq (i32.load8_u (local.get $str)) (i32.load8_u (i32.add (local.get $str) (i32.const 1))))))
    (local.set $str (i32.add (local.get $str) (i32.const 2)))
    (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    (local.set $n (i32.sub (local.get $n) (i32.const 2)))
    (br_if $loop (i32.gt_s (local.get $n) (i32.const 0))))

  (local.get $result))

(func $day16.common (param $filename i32) (param $diskLen i32) (result i32)
  (local $lines i32)
  (local $data i32)
  (local $result i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $data (i32.load (local.get $lines)))
  (loop $loop.expand
    (if (i32.lt_s (call $strlen (local.get $data)) (local.get $diskLen))
      (then
        (local.set $data (call $day16.expand (local.get $data)))
        (br $loop.expand))))
  (i32.store8 (i32.add (local.get $data) (local.get $diskLen)) (i32.const 0))

  (loop $loop.checksum
    (if (i32.eqz (i32.and (call $strlen (local.get $data)) (i32.const 1)))
      (then
        (local.set $data (call $day16.checksum (local.get $data)))
        (br $loop.checksum))))

  (local.set $result (call $malloc (i32.const 21)))
  (memory.fill (local.get $result) (i32.const 0x20) (i32.const 20))
  (memory.copy (local.get $result) (local.get $data) (call $strlen (local.get $data)))
  (local.get $result))

(func $day16.part0.ex (param $filename i32) (result i32)
  (call $day16.common (local.get $filename) (i32.const 20)))

(func $day16.part0.real (param $filename i32) (result i32)
  (call $day16.common (local.get $filename) (i32.const 272)))

(func $day16.part1.ex (param $filename i32) (result i32)
  (call $day16.common (local.get $filename) (i32.const 20)))

(func $day16.part1.real (param $filename i32) (result i32)
  (call $day16.common (local.get $filename) (i32.const 35651584)))

(elem (table $mains) (i32.const 16) $day16.main)
(func $day16.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x10_8000)
      (i32.const 0x100) (i32.const 0x10_9000)
      (i32.const 0x102) (i32.const 0x10_9020)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x10_8020)
      (i32.const 0x101) (i32.const 0x10_9040)
      (i32.const 0x103) (i32.const 0x10_9060)))
  nop)
