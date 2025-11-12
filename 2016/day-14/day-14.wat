(data (i32.const 0x0e_8000) "day-14/input-ex0.txt")
(data (i32.const 0x0e_8020) "day-14/input-real0.txt")

(data (i32.const 0x0e_0000) "abc89")

(elem (table $fns) (i32.const 0x0e0) $day14.part0 $day14.part1 $md5.digest $day14.stretchMD5)

;; struct match {
;;     3found: i8
;;     3nib: i8
;;     5set: i16
;; }

(func $day14.computeEntry
  (param $md5Context i32) (param $hash i32) (param $keyStart i32) (param $keyEnd i32) (param $i i32)
  (result i32)

  (local $n i32)
  (local $a i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)
  (local $ptr i32)
  (local $byte i32)
  (local $nibble i32)
  (local $run i32)
  (local $3found i32)
  (local $3nib i32)
  (local $5set i32)

  (local.set $keyEnd (i32.add (local.get $keyEnd) (call $u32.digitCount (local.get $i))))
  (local.set $n (i32.sub (local.get $keyEnd) (local.get $keyStart)))

  (i32.store8 (local.get $keyEnd) (i32.const 0))
  (loop $loop.format
    (local.set $keyEnd (i32.sub (local.get $keyEnd) (i32.const 1)))
    (i32.store8
      (local.get $keyEnd)
      (i32.add (i32.const 0x30) (i32.rem_u (local.get $i) (i32.const 10))))
    (local.set $i (i32.div_u (local.get $i) (i32.const 10)))
    (br_if $loop.format (local.get $i)))

  (local.set $a
    (local.set $b
      (local.set $c
        (local.set $d
          (call_indirect $fns (param i32 i32 i32) (result i32 i32 i32 i32)
            (local.get $md5Context) (local.get $keyStart) (local.get $n)
            (local.get $hash))))))
  (i32.store (i32.const 0x0e_1000) (local.get $a))
  (i32.store (i32.const 0x0e_1004) (local.get $b))
  (i32.store (i32.const 0x0e_1008) (local.get $c))
  (i32.store (i32.const 0x0e_100c) (local.get $d))

  (local.set $ptr (i32.const 0x0e_1000))
  (loop $loop.search
    (local.set $byte (i32.load8_u (local.get $ptr)))
    (local.set $i (i32.const 2))
    (loop $loop.nibble
      (if (i32.eq
            (local.get $nibble)
            (local.tee $nibble (i32.and (i32.shr_u (local.get $byte) (i32.const 4)) (i32.const 0xf))))
        (then (local.set $run (i32.add (local.get $run) (i32.const 1))))
        (else (local.set $run (i32.const 1))))
      (if (i32.and (i32.eqz (local.get $3found)) (i32.eq (local.get $run) (i32.const 3)))
        (then
          (local.set $3found (i32.const 1))
          (local.set $3nib (local.get $nibble))))
      (if (i32.eq (local.get $run) (i32.const 5))
        (then
          (local.set $5set
            (i32.or
              (local.get $5set)
              (i32.shl (i32.const 1) (local.get $nibble))))))
      (local.set $byte (i32.shl (local.get $byte) (i32.const 4)))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (br_if $loop.nibble (local.get $i)))
    (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    (br_if $loop.search (i32.lt_u (local.get $ptr) (i32.const 0x0e_1010))))

  (i32.or
    (local.get $3found)
    (i32.or
      (i32.shl (local.get $3nib) (i32.const 8))
      (i32.shl (local.get $5set) (i32.const 16)))))

(func $day14.fill1024
  (param $md5Context i32) (param $hash i32) (param $keyStart i32) (param $keyEnd i32) (param $i i32) (param $ptr i32)

  (local $j i32)

  (loop $loop
    (i32.store
      (local.get $ptr)
      (call $day14.computeEntry
        (local.get $md5Context)
        (local.get $hash)
        (local.get $keyStart)
        (local.get $keyEnd)
        (i32.add (local.get $i) (local.get $j))))
    (local.set $j (i32.add (local.get $j) (i32.const 1)))
    (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
    (br_if $loop (i32.lt_u (local.get $j) (i32.const 1024)))))

(func $day14.common (param $filename i32) (param $hash i32) (result i64)
  (local $context i32)
  (local $i i32)
  (local $j i32)
  (local $k i32)
  (local $buf i32)
  (local $buf2 i32)
  (local $key i32)
  (local $keyEnd i32)
  (local $nibble i32)
  (local $foundKeys i32)

  (local.set $key
    (i32.load
      (drop
        (call $splitDestructively
          (drop (call $readFile (local.get $filename)))
          (i32.const 0x0a)
          (i32.const 0)))))
  (local.set $keyEnd (i32.add (local.get $key) (call $strlen (local.get $key))))
  (local.set $context (call $md5.createContext))
  (local.set $buf2
    (i32.add
      (local.tee $buf (call $malloc (i32.const 8192)))
      (i32.const 4096)))

  (call $day14.fill1024
    (local.get $context)
    (local.get $hash)
    (local.get $key)
    (local.get $keyEnd)
    (i32.const 0)
    (local.get $buf2))

  (loop $loop.i
    (memory.copy (local.get $buf) (local.get $buf2) (i32.const 4096))
    (call $day14.fill1024
      (local.get $context)
      (local.get $hash)
      (local.get $key)
      (local.get $keyEnd)
      (i32.add (local.get $i) (i32.const 1024))
      (local.get $buf2))

    (local.set $j (i32.const 0))
    (loop $loop.j

      (if (local.tee $nibble (i32.load16_u (i32.add (local.get $buf) (i32.shl (local.get $j) (i32.const 2)))))
        (then
          (local.set $nibble (i32.shl (i32.const 1) (i32.shr_u (local.get $nibble) (i32.const 8))))
          (local.set $k (i32.const 1))
          (loop $loop.k
            (if (i32.and (local.get $nibble)
                  (i32.load16_u
                    (i32.add
                      (local.get $buf)
                      (i32.add
                        (i32.shl (i32.add (local.get $j) (local.get $k)) (i32.const 2))
                        (i32.const 2)))))
              (then
                (local.set $foundKeys (i32.add (local.get $foundKeys) (i32.const 1)))
                (if (i32.eq (local.get $foundKeys) (i32.const 64))
                  (then (return (i64.extend_i32_s (i32.add (local.get $i) (local.get $j))))))))
            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br_if $loop.k (i32.lt_u (local.get $k) (i32.const 1001))))))
      (local.set $j (i32.add (local.get $j) (i32.const 1)))
      (br_if $loop.j (i32.lt_u (local.get $j) (i32.const 1024))))

    (local.set $i (i32.add (local.get $i) (i32.const 1024)))
    (br $loop.i))
  unreachable)

(func $day14.stretchMD5 (param $context i32) (param $str i32) (param $len i32) (result i32 i32 i32 i32)
  (local $i i32)

  (call $md5.hexString.into
    (call $md5.digest (local.get $context) (local.get $str) (local.get $len))
    (i32.const 0x0e_2000))

  (local.set $i (i32.const 2015))
  (loop $loop
    (call $md5.hexString.into
      (call $md5.digest (local.get $context) (i32.const 0x0e_2000) (i32.const 32))
      (i32.const 0x0e_2000))
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (br_if $loop (local.get $i)))

  (call $md5.digest (local.get $context) (i32.const 0x0e_2000) (i32.const 32)))

(func $day14.part0 (param $filename i32) (result i64)
  (call $day14.common (local.get $filename) (i32.const 0x0e2)))

(func $day14.part1 (param $filename i32) (result i64)
  (call $day14.common (local.get $filename) (i32.const 0x0e3)))

(elem (table $mains) (i32.const 14) $day14.main)
(func $day14.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0e_8000)
      (i32.const 0x0e0) (i64.const 22728)
      (i32.const 0x0e1) (i64.const 22551)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0e_8020)
      (i32.const 0x0e0) (i64.const 23890)
      (i32.const 0x0e1) (i64.const 22696)))
  nop)
