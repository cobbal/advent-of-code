(data (i32.const 0x05_8000) "day-05/input-ex0.txt")
(data (i32.const 0x05_8040) "day-05/input-real0.txt")

(data (i32.const 0x05_9000) "18f47a30            ")
(data (i32.const 0x05_9020) "05ace8e3            ")
(data (i32.const 0x05_9040) "1a3099aa            ")
(data (i32.const 0x05_9060) "694190cd            ")

(data (i32.const 0x05_a000) "  \00")

(elem (table $fns) (i32.const 0x050) $day05.part0 $day05.part1 $day05.part0.dispatch $day05.part1.dispatch)

(func $day05.common (param $filename i32) (param $dispatch i32) (result i32)
  (local $context i32)
  (local $onesPtr i32)
  (local $i i32)
  (local $magCap i32)
  (local $line i32)
  (local $ptr i32)
  (local $remainingDigits i32)
  (local $hash i32)
  (local $foundDigits i32)

  (memory.fill (i32.const 0x05_1000) (i32.const 0x20) (i32.const 20))
  (memory.fill (i32.const 0x05_1000) (i32.const 0x5f) (i32.const 8))

  (local.set $line (drop (call $readFile (local.get $filename))))
  (i32.store8 (call $strrchr (local.get $line) (i32.const 0x0a)) (i32.const 0))
  (memory.copy
    (i32.const 0x05_0000)
    (local.get $line)
    (i32.add (call $strlen (local.get $line)) (i32.const 1)))

  (local.set $context (call $md5.createContext))

  (local.set $onesPtr (call $strrchr (i32.const 0x05_0000) (i32.const 0)))
  (local.set $magCap (i32.const 10))
  (loop $magnitude
    (i32.store8 (i32.add (local.get $onesPtr) (i32.const 1)) (i32.const 0))
    (loop $iLoop
      (if (i32.ge_u (local.get $i) (local.get $magCap))
        (then
          (local.set $onesPtr (i32.add (local.get $onesPtr) (i32.const 1)))
          (local.set $magCap (i32.mul (local.get $magCap) (i32.const 10)))
          (br $magnitude)))

      (local.set $ptr (local.get $onesPtr))
      (local.set $remainingDigits (local.get $i))
      (loop $writeDigits
        (if (local.get $remainingDigits)
          (then
            (i32.store8 (local.get $ptr)
              (i32.add (i32.const 0x30)
                (i32.rem_u (local.get $remainingDigits) (i32.const 10))))
            (local.set $remainingDigits (i32.div_u (local.get $remainingDigits) (i32.const 10)))
            (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
            (br $writeDigits))))

      (local.set $hash
        (drop
          (drop
            (drop
              (call $md5.digest
                (local.get $context)
                (i32.const 0x05_0000)
                (i32.add
                  (i32.sub (local.get $onesPtr) (i32.const 0x05_0000))
                  (i32.const 1)))))))

      (if (i32.eqz (i32.and (local.get $hash) (i32.const 0x00f0_ffff)))
        (then
          (if
            (call_indirect $fns
              (param i32 i32) (result i32)
              (local.get $i) (local.get $hash)
              (local.get $dispatch))
            (then (return (i32.const 0x05_1000))))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $iLoop)))
  unreachable)

(func $day05.part0.dispatch (param $i i32) (param $hash i32) (result i32)
  (local $blank i32)
  (if (local.tee $blank (call $strchr (i32.const 0x05_1000) (i32.const 0x5f)))
    (then
      (i32.store8
        (local.get $blank)
        (call $asciiHex (i32.and (i32.shr_u (local.get $hash) (i32.const 16)) (i32.const 0xf))))))
  (i32.eqz (local.get $blank)))

(func $day05.part0 (param $filename i32) (result i32)
  (call $day05.common (local.get $filename) (i32.const 0x052)))

(func $day05.part1.dispatch (param $i i32) (param $hash i32) (result i32)
  (local $ptr i32)
  (local $higit5 i32)
  (local $higit6 i32)
  (local.set $higit5 (i32.and (i32.shr_u (local.get $hash) (i32.const 16)) (i32.const 0xf)))
  (local.set $higit6 (i32.and (i32.shr_u (local.get $hash) (i32.const 28)) (i32.const 0xf)))
  (if (i32.ge_u (local.get $higit5) (i32.const 8))
    (then (return (i32.const 0))))
  (local.set $ptr (i32.add (i32.const 0x05_1000) (local.get $higit5)))
  (if (i32.ne (i32.load8_u (local.get $ptr)) (i32.const 0x5f))
    (then (return (i32.const 0))))
  (i32.store8 (local.get $ptr) (call $asciiHex (local.get $higit6)))
  ;; (call $printStr.nl (i32.const 0x05_1000))
  (i32.eqz (call $strchr (i32.const 0x05_1000) (i32.const 0x5f))))

(func $day05.part1 (param $filename i32) (result i32)
  (call $day05.common (local.get $filename) (i32.const 0x053)))

(elem (table $mains) (i32.const 5) $day05.main)
(func $day05.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x05_8000)
      (i32.const 0x050) (i32.const 0x05_9000)
      (i32.const 0x051) (i32.const 0x05_9020)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x05_8040)
      (i32.const 0x050) (i32.const 0x05_9040)
      (i32.const 0x051) (i32.const 0x05_9060)))
  nop)
