(data (i32.const 0x0c_8000) "day-12/input-ex0.txt")
(data (i32.const 0x0c_8020) "day-12/input-real0.txt")

(data (i32.const 0x0c_0000) "cpy")
(data (i32.const 0x0c_0010) "inc")
(data (i32.const 0x0c_0020) "dec")
(data (i32.const 0x0c_0030) "jnz")
(data (i32.const 0x0c_0040) "set")
(data (i32.const 0x0c_0050) "jmp")

(elem (table $fns) (i32.const 0x0c0) $day12.part0 $day12.part1)

(func $day12.readReg (param $str i32) (result i32)
  (local $r i32)
  (if
    (i32.le_u
      (local.tee $r
        (i32.sub
          (i32.load8_u (local.get $str))
          (i32.const 0x61)))
      (i32.const 4))
    (then (return (local.get $r))))
  (i32.const -1))

(func $day12.parse (param $line i32) (result i32)
  (local $words i32)
  (local $r i32)

  (local $op i32)
  (local $x i32)
  (local $y i32)

  (local.set $words (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))
  (local.set $x (i32.load (i32.add (local.get $words) (i32.const 4))))
  (block $result
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x0c_0000)))
      (then
        (local.set $y (i32.load (i32.add (local.get $words) (i32.const 8))))
        (local.set $y (call $day12.readReg (local.get $y)))
        (if (i32.ge_s (local.tee $r (call $day12.readReg (local.get $x))) (i32.const 0))
          (then
            ;; register copy
            (local.set $op (i32.const 0))
            (local.set $x (local.get $r)))
          (else
            ;; value copy
            (local.set $op (i32.const 4))
            (local.set $x (call $parseI32 (local.get $x)))))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x0c_0010)))
      (then
        (local.set $op (i32.const 1))
        (local.set $x (call $day12.readReg (local.get $x)))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x0c_0020)))
      (then
        (local.set $op (i32.const 2))
        (local.set $x (call $day12.readReg (local.get $x)))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x0c_0030)))
      (then
        (local.set $y (i32.load (i32.add (local.get $words) (i32.const 8))))
        (local.set $y (call $parseI32 (local.get $y)))
        (if (i32.ge_s (local.tee $r (call $day12.readReg (local.get $x))) (i32.const 0))
          (then
            ;; register jump
            (local.set $op (i32.const 3))
            (local.set $x (local.get $r)))
          (else
            ;; value jump
            (local.set $op (i32.const 5))
            (local.set $x (call $parseI32 (local.get $x)))))
        (br $result)))
    unreachable)

  (i32.or
    (local.get $op)
    (i32.or
      (i32.shl (i32.and (local.get $x) (i32.const 0xff)) (i32.const 8))
      (i32.shl (i32.and (local.get $y) (i32.const 0xff)) (i32.const 16)))))

(func $day12.eval (param $program i32) (param $n i32) (param $pc i32) (param $registers i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)
  (local $tmp i32)

  (if (i32.ge_u (local.get $pc) (local.get $n))
    (then (return)))

  (local.set $op (i32.load (i32.add (local.get $program) (i32.shl (local.get $pc) (i32.const 2)))))
  (local.set $x (i32.shr_s (i32.shl (local.get $op) (i32.const 16)) (i32.const 24)))
  (local.set $y (i32.shr_s (i32.shl (local.get $op) (i32.const 8)) (i32.const 24)))
  (local.set $op (i32.and (local.get $op) (i32.const 0xff)))

  (block $next
    (block $jmp
      (block $set
        (block $jnz
          (block $dec
            (block $inc
              (block $cpy
                (br_table $cpy $inc $dec $jnz $set $jmp (local.get $op)))
              ;; register copy
              (i32.store
                (i32.add (local.get $registers) (i32.shl (local.get $y) (i32.const 2)))
                (i32.load (i32.add (local.get $registers) (i32.shl (local.get $x) (i32.const 2)))))
              (br $next))
            ;; inc
            (i32.store
              (local.tee $tmp (i32.add (local.get $registers) (i32.shl (local.get $x) (i32.const 2))))
              (i32.add (i32.load (local.get $tmp)) (i32.const 1)))
            (br $next))
          ;; dec
          (i32.store
            (local.tee $tmp (i32.add (local.get $registers) (i32.shl (local.get $x) (i32.const 2))))
            (i32.sub (i32.load (local.get $tmp)) (i32.const 1)))
          (br $next))
        ;; jnz
        (if (i32.load (i32.add (local.get $registers) (i32.shl (local.get $x) (i32.const 2))))
          (then (local.set $pc (i32.sub (i32.add (local.get $pc) (local.get $y)) (i32.const 1)))))
        (br $next))
      ;; set
      (i32.store
        (i32.add (local.get $registers) (i32.shl (local.get $y) (i32.const 2)))
        (local.get $x))
      (br $next))
    ;; jmp
    (if (local.get $x)
      (then (local.set $pc (i32.sub (i32.add (local.get $pc) (local.get $y)) (i32.const 1)))))
    (br $next))
  ;; next
  (return_call $day12.eval (local.get $program) (local.get $n)
    (i32.add (local.get $pc) (i32.const 1))
    (local.get $registers)))

(func $day12.part0 (param $filename i32) (result i64)
  (local $n i32)
  (local $lines i32)
  (local $line i32)
  (local $program i32)
  (local $ptr i32)
  (local $registers i32)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $ptr
    (local.tee $program
      (call $malloc (i32.mul (local.get $n) (i32.const 4)))))
  (local.set $registers (call $malloc (i32.const 16)))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (i32.store (local.get $ptr) (call $day12.parse (local.get $line)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (call $day12.eval (local.get $program) (local.get $n) (i32.const 0) (local.get $registers))
  (i64.extend_i32_s (i32.load (local.get $registers))))

(func $day12.part1 (param $filename i32) (result i64)
  (local $n i32)
  (local $lines i32)
  (local $line i32)
  (local $program i32)
  (local $ptr i32)
  (local $registers i32)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $ptr
    (local.tee $program
      (call $malloc (i32.mul (local.get $n) (i32.const 4)))))
  (local.set $registers (call $malloc (i32.const 16)))
  (i32.store
    (i32.add (local.get $registers) (i32.const 8))
    (i32.const 1))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (i32.store (local.get $ptr) (call $day12.parse (local.get $line)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (call $day12.eval (local.get $program) (local.get $n) (i32.const 0) (local.get $registers))
  (i64.extend_i32_s (i32.load (local.get $registers))))

(elem (table $mains) (i32.const 12) $day12.main)
(func $day12.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0c_8000)
      (i32.const 0x0c0) (i64.const 42)
      (i32.const 0x0c1) (i64.const 42)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0c_8020)
      (i32.const 0x0c0) (i64.const 318083)
      (i32.const 0x0c1) (i64.const 9227737)))
  nop)
