(data (i32.const 0x17_8000) "day-23/input-ex0.txt")
(data (i32.const 0x17_8020) "day-23/input-real0.txt")

(data (i32.const 0x17_0000) "cpy")
(data (i32.const 0x17_0010) "inc")
(data (i32.const 0x17_0020) "dec")
(data (i32.const 0x17_0030) "jnz")
(data (i32.const 0x17_0040) "tgl")
(data (i32.const 0x17_0050) "fusedAdd")
(data (i32.const 0x17_0060) "fusedMul")

;; 0 cpy -> 3 jnz
;; 1 inc -> 2 dec
;; 2 dec -> 1 inc
;; 3 jnz -> 0 cpy
;; 4 tgl -> 1 inc
(global $day23.tglTable i32 (i32.const 0x17_1000))
(global $day23.x i32 (i32.const 0x17_1010))
(global $day23.y i32 (i32.const 0x17_1020))
(data (i32.const 0x17_1000) "\03\02\01\00\01")

(elem (table $fns) (i32.const 0x170) $day23.part0 $day23.part1)

(func $day23.readOperand (param $str i32) (result i32)
  (local $r i32)
  (if (i32.eqz (local.get $str))
    (then (return (i32.const 0))))
  (if
    (i32.le_u
      (local.tee $r
        (i32.sub
          (i32.load8_u (local.get $str))
          (i32.const 0x61)))
      (i32.const 4))
    (then (return (i32.add (local.get $r) (i32.const 0x7a)))))
  (call $parseI32 (local.get $str)))

(func $day23.printOperand (param $operand i32)
  (local $r i32)
  (call $printStr.small (i64.const 0x20))
  (if (i32.le_u (local.tee $r (i32.sub (local.get $operand) (i32.const 0x7a))) (i32.const 4))
    (then (call $printStr.small (i64.extend_i32_s (i32.add (i32.const 0x61) (local.get $r)))))
    (else (call $printI32.unpadded (local.get $operand)))))

(func $day23.decodeOperand (param $registers i32) (param $operandRef i32) (result i32)
  (local $operand i32)
  (local $r i32)
  (local.set $operand (i32.load (local.get $operandRef)))
  (if (i32.le_u (local.tee $r (i32.sub (local.get $operand) (i32.const 0x7a))) (i32.const 4))
    (then
      (return
        (i32.add
          (local.get $registers)
          (i32.shl (i32.sub (local.get $operand) (i32.const 0x7a)) (i32.const 2))))))
  (local.get $operandRef))

(func $day23.parse (param $line i32) (result i32)
  (local $words i32)
  (local $r i32)

  (local $op i32)
  (local $x i32)
  (local $y i32)

  (local.set $words (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))
  (local.set $x (call $day23.readOperand (i32.load (i32.add (local.get $words) (i32.const 4)))))
  (local.set $y (call $day23.readOperand (i32.load (i32.add (local.get $words) (i32.const 8)))))
  (block $result
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x17_0000)))
      (then
        (local.set $op (i32.const 0))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x17_0010)))
      (then
        (local.set $op (i32.const 1))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x17_0020)))
      (then
        (local.set $op (i32.const 2))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x17_0030)))
      (then
        (local.set $op (i32.const 3))
        (br $result)))
    (if (i32.eqz (call $strcmp (i32.load (local.get $words)) (i32.const 0x17_0040)))
      (then
        (local.set $op (i32.const 4))
        (br $result)))
    unreachable)

  (i32.or
    (local.get $op)
    (i32.or
      (i32.shl (i32.and (local.get $x) (i32.const 0xff)) (i32.const 8))
      (i32.shl (i32.and (local.get $y) (i32.const 0xff)) (i32.const 16)))))

(func $day23.dis (param $program i32) (param $n i32) (param $pc i32) (param $registers i32) (param $all i32)
  (local $i i32)
  (local $end i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)

  (if (local.get $all)
    (then
      (local.set $i (i32.const 0))
      (local.set $end (local.get $n)))
    (else
      (local.set $i (call $i32.max (i32.const 0) (i32.sub (local.get $pc) (i32.const 2))))
      (local.set $end (call $i32.min (i32.add (local.get $pc) (i32.const 3)) (local.get $n)))))

  (call $print.nl)
  (call $printI32 (i32.load (i32.add (local.get $registers) (i32.const 0))))
  (call $printI32 (i32.load (i32.add (local.get $registers) (i32.const 4))))
  (call $printI32 (i32.load (i32.add (local.get $registers) (i32.const 8))))
  (call $printI32 (i32.load (i32.add (local.get $registers) (i32.const 12))))
  (call $printStr.small (i64.const 0x3d63702020))
  (call $printI32.unpadded (local.get $pc))
  (call $print.nl)
  (loop $loop
    (if (i32.ge_s (local.get $i) (local.get $end))
      (then (return)))

    (local.set $op (i32.load (i32.add (local.get $program) (i32.shl (local.get $i) (i32.const 2)))))
    (local.set $x (i32.shr_s (i32.shl (local.get $op) (i32.const 16)) (i32.const 24)))
    (local.set $y (i32.shr_s (i32.shl (local.get $op) (i32.const 8)) (i32.const 24)))

    (if (i32.eq (local.get $i) (local.get $pc))
      (then (call $printStr.small (i64.const 0x203e2d)))
      (else (call $printStr.small (i64.const 0x202020))))
    (call $printI32.unpadded (local.get $i))
    (call $printStr.small (i64.const 0x20203a))

    (if (i32.eqz (local.get $op))
      (then (call $printStr.small (i64.const 0x706f6e))) ;; nop
      (else
        (local.set $op (i32.and (local.get $op) (i32.const 0xff)))
        (call $printStr (i32.add (i32.const 0x17_0000) (i32.shl (local.get $op) (i32.const 4))))
        (call $day23.printOperand (local.get $x))
        (if (i32.or
              (i32.eq (local.get $op) (i32.const 0))
              (i32.or
                (i32.eq (local.get $op) (i32.const 3))
                (i32.eq (local.get $op) (i32.const 5))))
          (then (call $day23.printOperand (local.get $y))))))
    (call $print.nl)
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br $loop)))

(func $day23.staticAnalysis (param $program i32) (param $n i32) (result i32)
  (local $newProg i32)
  (local $i i32)
  (local $j i32)
  (local $opPtr i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)
  (local $decReg i32)
  (local $incReg i32)

  (local.set $newProg (call $malloc (i32.shl (local.get $n) (i32.const 2))))

  (loop $loop.i
    (block $write.i
      (local.set $decReg (i32.const 0))
      (local.set $incReg (i32.const 0))

      (br_if $write.i (i32.ge_s (i32.add (local.get $i) (i32.const 2)) (local.get $n)))

      ;; (call $day23.dis (local.get $program) (local.get $n) (local.get $i) (i32.const 0) (i32.const 0))
      ;; (call $debugger)
      (local.set $j (i32.add (local.get $i) (i32.const 1)))
      (loop $loop.j
        (local.set $opPtr (i32.add (local.get $program) (i32.shl (local.get $j) (i32.const 2))))
        (local.set $op (i32.load8_u (local.get $opPtr)))
        (local.set $x (i32.load8_s (i32.add (local.get $opPtr) (i32.const 1))))
        (if (i32.eq (local.get $op) (i32.const 1)) ;; inc
          (then (local.set $incReg (local.get $x))))
        (if (i32.eq (local.get $op) (i32.const 2)) ;; dec
          (then (local.set $decReg (local.get $x))))
        (local.set $j (i32.sub (local.get $j) (i32.const 1)))
        (br_if $loop.j (i32.le_s (local.get $i) (local.get $j))))
      (br_if $write.i (i32.eqz (local.get $decReg)))
      (br_if $write.i (i32.eqz (local.get $incReg)))

      (local.set $opPtr
        (i32.add
          (local.get $program)
          (i32.shl (i32.add (local.get $i) (i32.const 2)) (i32.const 2))))
      (local.set $op (i32.load8_u (local.get $opPtr)))
      (local.set $x (i32.load8_s (i32.add (local.get $opPtr) (i32.const 1))))
      (local.set $y (i32.load8_s (i32.add (local.get $opPtr) (i32.const 2))))
      (br_if $write.i (i32.ne (local.get $op) (i32.const 3))) ;; jnz
      (br_if $write.i (i32.ne (local.get $x) (local.get $decReg)))
      (br_if $write.i (i32.ne (local.get $y) (i32.const -2)))

      ;; TODO: also needs to consume the copy before
      ;; Found addition, check for multiply
      ;; (block $fusedAdd
      ;;   (br_if $fusedAdd (i32.ge_s (i32.add (local.get $i) (i32.const 4)) (local.get $n)))
      ;;   (local.set $opPtr
      ;;     (i32.add
      ;;       (local.get $program)
      ;;       (i32.shl (i32.add (local.get $i) (i32.const 3)) (i32.const 2))))
      ;;   (local.set $op (i32.load8_u (local.get $opPtr)))
      ;;   (local.set $x (i32.load8_s (i32.add (local.get $opPtr) (i32.const 1))))
      ;;   (br_if $fusedAdd (i32.ne (local.get $op) (i32.const 2))) ;; dec
      ;;   (br_if $fusedAdd (i32.eq (local.get $x) (local.get $incReg)))
      ;;   (br_if $fusedAdd (i32.eq (local.get $x) (local.get $decReg)))
      ;;   (local.set $opPtr
      ;;     (i32.add
      ;;       (local.get $program)
      ;;       (i32.shl (i32.add (local.get $i) (i32.const 3)) (i32.const 2))))
      ;;   (local.set $op (i32.load8_u (local.get $opPtr)))
      ;;   (br_if $fusedAdd (i32.ne (local.get $x) (i32.load8_s (i32.add (local.get $opPtr) (i32.const 1)))))
      ;;   (br_if $fusedAdd (i32.ne (i32.const -5) (i32.load8_s (i32.add (local.get $opPtr) (i32.const 2)))))
      ;;   ;; it's a valid multiply
      ;;   (call $printStr (i32.const 0x17_0060))
      ;;   (call $printStr.small (i64.const 20))
      ;;   (call $printStr.nl))

      (i32.store
        (i32.add (local.get $newProg) (i32.shl (local.get $i) (i32.const 2)))
        (i32.or
          (i32.const 5)
          (i32.or
            (i32.shl (local.get $decReg) (i32.const 8))
            (i32.shl (local.get $incReg) (i32.const 16)))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $loop.i))
    (memory.copy
      (i32.add (local.get $newProg) (i32.shl (local.get $i) (i32.const 2)))
      (i32.add (local.get $program) (i32.shl (local.get $i) (i32.const 2)))
      (i32.const 4))
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $loop.i (i32.lt_s (local.get $i) (local.get $n))))

  (local.get $newProg))

(func $day23.eval (param $program i32) (param $deOpt i32) (param $n i32) (param $pc i32) (param $registers i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)
  (local $tmp i32)
  (local $opPtr i32)

  (if (i32.ge_u (local.get $pc) (local.get $n))
    (then (return)))

  (local.set $opPtr (i32.add (local.get $program) (i32.shl (local.get $pc) (i32.const 2))))
  (local.set $op (i32.load8_u (local.get $opPtr)))
  (local.set $x (i32.load8_s (i32.add (local.get $opPtr) (i32.const 1))))
  (local.set $y (i32.load8_s (i32.add (local.get $opPtr) (i32.const 2))))

  (i32.store (global.get $day23.x) (local.get $x))
  (i32.store (global.get $day23.y) (local.get $y))
  (local.set $x (call $day23.decodeOperand (local.get $registers) (global.get $day23.x)))
  (local.set $y (call $day23.decodeOperand (local.get $registers) (global.get $day23.y)))

  (block $next
    (block $badOp
      (block $fusedAdd
        (block $tgl
          (block $jnz
            (block $dec
              (block $inc
                (block $cpy
                  ;; (call $day23.dis (local.get $program) (local.get $n) (local.get $pc) (local.get $registers) (i32.const 0))
                  ;; (call $debugger)
                  (br_table $cpy $inc $dec $jnz $tgl $fusedAdd $badOp (local.get $op)))
                ;; cpy
                (i32.store (local.get $y) (i32.load (local.get $x)))
                (br $next))
              ;; inc
              (i32.store
                (local.get $x)
                (i32.add (i32.load (local.get $x)) (i32.const 1)))
              (br $next))
            ;; dec
            (i32.store
              (local.get $x)
              (i32.sub (i32.load (local.get $x)) (i32.const 1)))
            (br $next))
          ;; jnz
          (if (i32.load (local.get $x))
            (then (local.set $pc (i32.sub (i32.add (local.get $pc) (i32.load (local.get $y))) (i32.const 1)))))
          (br $next))
        ;; tgl
        (local.set $tmp (i32.add (local.get $pc) (i32.load (local.get $x))))
        (br_if $next (i32.ge_u (local.get $tmp) (local.get $n)))
        (i32.store8
          (local.tee $opPtr (i32.add (local.get $deOpt) (i32.shl (local.get $tmp) (i32.const 2))))
          (i32.load8_u
            (i32.add
              (global.get $day23.tglTable)
              (i32.load8_u (local.get $opPtr)))))
        (local.set $program (call $day23.staticAnalysis (local.get $deOpt) (local.get $n)))
        ;; (call $day23.dis (local.get $program) (local.get $n) (i32.const 0) (local.get $registers) (i32.const 1))
        (br $next))
      ;; fusedAdd
      (i32.store
        (local.get $y)
        (i32.add (i32.load (local.get $x)) (i32.load (local.get $y))))
      (i32.store (local.get $x) (i32.const 0))
      (local.set $pc (i32.add (local.get $pc) (i32.const 2)))
      (br $next))
    ;; badOp
    unreachable)
  ;; next
  (return_call $day23.eval (local.get $program) (local.get $deOpt) (local.get $n)
    (i32.add (local.get $pc) (i32.const 1))
    (local.get $registers)))

(func $day23.common (param $filename i32) (param $a0 i32) (result i64)
  (local $n i32)
  (local $lines i32)
  (local $line i32)
  (local $program i32)
  (local $optProgram i32)
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
  (i32.store (local.get $registers) (local.get $a0))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (i32.store (local.get $ptr) (call $day23.parse (local.get $line)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))

  (local.set $optProgram
    (call $day23.staticAnalysis (local.get $program) (local.get $n)))

  (call $day23.eval (local.get $optProgram) (local.get $program) (local.get $n) (i32.const 0) (local.get $registers))
  (i64.extend_i32_s (i32.load (local.get $registers))))

(func $day23.part0 (param $filename i32) (result i64)
  (call $day23.common (local.get $filename) (i32.const 7)))

(func $day23.part1 (param $filename i32) (result i64)
  (call $day23.common (local.get $filename) (i32.const 12)))

(elem (table $mains) (i32.const 23) $day23.main)
(func $day23.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x17_8000)
      (i32.const 0x170) (i64.const 3)
      (i32.const 0x171) (i64.const 3)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x17_8020)
      (i32.const 0x170) (i64.const 10440)
      (i32.const 0x171) (i64.const 479007000)))
  nop)
