(data (i32.const 0x15_8000) "day-21/input-ex0.txt")
(data (i32.const 0x15_8020) "day-21/input-real0.txt")

(data (i32.const 0x15_9000) "   decab")
(data (i32.const 0x15_9010) "   abcde")
(data (i32.const 0x15_9020) "dbfgaehc")
(data (i32.const 0x15_9030) "aghfcdeb")

(global $day21.swap-position i32 (i32.const 0x15_0000))
(data (i32.const 0x15_0000) "swap-position")
(global $day21.swap-letter i32 (i32.const 0x15_0020))
(data (i32.const 0x15_0020) "swap-letter")
(global $day21.rotate-left i32 (i32.const 0x15_0040))
(data (i32.const 0x15_0040) "rotate-left")
(global $day21.rotate-right i32 (i32.const 0x15_0060))
(data (i32.const 0x15_0060) "rotate-right")
(global $day21.rotate-based i32 (i32.const 0x15_0080))
(data (i32.const 0x15_0080) "rotate-based")
(global $day21.reverse-positions i32 (i32.const 0x15_00a0))
(data (i32.const 0x15_00a0) "reverse-positions")
(global $day21.move-position i32 (i32.const 0x15_00c0))
(data (i32.const 0x15_00c0) "move-position")

(global $day21.part1.init.ex i32 (i32.const 0x15_00e0))
(data (i32.const 0x15_00e0) "decab")
(global $day21.part1.init.real i32 (i32.const 0x15_00f0))
(data (i32.const 0x15_00f0) "fbgdceah")

(global $day21.str i32 (i32.const 0x15_1000))

(elem (table $fns) (i32.const 0x150) $day21.part0.ex $day21.part0.real $day21.part1.ex $day21.part1.real)

(func $day21.parse (param $filename i32) (result i32 i32)
  (local $lines i32)
  (local $n i32)
  (local $instructions i32)
  (local $ptr i32)
  (local $words i32)
  (local $str i32)
  (local $word0 i32)
  (local $word1 i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))
  (local.set $instructions
    (local.tee $ptr
      (call $malloc (i32.shl (i32.add (local.get $n) (i32.const 1)) (i32.const 2)))))
  (loop $loop.lines
    (if (local.tee $str (i32.load (local.get $lines)))
      (then
        (i32.store8
          (call $strchr (local.get $str) (i32.const 0x20))
          (i32.const 0x2d (;'-';)))

        (local.set $words
          (drop (call $splitDestructively (local.get $str) (i32.const 0x20) (i32.const 0))))
        (local.set $word0 (i32.load (local.get $words)))
        (local.set $word1 (i32.load (i32.add (local.get $words) (i32.const 4))))
        (local.set $y (i32.const 0xff))

        (block $break
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.swap-position)))
            (then
              ;; swap-position _ with position _
              (local.set $op (i32.const 0))
              (local.set $x (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $y (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 16)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.swap-letter)))
            (then
              ;; swap-letter _ with letter _
              (local.set $op (i32.const 1))
              (local.set $x (i32.load8_u (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $y (i32.load8_u (i32.load (i32.add (local.get $words) (i32.const 16)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.rotate-left)))
            (then
              ;; rotate-left _ steps
              (local.set $op (i32.const 2))
              (local.set $x (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.rotate-right)))
            (then
              ;; rotate-right _ steps
              (local.set $op (i32.const 3))
              (local.set $x (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.rotate-based)))
            (then
              ;; rotate-based on position of letter _
              (local.set $op (i32.const 4))
              (local.set $x (i32.load8_u (i32.load (i32.add (local.get $words) (i32.const 20)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.reverse-positions)))
            (then
              ;; reverse-positions _ through _
              (local.set $op (i32.const 5))
              (local.set $x (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $y (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 12)))))
              (br $break)))
          (if (i32.eqz (call $strcmp (local.get $word0) (global.get $day21.move-position)))
            (then
              ;; move-position _ to position _
              (local.set $op (i32.const 6))
              (local.set $x (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $y (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 16)))))
              (br $break)))
          unreachable)

        (i32.store
          (local.get $ptr)
          (i32.or
            (i32.shl (local.get $op) (i32.const 0))
            (i32.or
              (i32.shl (local.get $x) (i32.const 8))
              (i32.shl (local.get $y) (i32.const 16)))))

        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $loop.lines))))
  (local.get $instructions) (local.get $n))

(func $day21.doOp (param $n i32) (param $op i32) (param $x i32) (param $y i32)
  ;; (call $printStr
  ;;   (i32.add
  ;;     (global.get $day21.swap-position)
  ;;     (i32.mul (local.get $op) (i32.const 0x20))))
  ;; (call $printI32 (local.get $x))
  ;; (call $printI32.nl (local.get $y))

  ;; swap-position
  (if (i32.eq (local.get $op) (i32.const 0))
    (then
      (call $i8.swap
        (i32.add (global.get $day21.str) (local.get $x))
        (i32.add (global.get $day21.str) (local.get $y)))
      (return)))

  ;; swap-letter
  (if (i32.eq (local.get $op) (i32.const 1))
    (then
      (call $i8.swap
        (call $strchr (global.get $day21.str) (local.get $x))
        (call $strchr (global.get $day21.str) (local.get $y)))
      (return)))

  ;; rotate-left
  (if (i32.eq (local.get $op) (i32.const 2))
    (then
      (call $memrotate.left
        (global.get $day21.str)
        (i32.add
          (global.get $day21.str)
          (local.get $x))
        (i32.add (global.get $day21.str) (local.get $n)))
      (return)))

  ;; rotate-right
  (if (i32.eq (local.get $op) (i32.const 3))
    (then
      (call $memrotate.left
        (global.get $day21.str)
        (i32.add
          (global.get $day21.str)
          (i32.sub (local.get $n) (local.get $x)))
        (i32.add (global.get $day21.str) (local.get $n)))
      (return)))

  ;; rotate-based
  (if (i32.eq (local.get $op) (i32.const 4))
    (then
      (local.set $x
        (i32.sub
          (call $strchr (global.get $day21.str) (local.get $x))
          (global.get $day21.str)))
      (local.set $x
        (i32.rem_u
          (i32.add
            (i32.const 1)
            (i32.add
              (local.get $x)
              (i32.ge_u (local.get $x) (i32.const 4))))
          (local.get $n)))
      (call $memrotate.left
        (global.get $day21.str)
        (i32.add
          (global.get $day21.str)
          (i32.sub (local.get $n) (local.get $x)))
        (i32.add (global.get $day21.str) (local.get $n)))
      (return)))

  ;; reverse-positions
  (if (i32.eq (local.get $op) (i32.const 5))
    (then
      (loop $rev
        (if (i32.lt_u (local.get $x) (local.get $y))
          (then
            (call $i8.swap
              (i32.add (global.get $day21.str) (local.get $x))
              (i32.add (global.get $day21.str) (local.get $y)))
            (local.set $x (i32.add (local.get $x) (i32.const 1)))
            (local.set $y (i32.sub (local.get $y) (i32.const 1)))
            (br $rev))))
      (return)))

  ;; move-position
  (if (i32.eq (local.get $op) (i32.const 6))
    (then
      (if (i32.lt_s (local.get $x) (local.get $y))
        (then
          (call $memrotate.left
            (i32.add (global.get $day21.str) (local.get $x))
            (i32.add (global.get $day21.str) (i32.add (local.get $x) (i32.const 1)))
            (i32.add (global.get $day21.str) (i32.add (local.get $y) (i32.const 1)))))
        (else
          (call $memrotate.left
            (i32.add (global.get $day21.str) (local.get $y))
            (i32.add (global.get $day21.str) (local.get $x))
            (i32.add (global.get $day21.str) (i32.add (local.get $x) (i32.const 1))))))
      (return)))
  unreachable)

;; 0 -> 1 =>  1: 1/1
;; 1 -> 2 =>  3: 3/3
;; 2 -> 3 =>  5: 0/5
;; 3 -> 4 =>  7: 2/7
;; 4 -> 6 => 10: 0/2
;; 5 -> 7 => 12: -/4
;; 6 -> 8 => 14: -/6
;; 7 -> 9 => 16: -/0
(func $day21.unrot.check
  (call $assert (i32.eq (call $day21.unrot (i32.const 1) (i32.const 5)) (i32.const 1)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 3) (i32.const 5)) (i32.const 2)))
  ;; (call $assert (i32.eq (call $day21.unrot (i32.const 0) (i32.const 5)) (i32.const 3)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 2) (i32.const 5)) (i32.const 4)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 0) (i32.const 5)) (i32.const 1)))

  (call $assert (i32.eq (call $day21.unrot (i32.const 1) (i32.const 8)) (i32.const 1)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 3) (i32.const 8)) (i32.const 2)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 5) (i32.const 8)) (i32.const 3)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 7) (i32.const 8)) (i32.const 4)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 2) (i32.const 8)) (i32.const 6)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 4) (i32.const 8)) (i32.const 7)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 6) (i32.const 8)) (i32.const 0)))
  (call $assert (i32.eq (call $day21.unrot (i32.const 0) (i32.const 8)) (i32.const 1)))
  nop)

(func $day21.unrot (param $x i32) (param $n i32) (result i32)
  (local $result i32)
  (if (i32.eqz (local.get $x))
    (then (local.set $result (i32.const 1)))
    (else
      (local.set $result (i32.add (i32.shr_u (local.get $x) (i32.const 1)) (i32.const 1)))
      (if (i32.eqz (i32.and (local.get $x) (i32.const 1)))
        (then (local.set $result (i32.add (local.get $result) (i32.shr_u (local.get $n) (i32.const 1))))))))
  (i32.rem_u (local.get $result) (local.get $n)))


(func $day21.doPo (param $n i32) (param $op i32) (param $x i32) (param $y i32)
  ;; (call $printStr
  ;;   (i32.add
  ;;     (global.get $day21.swap-position)
  ;;     (i32.mul (local.get $op) (i32.const 0x20))))
  ;; (call $printI32 (local.get $x))
  ;; (call $printI32.nl (local.get $y))

  ;; swap-position:
  (if (i32.eq (local.get $op) (i32.const 0))
    (then (return_call $day21.doOp (local.get $n) (local.get $op) (local.get $x) (local.get $y))))

  ;; swap-letter:
  (if (i32.eq (local.get $op) (i32.const 1))
    (then (return_call $day21.doOp (local.get $n) (local.get $op) (local.get $x) (local.get $y))))

  ;; rotate-left:
  (if (i32.eq (local.get $op) (i32.const 2))
    (then (return_call $day21.doOp (local.get $n) (i32.const 3) (local.get $x) (local.get $y))))

  ;; rotate-right:
  (if (i32.eq (local.get $op) (i32.const 3))
    (then (return_call $day21.doOp (local.get $n) (i32.const 2) (local.get $x) (local.get $y))))

  ;; rotate-based:
  (if (i32.eq (local.get $op) (i32.const 4))
    (then
      (local.set $x
        (i32.sub
          (call $strchr (global.get $day21.str) (local.get $x))
          (global.get $day21.str)))
      (local.set $x (call $day21.unrot (local.get $x) (local.get $n)))
      (return_call $day21.doOp (local.get $n) (i32.const 2) (local.get $x) (i32.const 0))))

  ;; reverse-positions:
  (if (i32.eq (local.get $op) (i32.const 5))
    (then (return_call $day21.doOp (local.get $n) (local.get $op) (local.get $x) (local.get $y))))

  ;; move-position:
  (if (i32.eq (local.get $op) (i32.const 6))
    (then (return_call $day21.doOp (local.get $n) (local.get $op) (local.get $y) (local.get $x))))
  unreachable)

(func $day21.part0.common (param $filename i32) (param $n i32) (result i32)
  (local $instrs i32)
  (local $instr i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)

  (local.set $instrs (drop (call $day21.parse (local.get $filename))))
  (i64.store (global.get $day21.str) (i64.const 0x68676665_64636261))
  (i32.store8 (i32.add (global.get $day21.str) (local.get $n)) (i32.const 0))

  (loop $loop
    ;; (call $printStr.nl (global.get $day21.str))
    (if (local.tee $instr (i32.load (local.get $instrs)))
      (then
        (local.set $y (i32.and (i32.shr_u (local.get $instr) (i32.const 16)) (i32.const 0xff)))
        (local.set $x (i32.and (i32.shr_u (local.get $instr) (i32.const 8)) (i32.const 0xff)))
        (local.set $op (i32.and (i32.shr_u (local.get $instr) (i32.const 0)) (i32.const 0xff)))
        (call $day21.doOp (local.get $n) (local.get $op) (local.get $x) (local.get $y))
        (local.set $instrs (i32.add (local.get $instrs) (i32.const 4)))
        (br $loop))))

  (call $rPad (i32.const 0x20) (global.get $day21.str) (i32.const 8)))

(func $day21.part1.common (param $filename i32) (result i32)
  (local $instrs i32)
  (local $i i32)
  (local $op i32)
  (local $x i32)
  (local $y i32)
  (local $n i32)

  (local.set $instrs (local.set $i (call $day21.parse (local.get $filename))))
  (local.set $n (call $strlen (global.get $day21.str)))

  (loop $loop
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    ;; (call $printStr.nl (global.get $day21.str))
    (local.set $op
      (i32.load
        (i32.add
          (local.get $instrs)
          (i32.shl (local.get $i) (i32.const 2)))))
    (local.set $y (i32.and (i32.shr_u (local.get $op) (i32.const 16)) (i32.const 0xff)))
    (local.set $x (i32.and (i32.shr_u (local.get $op) (i32.const 8)) (i32.const 0xff)))
    (local.set $op (i32.and (i32.shr_u (local.get $op) (i32.const 0)) (i32.const 0xff)))
    (call $day21.doPo (local.get $n) (local.get $op) (local.get $x) (local.get $y))
    (br_if $loop (local.get $i)))

  (call $rPad (i32.const 0x20) (global.get $day21.str) (i32.const 8)))

(func $day21.part0.ex (param $filename i32) (result i32)
  (call $day21.part0.common (local.get $filename) (i32.const 5)))

(func $day21.part0.real (param $filename i32) (result i32)
  (call $day21.part0.common (local.get $filename) (i32.const 8)))

(func $day21.part1.ex (param $filename i32) (result i32)
  (memory.copy (global.get $day21.str) (global.get $day21.part1.init.ex) (i32.const 16))
  (call $day21.part1.common (local.get $filename)))

(func $day21.part1.real (param $filename i32) (result i32)
  (memory.copy (global.get $day21.str) (global.get $day21.part1.init.real) (i32.const 16))
  (call $day21.part1.common (local.get $filename)))

(elem (table $mains) (i32.const 21) $day21.main)
(func $day21.main (result i32)
  (call $day21.unrot.check)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x15_8000)
      (i32.const 0x150) (i32.const 0x15_9000)
      (i32.const 0x152) (i32.const 0x15_9010)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x15_8020)
      (i32.const 0x151) (i32.const 0x15_9020)
      (i32.const 0x153) (i32.const 0x15_9030)))
  nop)
