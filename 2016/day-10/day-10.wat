(data (i32.const 0x0a_8000) "day-10/input-ex0.txt")
(data (i32.const 0x0a_8020) "day-10/input-real0.txt")

(elem (i32.const 0x0a0) $day10.part0.ex $day10.part0.real $day10.part1)

(data (i32.const 0x0a_0000) "value")
(data (i32.const 0x0a_0010) "bot")
(data (i32.const 0x0a_0020) "output")

(func $day10.isBot (param $s i32) (result i32)
  (if (i32.eqz (call $strcmp (local.get $s) (i32.const 0x0a_0010)))
    (then (return (i32.const 1))))
  (if (i32.eqz (call $strcmp (local.get $s) (i32.const 0x0a_0020)))
    (then (return (i32.const 0))))
  (call $printStr.nl (local.get $s))
  (unreachable))

(func $day10.parseBot (param $sp i32) (result i32)
  (i32.or
    (i32.shl
      (call $parseI32 (i32.load (i32.add (local.get $sp) (i32.const 4))))
      (i32.const 1))
    (call $day10.isBot (i32.load (local.get $sp)))))

(func $day10.parseList (param $filename i32) (result i32)
  (local $lines i32)
  (local $line i32)
  (local $words i32)
  (local $word i32)

  (local $src i32)
  (local $dest0 i32)
  (local $dest1 i32)

  (local $command i32)
  (local $commandList i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (loop $lineLoop
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (block $continue
          (local.set $word
            (i32.load
              (local.tee $words
                (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))))

          (if (i32.eqz (call $strcmp (local.get $word) (i32.const 0x0a_0000)))
            (then
            ;; value _ goes to _ _
              (local.set $src (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
              (local.set $dest0 (call $day10.parseBot (i32.add (local.get $words) (i32.const 16))))
              (local.set $command (call $cons.3 (i32.const -1) (local.get $src) (local.get $dest0)))
              (br $continue)))
          (if (i32.eqz (call $strcmp (local.get $word) (i32.const 0x0a_0010)))
            (then
            ;; bot _ gives low to _ _ and high to _ _
              (local.set $src (call $day10.parseBot (local.get $words)))
              (local.set $dest0 (call $day10.parseBot (i32.add (local.get $words) (i32.const 20))))
              (local.set $dest1 (call $day10.parseBot (i32.add (local.get $words) (i32.const 40))))
              (local.set $command
                (call $cons.3 (local.get $src) (local.get $dest0) (local.get $dest1)))
              (br $continue)))
          (unreachable))
        (local.set $commandList (call $cons (local.get $command) (local.get $commandList)))
        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (br $lineLoop))))
  (local.get $commandList))

(func $day10.maxIndex (param $list i32) (result i32)
  (local $command i32)
  (local $max i32)
  (local $i0 i32)
  (local $i1 i32)
  (local $i2 i32)
  (loop $loop
    (if (i32.eqz (local.get $list))
      (then (return (local.get $max))))
    (local.set $command
      (local.set $list
        (call $uncons (local.get $list))))
    (local.set $i0 (local.set $i1 (local.set $i2 (call $uncons.3 (local.get $command)))))
    (if (i32.eq (local.get $i0) (i32.const -1))
      (then
        (local.set $max (call $i32.max (local.get $max) (local.get $i2))))
      (else
        (local.set $max (call $i32.max (local.get $max) (local.get $i0)))
        (local.set $max (call $i32.max (local.get $max) (local.get $i1)))
        (local.set $max (call $i32.max (local.get $max) (local.get $i2)))))
    (br $loop))
  (unreachable))

(func $day10.send (param $bots i32) (param $value i32) (param $dest i32)
  (local $botPtr i32)
  (local $i0 i32)
  (local $i1 i32)
  (local $i2 i32)
  (local $lo i32)
  (local $hi i32)

  (local.set $botPtr 
    (i32.add (local.get $bots) (i32.mul (local.get $dest) (i32.const 4))))

  (if (i32.eqz (i32.and (local.get $dest) (i32.const 1)))
    (then
    ;; output
      (call $assert_not (i32.load (local.get $botPtr)))
      (i32.store (local.get $botPtr) (local.get $value)))
    (else
    ;; bot
      (local.set $i0 (local.set $i1 (local.set $i2 (call $uncons.3 (i32.load (local.get $botPtr))))))
      (if (i32.lt_s (local.get $i0) (i32.const 0))
        (then
        ;; store value in bot inbox
          (call $assert (i32.eq (local.get $i0) (i32.const -2)))
          (call $setCar (i32.load (local.get $botPtr)) (local.get $value)))
        (else
        ;; pop value and process
          (local.set $lo (call $i32.min (local.get $i0) (local.get $value)))
          (local.set $hi (call $i32.max (local.get $i0) (local.get $value)))
          (call $setCar (i32.load (local.get $botPtr))
            (i32.or
              (i32.const 0xffff_0000)
              (i32.or
                (i32.shl (local.get $lo) (i32.const 8))
                (local.get $hi))))
          (call $day10.send (local.get $bots) (local.get $lo) (local.get $i1))
          (call $day10.send (local.get $bots) (local.get $hi) (local.get $i2)))))))

(func $findBot (param $bots i32) (param $maxIndex i32) (param $targetLo i32) (param $targetHi i32) (result i64)
  (local $target i32)
  (local $i i32)

  (local.set $target
    (i32.or
      (i32.const 0xffff_0000)
      (i32.or
        (i32.shl (local.get $targetLo) (i32.const 8))
        (local.get $targetHi))))

  (local.set $i (i32.const 1))
  (loop $loop
    (if (i32.le_u (local.get $i) (local.get $maxIndex))
      (then
        (if
          (i32.eq
            (local.get $target)
            (i32.load (i32.load (i32.add (local.get $bots) (i32.mul (local.get $i) (i32.const 4))))))
            (then (return (i64.extend_i32_s (i32.shr_u (local.get $i) (i32.const 1))))))
        (local.set $i (i32.add (local.get $i) (i32.const 2)))
        (br $loop))))
  (unreachable))

(func $day10.part0.common (param $filename i32) (result i32 i32)
  (local $commandList i32)
  (local $maxIndex i32)
  (local $bots i32)
  (local $list i32)
  (local $command i32)
  (local $i0 i32)
  (local $i1 i32)
  (local $i2 i32)
  (local $target i32)

  (local.set $commandList (call $day10.parseList (local.get $filename)))
  (local.set $bots
    (call $malloc
      (i32.mul
        (local.tee $maxIndex (call $day10.maxIndex (local.get $commandList)))
        (i32.const 4))))

  (local.set $list (local.get $commandList))
  (loop $loop
    (if (local.get $list)
      (then 
        (local.set $command
          (local.set $list
            (call $uncons (local.get $list))))
        (local.set $i0 (local.set $i1 (local.set $i2 (call $uncons.3 (local.get $command)))))
        (if (i32.gt_s (local.get $i0) (i32.const 0))
          (then
            (call $setCar (local.get $command) (i32.const -2))
            (i32.store
              (i32.add
                (local.get $bots) 
                (i32.mul (local.get $i0) (i32.const 4)))
              (local.get $command))))
        (br $loop))))

  (local.set $list (local.get $commandList))
  (loop $loop
    (if (local.get $list)
      (then 
        (local.set $command
          (local.set $list
            (call $uncons (local.get $list))))
        (local.set $i0 (local.set $i1 (local.set $i2 (call $uncons.3 (local.get $command)))))
        (if (i32.eq (local.get $i0) (i32.const -1))
          (then
            (call $day10.send (local.get $bots) (local.get $i1) (local.get $i2))))
        (br $loop))))

  (local.get $bots)
  (local.get $maxIndex))

(func $day10.part0.ex (param $filename i32) (result i64)
  (call $findBot
    (call $day10.part0.common (local.get $filename))
    (i32.const 2) (i32.const 5)))

(func $day10.part0.real (param $filename i32) (result i64)
  (call $findBot
    (call $day10.part0.common (local.get $filename))
    (i32.const 17) (i32.const 61)))

(func $day10.part1 (param $filename i32) (result i64)
  (local $bots i32)
  (local.set $bots (drop (call $day10.part0.common (local.get $filename))))
  (i64.extend_i32_s
    (i32.const 1)
    (i32.mul (i32.load (i32.add (local.get $bots) (i32.const 0))))
    (i32.mul (i32.load (i32.add (local.get $bots) (i32.const 8))))
    (i32.mul (i32.load (i32.add (local.get $bots) (i32.const 16))))))

(elem (table $mains) (i32.const 10) $day10.main)
(func $day10.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0a_8000)
      (i32.const 0x0a0) (i64.const 2)
      (i32.const 0x0a2) (i64.const 30)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0a_8020)
      (i32.const 0x0a1) (i64.const 86)
      (i32.const 0x0a2) (i64.const 22847)))
  nop)
