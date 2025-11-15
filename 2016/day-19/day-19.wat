(data (i32.const 0x13_8000) "day-19/input-ex0.txt")
(data (i32.const 0x13_8020) "day-19/input-real0.txt")

(elem (table $fns) (i32.const 0x130) $day19.part0 $day19.part1)


(func $day19.part0 (param $filename i32) (result i64)
  (local $n i32)
  (local $start i32)
  (local $spacing i32)
  (local $odd i32)

  (local.set $n (call $parseI32 (drop (call $readFile (local.get $filename)))))
  (local.set $spacing (i32.const 1))

  (loop $loop
    (if (i32.le_s (local.get $n) (i32.const 2))
      (then (return (i64.extend_i32_s (i32.add (local.get $start) (i32.const 1))))))
    (local.set $odd (i32.and (local.get $n) (i32.const 1)))
    (local.set $n (i32.shr_u (local.get $n) (i32.const 1)))
    (local.set $spacing (i32.shl (local.get $spacing) (i32.const 1)))
    (if (local.get $odd)
      (then
        (local.set $start (i32.add (local.get $start) (local.get $spacing)))))
    (br $loop))
  unreachable)

;; (left x right count)
(func $day19.makeTree (param $start i32) (param $end i32) (result i32)
  (local $mid i32)
  (if (i32.ge_s (local.get $start) (local.get $end))
    (then (return (i32.const 0))))
  (local.set $mid (i32.shr_u (i32.add (local.get $start) (local.get $end)) (i32.const 1)))
  (call $cons.4
    (call $day19.makeTree (local.get $start) (local.get $mid))
    (local.get $mid)
    (call $day19.makeTree (i32.add (local.get $mid) (i32.const 1)) (local.get $end))
    (i32.sub (local.get $end) (local.get $start))))

(func $day19.treeDel (param $tree i32) (param $i i32) (result i32 i32)
  (local $left i32)
  (local $x i32)
  (local $right i32)
  (local $count i32)
  (local $leftCount i32)
  (local $leftRight i32)
  (local $leftMax i32)
  (local $popped i32)

  (call $assert (local.get $tree))
  (local.set $left
    (local.set $x
      (local.set $right
        (local.set $count
          (call $uncons.4 (local.get $tree))))))

  (block $done
    (if (local.get $left)
      (then (local.set $leftCount (call $cjr (local.get $left)))))

    (if (i32.lt_s (local.get $i) (local.get $leftCount))
      (then
        (call $setCar (local.get $tree)
          (local.set $popped
            (call $day19.treeDel (local.get $left) (local.get $i))))
        (br $done)))
    (if (i32.eq (local.get $leftCount) (local.get $i))
      (then
        (local.set $popped (local.get $x))
        (if (i32.eqz (local.get $left))
          (then
            (local.set $tree (local.get $right))
            (br $done)))
        ;; remove max element from left
        (call $setCar (local.get $tree)
          (local.set $leftMax
            (call $day19.treeDel (local.get $left) (i32.sub (local.get $leftCount) (i32.const 1)))))
        (call $setCdr (local.get $tree) (local.get $leftMax))
        (br $done)))
    (call $setCgr (local.get $tree)
      (local.set $popped
        (call $day19.treeDel
          (local.get $right)
          (i32.sub (i32.sub (local.get $i) (local.get $leftCount)) (i32.const 1)))))
    (br $done))

  (if (local.get $tree)
    (then (call $setCjr (local.get $tree) (i32.sub (local.get $count) (i32.const 1)))))
  (local.get $tree) (local.get $popped))

(func $treePrint (param $tree i32)
  (if (i32.eqz (local.get $tree))
    (then (call $printStr.small (i64.const 0x2e)))
    (else
      (call $printStr.small (i64.const 0x23_28 (;"(#";)))
      (call $printI32.unpadded (call $cjr (local.get $tree)))
      (call $printStr.small (i64.const 0x20))
      (call $treePrint (call $car (local.get $tree)))
      (call $printStr.small (i64.const 0x20))
      (call $printI32.unpadded (call $cdr (local.get $tree)))
      (call $printStr.small (i64.const 0x20))
      (call $treePrint (call $cgr (local.get $tree)))
      (call $printStr.small (i64.const 0x29 (;')';))))))

(func $day19.part1 (param $filename i32) (result i64)
  (local $n i32)
  (local $tree i32)
  (local $i i32)
  (local $target i32)

  (local.set $n (call $parseI32 (drop (call $readFile (local.get $filename)))))

  (local.set $tree
    (call $day19.makeTree
      (i32.const 1)
      (i32.add (local.get $n) (i32.const 1))))

  (loop $turn
    (if (i32.eq (local.get $n) (i32.const 1))
      (then (return (i64.extend_i32_s (call $cdr (local.get $tree))))))
    (local.set $target
      (i32.rem_u
        (i32.add (local.get $i) (i32.shr_u (local.get $n) (i32.const 1)))
        (local.get $n)))
    (local.set $tree
      (drop (call $day19.treeDel (local.get $tree) (local.get $target))))
    (local.set $n (i32.sub (local.get $n) (i32.const 1)))
    (if (i32.lt_u (local.get $i) (local.get $target))
      (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))
    (local.set $i (i32.rem_u (local.get $i) (local.get $n)))
    (br $turn))
  unreachable)

(elem (table $mains) (i32.const 19) $day19.main)
(func $day19.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x13_8000)
      (i32.const 0x130) (i64.const 3)
      (i32.const 0x131) (i64.const 2)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x13_8020)
      (i32.const 0x130) (i64.const 1830117)
      (i32.const 0x131) (i64.const 1417887)))
  nop)
