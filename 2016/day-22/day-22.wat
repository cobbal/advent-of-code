(data (i32.const 0x16_8000) "day-22/input-ex0.txt")
(data (i32.const 0x16_8020) "day-22/input-real0.txt")

(elem (table $fns) (i32.const 0x160) $day22.part0 $day22.part1)

(func $day22.parse (param $filename i32) (result i32)
  (local $lines i32)
  (local $n i32)
  (local $result i32)
  (local $ptr i32)
  (local $line i32)
  (local $words i32)
  (local $x i32)
  (local $y i32)
  (local $cap i32)
  (local $used i32)

  (local.set $lines
    (local.set $n
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  ;; skip 2 lines
  (local.set $lines (i32.add (local.get $lines) (i32.const 8)))
  (local.set $n (i32.sub (local.get $n) (i32.const 2)))

  (local.set $result
    (local.tee $ptr
      (call $malloc (i32.shl (i32.add (local.get $n) (i32.const 1)) (i32.const 2)))))
  (loop $loop.lines
    (if (local.tee $line (i32.load (local.get $lines)))
      (then
        (local.set $words
          (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0))))
        (local.set $x
          (call $parseI32
            (i32.add
              (call $strchr (i32.load (local.get $words)) (i32.const 0x78 (;'x';)))
              (i32.const 1))))
        (local.set $y
          (call $parseI32
            (i32.add
              (call $strchr (i32.load (local.get $words)) (i32.const 0x79 (;'y';)))
              (i32.const 1))))
        (local.set $cap (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 4)))))
        (local.set $used (call $parseI32 (i32.load (i32.add (local.get $words) (i32.const 8)))))
        (i32.store
          (local.get $ptr)
          (call $cons.4 (local.get $y) (local.get $x) (local.get $used) (local.get $cap)))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (br $loop.lines))))
  (local.get $result))

(func $day22.minThing (param $offset i32) (param $nodes i32) (result i32)
  (local $min i32)
  (local.set $min (i32.const 0x7fff_ffff))
  (loop $loop
    (local.set $min
      (call $i32.min
        (local.get $min)
        (i32.load (i32.add (i32.load (local.get $nodes)) (local.get $offset)))))
    (br_if $loop (i32.load (local.tee $nodes (i32.add (local.get $nodes) (i32.const 4))))))
  (local.get $min))

(func $day22.maxThing (param $offset i32) (param $nodes i32) (result i32)
  (local $max i32)
  (local.set $max (i32.const 0x8000_0000))
  (loop $loop
    (local.set $max
      (call $i32.max
        (local.get $max)
        (i32.load (i32.add (i32.load (local.get $nodes)) (local.get $offset)))))
    (br_if $loop (i32.load (local.tee $nodes (i32.add (local.get $nodes) (i32.const 4))))))
  (local.get $max))

(func $day22.part0 (param $filename i32) (result i64)
  (local $nodes i32)
  (local $ptrA i32)
  (local $ptrB i32)
  (local $usedA i32)
  (local $usedB i32)
  (local $capB i32)
  (local $pairs i32)

  (local.set $nodes
    (local.tee $ptrA
      (call $day22.parse (local.get $filename))))

  (loop $loop.a
    (local.set $ptrB (local.get $nodes))
    (loop $loop.b
      (block $continue.b
        (local.set $usedA (call $cgr (i32.load (local.get $ptrA))))
        (local.set $usedB (call $cgr (i32.load (local.get $ptrB))))
        (local.set $capB (call $cjr (i32.load (local.get $ptrB))))
        (br_if $continue.b (i32.eqz (local.get $usedA)))
        (br_if $continue.b (i32.eq (local.get $ptrA) (local.get $ptrB)))
        (br_if $continue.b
          (i32.gt_s
            (i32.add (local.get $usedA) (local.get $usedB))
            (local.get $capB)))
        (local.set $pairs (i32.add (local.get $pairs) (i32.const 1))))
      (br_if $loop.b (i32.load (local.tee $ptrB (i32.add (local.get $ptrB) (i32.const 4))))))
    (br_if $loop.a (i32.load (local.tee $ptrA (i32.add (local.get $ptrA) (i32.const 4))))))

  (i64.extend_i32_s (local.get $pairs)))

(func $day22.classify (param $minCap i32) (param $cell i32) (result i32)
  (local $used i32)
  (local $cap i32)

  (local.set $used (call $cgr (local.get $cell)))
  (local.set $cap (call $cjr (local.get $cell)))

  (if (i32.eqz (local.get $used))
    (then (return (i32.const 0x5f (;'_';)))))
  (if (i32.gt_s (local.get $used) (local.get $minCap))
    (then (return (i32.const 0x23 (;'#';)))))
  (i32.const 0x2e (;'.';)))

(func $day22.insertIfEmpty (param $set i32) (param $grid i32) (param $y i32) (param $x i32) (result i32)
  (if
    (i32.ne
      (i32.const 0x2e)
      (call $grid.getSafe (local.get $grid) (local.get $y) (local.get $x) (i32.const 0x23)))
    (then (return (local.get $set))))
  (call $set.insert
    (local.get $set)
    (call $grid.index.pack (local.get $grid) (local.get $y) (local.get $x))))

(func $day22.bfs (param $grid i32) (param $loc i32) (param $target i32) (result i32)
  (local $steps i32)
  (local $set i32)
  (local $nextSet i32)
  (local $n i32)
  (local $yx i32)
  (local $y i32)
  (local $x i32)

  (local.set $set (call $set.singleton (global.get $fns.i32.cmp_s) (local.get $loc)))

  (loop $loop.step
    (local.set $nextSet (call $set.empty (global.get $fns.i32.cmp_s)))
    (local.set $n (call $map.count (local.get $set)))
    (local.set $set (call $map.elements (local.get $set)))
    (loop $loop.inStep
      (local.set $yx (i32.load (local.get $set)))
      (if (i32.eq (local.get $yx) (local.get $target))
        (then (return (local.get $steps))))
      (local.set $y (local.set $x (call $grid.index.unpack (local.get $grid) (local.get $yx))))
      (local.get $nextSet)
      (call $day22.insertIfEmpty (local.get $grid) (i32.sub (local.get $y) (i32.const 1)) (local.get $x))
      (call $day22.insertIfEmpty (local.get $grid) (i32.add (local.get $y) (i32.const 1)) (local.get $x))
      (call $day22.insertIfEmpty (local.get $grid) (local.get $y) (i32.sub (local.get $x) (i32.const 1)))
      (call $day22.insertIfEmpty (local.get $grid) (local.get $y) (i32.add (local.get $x) (i32.const 1)))
      (local.set $nextSet)
      (local.set $set (i32.add (local.get $set) (i32.const 8)))
      (br_if $loop.inStep (local.tee $n (i32.sub (local.get $n) (i32.const 1)))))

    (local.set $steps (i32.add (local.get $steps) (i32.const 1)))
    (local.set $set (local.get $nextSet))
    (br $loop.step))
  (i32.const -1))

(func $day22.part1 (param $filename i32) (result i64)
  (local $nodes i32)
  (local $minCap i32)
  (local $height i32)
  (local $width i32)
  (local $grid i32)
  (local $cell i32)
  (local $steps i64)
  (local $blank i32)
  (local $target i32)

  (local.set $nodes (call $day22.parse (local.get $filename)))
  (local.set $minCap (call $day22.minThing (i32.const 12) (local.get $nodes)))
  (local.set $height (i32.add (call $day22.maxThing (i32.const 0) (local.get $nodes)) (i32.const 1)))
  (local.set $width (i32.add (call $day22.maxThing (i32.const 4) (local.get $nodes)) (i32.const 1)))

  (local.set $grid (call $grid.create (local.get $height) (local.get $width)))
  (call $grid.fill (local.get $grid) (i32.const 0x3f))
  (loop $loop.gridInit
    (local.set $cell (i32.load (local.get $nodes)))
    (call $grid.set
      (local.get $grid)
      (call $uncons (local.get $cell))
      (call $day22.classify (local.get $minCap) (local.get $cell)))
    (br_if $loop.gridInit (i32.load (local.tee $nodes (i32.add (local.get $nodes) (i32.const 4))))))
  (call $grid.set
    (local.get $grid)
    (i32.const 0)
    (i32.sub (local.get $width) (i32.const 1))
    (i32.const 0x47))
  (local.set $blank (call $grid.find (local.get $grid) (i32.const 0x5f)))
  (local.set $target (call $grid.find (local.get $grid) (i32.const 0x47)))

  (loop $solve
    (if (i32.eq (call $grid.get (local.get $grid) (i32.const 0) (i32.const 0)) (i32.const 0x47))
      (then (return (local.get $steps))))
    ;; Phase 0: move blank to left of target
    (local.set $steps
      (i64.add
        (local.get $steps)
        (i64.extend_i32_s
          (call $day22.bfs
            (local.get $grid)
            (local.get $blank)
            (i32.sub (local.get $target) (i32.const 1))))))
    (call $grid.set
      (local.get $grid)
      (call $grid.index.unpack (local.get $grid) (local.get $blank))
      (i32.const 0x2e))
    (call $grid.set
      (local.get $grid)
      (call $grid.index.unpack (local.get $grid) (local.tee $blank (i32.sub (local.get $target) (i32.const 1))))
      (i32.const 0x5f))
    ;; Phase 1: swap blank with target
    (local.set $steps (i64.add (local.get $steps) (i64.const 1)))
    (local.get $blank) (local.get $target)
    (local.set $blank) (local.set $target)
    (call $grid.set
      (local.get $grid)
      (call $grid.index.unpack (local.get $grid) (local.get $blank))
      (i32.const 0x5f))
    (call $grid.set
      (local.get $grid)
      (call $grid.index.unpack (local.get $grid) (local.get $target))
      (i32.const 0x47))
    (br $solve))

  (i64.extend_i32_s (local.get $minCap)))

(elem (table $mains) (i32.const 22) $day22.main)
(func $day22.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x16_8000)
      (i32.const 0x160) (i64.const 7)
      (i32.const 0x161) (i64.const 7)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x16_8020)
      (i32.const 0x160) (i64.const 1003)
      (i32.const 0x161) (i64.const 192)))
  nop)
