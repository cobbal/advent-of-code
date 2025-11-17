(data (i32.const 0x18_8000) "day-24/input-ex0.txt")
(data (i32.const 0x18_8020) "day-24/input-real0.txt")

(elem (table $fns) (i32.const 0x180) $day24.part0 $day24.part1)

(func $day24.parse (param $filename i32) (result i32 i32)
  (local $width i32)
  (local $height i32)
  (local $lines i32)
  (local $grid i32)
  (local $y i32)
  (local $locs i32)
  (local $loc i32)
  (local $i i32)

  (local.set $lines
    (local.set $height
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (local.set $grid
    (call $grid.create
      (local.get $height)
      (local.tee $width (call $strlen (i32.load (local.get $lines))))))

  (loop $grid.init
    (memory.copy
      (call $grid._ptr (local.get $grid) (local.get $y) (i32.const 0))
      (i32.load (local.get $lines))
      (local.get $width))
    (local.set $y (i32.add (local.get $y) (i32.const 1)))
    (br_if $grid.init (i32.load (local.tee $lines (i32.add (local.get $lines) (i32.const 4))))))

  (local.set $i (i32.const 10))
  (loop $digit.search
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (local.set $loc (call $grid.find (local.get $grid) (i32.add (i32.const 0x30) (local.get $i))))
    (if (i32.gt_s (local.get $loc) (i32.const 0))
      (then
        (local.set $locs (call $cons (local.get $loc) (local.get $locs)))
        (call $grid.set
          (local.get $grid)
          (call $grid.index.unpack (local.get $grid) (local.get $loc))
          (i32.const 0x2e))))
    (br_if $digit.search (local.get $i)))
  (local.get $grid) (local.get $locs))

(func $day24.fillDistRow (param $grid i32) (param $locs i32) (param $n i32) (param $root i32) (param $row i32)
  (local $visited i32)
  (local $frontier i32)
  (local $nextFrontier i32)
  (local $dist i32)
  (local $i i32)
  (local $yx i32)
  (local $y i32)
  (local $x i32)
  (local $target i32)

  (local.set $visited (call $set.empty (global.get $fns.i32.cmp_s)))
  (local.set $nextFrontier (call $set.singleton (global.get $fns.i32.cmp_s) (local.get $root)))

  (loop $loop.step
    (local.set $frontier (call $map.elements (local.get $nextFrontier)))
    (local.set $nextFrontier (call $set.empty (global.get $fns.i32.cmp_s)))
    (loop $loop.loc
      (block $continue.loc
        (local.set $yx (i32.load (local.get $frontier)))
        (br_if $continue.loc (call $set.contains (local.get $visited) (local.get $yx)))
        (local.set $visited (call $set.insert (local.get $visited) (local.get $yx)))

        (local.set $i (local.get $n))
        (loop $loop.targets
          (local.set $i (i32.sub (local.get $i) (i32.const 1)))
          (local.set $target (i32.load (i32.add (local.get $locs) (i32.shl (local.get $i) (i32.const 2)))))
          (if (i32.eq (local.get $yx) (local.get $target))
            (then
              (i32.store
                (i32.add (local.get $row) (i32.shl (local.get $i) (i32.const 2)))
                (local.get $dist))))
          (br_if $loop.targets (local.get $i)))

        (local.set $y (local.set $x (call $grid.index.unpack (local.get $grid) (local.get $yx))))

        ;; so what if it's 2 days stale?
        (local.get $nextFrontier)
        (call $day22.insertIfEmpty (local.get $grid) (i32.sub (local.get $y) (i32.const 1)) (local.get $x))
        (call $day22.insertIfEmpty (local.get $grid) (i32.add (local.get $y) (i32.const 1)) (local.get $x))
        (call $day22.insertIfEmpty (local.get $grid) (local.get $y) (i32.sub (local.get $x) (i32.const 1)))
        (call $day22.insertIfEmpty (local.get $grid) (local.get $y) (i32.add (local.get $x) (i32.const 1)))
        (local.set $nextFrontier))
      (br_if $loop.loc (i32.load (local.tee $frontier (i32.add (local.get $frontier) (i32.const 8))))))
    (local.set $dist (i32.add (local.get $dist) (i32.const 1)))
    (br_if $loop.step (call $set.count (local.get $nextFrontier)))))

(func $day24.tsp
  (param $matrix i32) (param $n i32)
  (param $i i32) (param $visited i32) (param $endAt0 i32)
  (result i32)

  (local $j i32)
  (local $best i32)

  (local.set $best (i32.const 0x7fff_ffff))
  (local.set $visited (i32.or (local.get $visited) (i32.shl (i32.const 1) (local.get $i))))

  (if (i32.eq (i32.popcnt (local.get $visited)) (local.get $n))
    (then
      (if (i32.eqz (local.get $endAt0))
        (then (return (i32.const 0))))
      (return
        (i32.load
          (i32.add
            (local.get $matrix)
            (i32.shl (i32.mul (local.get $i) (local.get $n)) (i32.const 2)))))))

  (loop $loop.j
    (block $continue
      (br_if $continue (i32.and (local.get $visited) (i32.shl (i32.const 1) (local.get $j))))
      (local.set $best
        (call $i32.min
          (local.get $best)
          (i32.add
            (i32.load
              (i32.add
                (local.get $matrix)
                (i32.shl (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 2))))
            (call $day24.tsp
              (local.get $matrix)
              (local.get $n)
              (local.get $j)
              (local.get $visited)
              (local.get $endAt0))))))
    (local.set $j (i32.add (local.get $j) (i32.const 1)))
    (br_if $loop.j (i32.lt_s (local.get $j) (local.get $n))))

  (local.get $best))

(func $day24.common (param $filename i32) (param $endAt0 i32) (result i64)
  (local $grid i32)
  (local $locs i32)
  (local $n i32)
  (local $distMatrix i32)
  (local $y i32)
  (local $x i32)

  (local.set $grid (local.set $locs (call $day24.parse (local.get $filename))))
  (local.set $n (call $list.len (local.get $locs)))
  (local.set $locs (call $list.toArray (local.get $locs)))
  (local.set $distMatrix (call $malloc (i32.shl (i32.mul (local.get $n) (local.get $n)) (i32.const 2))))

  (loop $loop.fillMat
    (call $day24.fillDistRow
      (local.get $grid)
      (local.get $locs)
      (local.get $n)
      (i32.load (i32.add (local.get $locs) (i32.shl (local.get $y) (i32.const 2))))
      (i32.add (local.get $distMatrix) (i32.shl (i32.mul (local.get $y) (local.get $n)) (i32.const 2))))
    (local.set $y (i32.add (local.get $y) (i32.const 1)))
    (br_if $loop.fillMat (i32.lt_s (local.get $y) (local.get $n))))

  (i64.extend_i32_s
    (call $day24.tsp (local.get $distMatrix) (local.get $n) (i32.const 0) (i32.const 0) (local.get $endAt0))))

(func $day24.part0 (param $filename i32) (result i64)
  (call $day24.common (local.get $filename) (i32.const 0)))

(func $day24.part1 (param $filename i32) (result i64)
  (call $day24.common (local.get $filename) (i32.const 1)))

(elem (table $mains) (i32.const 24) $day24.main)
(func $day24.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x18_8000)
      (i32.const 0x180) (i64.const 14)
      (i32.const 0x181) (i64.const 20)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x18_8020)
      (i32.const 0x180) (i64.const 448)
      (i32.const 0x181) (i64.const 672)))
  nop)
