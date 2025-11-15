(data (i32.const 0x11_8000) "day-17/input-ex0.txt")
(data (i32.const 0x11_8040) "day-17/input-ex1.txt")
(data (i32.const 0x11_8080) "day-17/input-ex2.txt")
(data (i32.const 0x11_80c0) "day-17/input-real0.txt")

(data (i32.const 0x11_1000) "                        DDRRRD")
(data (i32.const 0x11_1020) "                 370")
(data (i32.const 0x11_1040) "                  DDUDRLRRUDRD")
(data (i32.const 0x11_1060) "                 492")
(data (i32.const 0x11_1080) "DRURDRUDDLLDLUURRDULRLDUUDDDRR")
(data (i32.const 0x11_10a0) "                 830")
(data (i32.const 0x11_10c0) "                    RLDRUDRDDR")
(data (i32.const 0x11_10e0) "                    ")
(global $day17.buf i32 (i32.const 0x11_9000))

(elem (table $fns) (i32.const 0x110) $day17.part0 $day17.part1)

(func $day17.doors (param $context i32) (param $str i32) (param $n i32) (result i32)
  (local $hash i32)
  (local.set $hash
    (drop (drop (drop (call $md5.digest (local.get $context) (local.get $str) (local.get $n))))))

  (i32.const 0)
  (i32.or
    (i32.shl
      (i32.lt_s (i32.const 0xa) (i32.and (i32.shr_u (local.get $hash) (i32.const 4)) (i32.const 0xf)))
      (i32.const 0)))
  (i32.or
    (i32.shl
      (i32.lt_s (i32.const 0xa) (i32.and (i32.shr_u (local.get $hash) (i32.const 0)) (i32.const 0xf)))
      (i32.const 4)))
  (i32.or
    (i32.shl
      (i32.lt_s (i32.const 0xa) (i32.and (i32.shr_u (local.get $hash) (i32.const 12)) (i32.const 0xf)))
      (i32.const 8)))
  (i32.or
    (i32.shl
      (i32.lt_s (i32.const 0xa) (i32.and (i32.shr_u (local.get $hash) (i32.const 8)) (i32.const 0xf)))
      (i32.const 12))))

(func $day17.push (param $locs i32) (param $str i32) (param $ch i32) (param $y i32) (param $x i32) (result i32)
  (local $n i32)
  (local $newStr i32)

  (if (i32.gt_u (local.get $y) (i32.const 3))
    (then (return (local.get $locs))))
  (if (i32.gt_u (local.get $x) (i32.const 3))
    (then (return (local.get $locs))))

  (local.set $n (call $strlen (local.get $str)))
  (local.set $newStr (call $malloc (i32.add (local.get $n) (i32.const 2))))
  (memory.copy (local.get $newStr) (local.get $str) (local.get $n))
  (i32.store8 (i32.add (local.get $newStr) (local.get $n)) (local.get $ch))

  (call $cons.3
    (local.get $newStr)
    (i32.or (i32.shl (local.get $y) (i32.const 2)) (local.get $x))
    (local.get $locs)))

(func $day17.part0 (param $filename i32) (result i32)
  (local $code i32)
  (local $context i32)
  (local $locs i32)
  (local $newLocs i32)
  (local $n i32)
  (local $y i32)
  (local $x i32)
  (local $doors i32)

  (local.set $code
    (i32.load
      (drop
        (call $splitDestructively
          (drop (call $readFile (local.get $filename)))
          (i32.const 0x0a)
          (i32.const 0)))))

  (local.set $n (call $strlen (local.get $code)))
  (local.set $newLocs (call $cons.3 (local.get $code) (i32.const 0) (i32.const 0)))
  (local.set $context (call $md5.createContext))

  (loop $loop.expand
    (local.set $locs (local.get $newLocs))
    (local.set $newLocs (i32.const 0))
    (call $assert (local.get $locs))
    (loop $loop.iterate
      (if (local.get $locs)
        (then
          (local.set $code
            (local.set $x
              (local.set $locs
                (call $uncons.3 (local.get $locs)))))

          (if (i32.eq (local.get $x) (i32.const 0xf))
            (then
              (return
                (call $rPad
                  (i32.const 0x20)
                  (i32.add (local.get $code) (local.get $n))
                  (i32.const 30)))))

          (local.set $y (i32.shr_u (local.get $x) (i32.const 2)))
          (local.set $x (i32.and (local.get $x) (i32.const 3)))

          (local.set $doors
            (call $day17.doors
              (local.get $context)
              (local.get $code)
              (call $strlen (local.get $code))))

          (if (i32.and (local.get $doors) (i32.const 0x0001))
            (then
              (local.set $newLocs
                (call $day17.push
                  (local.get $newLocs) (local.get $code) (i32.const 0x55)
                  (i32.sub (local.get $y) (i32.const 1)) (local.get $x)))))
          (if (i32.and (local.get $doors) (i32.const 0x0010))
            (then
              (local.set $newLocs
                (call $day17.push
                  (local.get $newLocs) (local.get $code) (i32.const 0x44)
                  (i32.add (local.get $y) (i32.const 1)) (local.get $x)))))
          (if (i32.and (local.get $doors) (i32.const 0x0100))
            (then
              (local.set $newLocs
                (call $day17.push
                  (local.get $newLocs) (local.get $code) (i32.const 0x4c)
                  (local.get $y) (i32.sub (local.get $x) (i32.const 1))))))
          (if (i32.and (local.get $doors) (i32.const 0x1000))
            (then
              (local.set $newLocs
                (call $day17.push
                  (local.get $newLocs) (local.get $code) (i32.const 0x52)
                  (local.get $y) (i32.add (local.get $x) (i32.const 1))))))
          (br $loop.iterate))))
    (br $loop.expand))
  unreachable)

(func $day17.step
  (param $context i32) (param $buf i32)
  (param $len i32) (param $ch i32)
  (param $y i32) (param $x i32) (result i32)


  (return_call $day17.dfs
    (local.get $context) (local.get $buf)
    (i32.add (local.get $len) (i32.const 1)) (local.get $y) (local.get $x)))

(func $day17.dfs (param $context i32) (param $len i32) (param $y i32) (param $x i32) (result i32)
  (local $doors i32)
  (local $best i32)
  (local $ptr i32)

  (if (i32.gt_u (local.get $y) (i32.const 3))
    (then (return (i32.const 0))))
  (if (i32.gt_u (local.get $x) (i32.const 3))
    (then (return (i32.const 0))))
  (if (i32.eq (i32.add (local.get $x) (local.get $y)) (i32.const 6))
    (then (return (local.get $len))))

  (call $debugger)

  (local.set $doors
    (call $day17.doors (local.get $context) (global.get $day17.buf) (local.get $len)))

  (if (i32.and (local.get $doors) (i32.const 0x0001))
    (then
      (i32.store8 (i32.add (global.get $day17.buf) (local.get $len)) (i32.const 0x55))
      (local.set $best
        (call $i32.max (local.get $best)
          (call $day17.dfs
            (local.get $context) (i32.add (local.get $len) (i32.const 1))
            (i32.sub (local.get $y) (i32.const 1)) (local.get $x))))))
  (if (i32.and (local.get $doors) (i32.const 0x0010))
    (then
      (i32.store8 (i32.add (global.get $day17.buf) (local.get $len)) (i32.const 0x44))
      (local.set $best
        (call $i32.max (local.get $best)
          (call $day17.dfs
            (local.get $context) (i32.add (local.get $len) (i32.const 1))
            (i32.add (local.get $y) (i32.const 1)) (local.get $x))))))
  (if (i32.and (local.get $doors) (i32.const 0x0100))
    (then
      (i32.store8 (i32.add (global.get $day17.buf) (local.get $len)) (i32.const 0x4c))
      (local.set $best
        (call $i32.max (local.get $best)
          (call $day17.dfs
            (local.get $context) (i32.add (local.get $len) (i32.const 1))
            (local.get $y) (i32.sub (local.get $x) (i32.const 1)))))))
  (if (i32.and (local.get $doors) (i32.const 0x1000))
    (then
      (i32.store8 (i32.add (global.get $day17.buf) (local.get $len)) (i32.const 0x52))
      (local.set $best
        (call $i32.max (local.get $best)
          (call $day17.dfs
            (local.get $context) (i32.add (local.get $len) (i32.const 1))
            (local.get $y) (i32.add (local.get $x) (i32.const 1)))))))
  (i32.store8 (i32.add (global.get $day17.buf) (local.get $len)) (i32.const 0))

  (local.get $best))

(func $day17.part1 (param $filename i32) (result i32)
  (local $code i32)
  (local $context i32)
  (local $codeLen i32)

  (local.set $context (call $md5.createContext))

  (local.set $code
    (i32.load
      (drop
        (call $splitDestructively
          (drop (call $readFile (local.get $filename)))
          (i32.const 0x0a)
          (i32.const 0)))))

  (memory.copy
    (global.get $day17.buf)
    (local.get $code)
    (local.tee $codeLen (call $strlen (local.get $code))))

  (call $formatI32
    (i32.sub
      (call $day17.dfs
        (local.get $context) (local.get $codeLen)
        (i32.const 0) (i32.const 0))
      (local.get $codeLen))))

(elem (table $mains) (i32.const 17) $day17.main)
(func $day17.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputStr
      (i32.const 0x11_8000)
      (i32.const 0x110) (i32.const 0x11_1000)
      (i32.const 0x111) (i32.const 0x11_1020)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x11_8040)
      (i32.const 0x110) (i32.const 0x11_1040)
      (i32.const 0x111) (i32.const 0x11_1060)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x11_8080)
      (i32.const 0x110) (i32.const 0x11_1080)
      (i32.const 0x111) (i32.const 0x11_10a0)))
  (i32.add
    (call $checkInputStr
      (i32.const 0x11_80c0)
      (i32.const 0x110) (i32.const 0x11_10c0)
      (i32.const 0x111) (i32.const 0x11_10e0)))
  nop)
