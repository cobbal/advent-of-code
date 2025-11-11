(data (i32.const 0x0b_8000) "day-11/input-ex0.txt")
(data (i32.const 0x0b_8020) "day-11/input-real0.txt")

(elem (table $fns) (i32.const 0x0b0) $day11.part0 $day11.part1 $day11.stateCmp)

(data (i32.const 0x0b_0000) "F_ _  ")
(data (i32.const 0x0b_00a0) "G ")
(data (i32.const 0x0b_00a8) "M ")
(data (i32.const 0x0b_00b0) ".____")
(data (i32.const 0x0b_0100) "generator")
(data (i32.const 0x0b_0120) "compatible")
(data (i32.const 0x0b_0140) "a")
(data (i32.const 0x0b_0160) "elerium")
(data (i32.const 0x0b_0180) "dilithium")

;; struct state {
;;   elevator: i8
;;   heights: i8[elemCount * 2]
;;   terminator: i8 = 0x00
;; }
;; struct context {
;;   elemCount: i32
;;   names: list<string>
;; }

(func $day11.printFloor (param $context i32) (param $state i32) (param $floor i32)
  (local $i i32)
  (local $n i32)
  (local $name i32)
  (local $itemFloor i32)
  (local $len i32)

  (local.set $n (i32.load (local.get $context)))

  (i32.store8 (i32.const 0x0b_0001) (i32.add (i32.const 0x30) (local.get $floor)))
  (if (i32.eq (local.get $floor) (i32.load8_u (local.get $state)))
    (then (i32.store8 (i32.const 0x0b_0003) (i32.const 0x45)))
    (else (i32.store8 (i32.const 0x0b_0003) (i32.const 0x2e))))
  (call $printStr (i32.const 0x0b_0000))

  (local.set $state (i32.add (local.get $state) (i32.const 1)))
  (local.set $context (i32.load (i32.add (local.get $context) (i32.const 4))))
  (loop $loop
    (if (i32.lt_u (local.get $i) (i32.shl (local.get $n) (i32.const 1)))
      (then
        (local.set $itemFloor (i32.load8_u (i32.add (local.get $state) (local.get $i))))
        (local.set $name
          (call $list.nth
            (local.get $context)
            (i32.shr_u (local.get $i) (i32.const 1))))
        (if (i32.eq (local.get $itemFloor) (local.get $floor))
          (then
            (call $printStr (local.get $name))
            (if (i32.and (local.get $i) (i32.const 1))
              (then (call $printStr (i32.const 0x0b_00a8)))
              (else (call $printStr (i32.const 0x0b_00a0)))))
          (else
            (local.set $len (i32.add (call $strlen (local.get $name)) (i32.const 1)))
            (memory.fill (i32.const 0x0b_00b1) (i32.const 0x20) (local.get $len))
            (i32.store8 (i32.add (i32.const 0x0b_00b1) (local.get $len)) (i32.const 0))
            (call $printStr (i32.const 0x0b_00b0))))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop))))

  (call $print.nl))

(func $day11.print (param $context i32) (param $state i32)
  (call $print.nl)
  (call $day11.printFloor (local.get $context) (local.get $state) (i32.const 4))
  (call $day11.printFloor (local.get $context) (local.get $state) (i32.const 3))
  (call $day11.printFloor (local.get $context) (local.get $state) (i32.const 2))
  (call $day11.printFloor (local.get $context) (local.get $state) (i32.const 1)))

(func $day11.parse (param $filename i32) (param $extra i32) (result i32 i32)
  (local $ptr i32)
  (local $c i32)
  (local $lines i32)
  (local $line i32)
  (local $floor i32)
  (local $words i32)
  (local $word0 i32)
  (local $word1 i32)
  (local $nameList i32)
  (local $index i32)
  (local $parseList i32)
  (local $n i32)
  (local $context i32)
  (local $state0 i32)

  (local.set $lines
    (drop
      (call $splitDestructively
        (drop (call $readFile (local.get $filename)))
        (i32.const 0x0a)
        (i32.const 0))))

  (if (local.get $extra)
    (then
      (local.set $nameList
        (call $cons (i32.const 0x0b_0160)
          (call $cons (i32.const 0x0b_0180)
            (i32.const 0))))
      (local.set $parseList
        (call $cons (i32.const 0x0)
          (call $cons (i32.const 0x4)
            (call $cons (i32.const 0x8)
              (call $cons (i32.const 0xc)
                (i32.const 0))))))))

  (loop $lineLoop
    (if (local.tee $ptr (local.tee $line (i32.load (local.get $lines))))
      (then
        (loop $depunctuate
          (if (local.tee $c (i32.load8_u (local.get $ptr)))
            (then
              (if (i32.ge_u (i32.sub (local.get $c) (i32.const 0x61)) (i32.const 26))
                (then (i32.store8 (local.get $ptr) (i32.const 0x20))))

              (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
              (br $depunctuate))))

        (local.set $words
          (i32.add
            (drop (call $splitDestructively (local.get $line) (i32.const 0x20) (i32.const 0)))
            (i32.const 20)))

        (loop $wordLoop
          (if (local.tee $word0 (i32.load (local.get $words)))
            (then
              (if (local.tee $word1 (i32.load (i32.add (local.get $words) (i32.const 4))))
                (then
                  (if (i32.eqz (call $strcmp (local.get $word0) (i32.const 0x0b_0140) (; "a" ;)))
                    (then
                      (local.set $words (i32.add (local.get $words) (i32.const 4)))
                      (br $wordLoop)))

                  (local.set $nameList
                    (local.set $index
                      (call $stringList.addToList (local.get $nameList) (local.get $word0))))

                  (if (i32.eqz (call $strcmp (local.get $word1) (i32.const 0x0b_0100) (; "generator" ;)))
                    (then
                      ;; have a 2 * index($word0) on floor $floor
                      (local.set $parseList
                        (call $cons
                          (i32.add
                            (i32.shl (local.get $index) (i32.const 3))
                            (local.get $floor))
                          (local.get $parseList)))
                      (local.set $words (i32.add (local.get $words) (i32.const 12))))
                    (else
                      (call $assert_not
                        (call $strcmp (local.get $word1) (i32.const 0x0b_0120 (; "compatible" ;))))
                      ;; have a 2 * index($word0) + 1 on floor $floor
                      (local.set $parseList
                        (call $cons
                          (i32.add
                            (i32.add
                              (i32.shl (local.get $index) (i32.const 3))
                              (i32.const 4))
                            (local.get $floor))
                          (local.get $parseList)))
                      (local.set $words (i32.add (local.get $words) (i32.const 16)))))
                  (br $wordLoop))))))

        (local.set $lines (i32.add (local.get $lines) (i32.const 4)))
        (local.set $floor (i32.add (local.get $floor) (i32.const 1)))
        (br $lineLoop))))

  (local.set $n (call $list.len (local.get $nameList)))
  (local.set $context (call $cons (local.get $n) (local.get $nameList)))

  (local.set $state0
    (call $malloc (i32.add (i32.mul (local.get $n) (i32.const 2)) (i32.const 2))))
  (i32.store8 (local.get $state0) (i32.const 1))

  (loop $postParse
    (if (local.get $parseList)
      (then
        (local.set $index
          (i32.shr_u
            (local.tee $floor (call $car (local.get $parseList)))
            (i32.const 2)))
        (local.set $floor (i32.add (i32.and (local.get $floor) (i32.const 3)) (i32.const 1)))
        (i32.store8
          (i32.add (i32.add (local.get $state0) (i32.const 1)) (local.get $index))
          (local.get $floor))
        (local.set $parseList (call $cdr (local.get $parseList)))
        (br $postParse))))

  (local.get $context) (local.get $state0))

(func $day11.stateCmp (param $s0 i32) (param $s1 i32) (result i32)
  unreachable)

(func $day11.checkState (param $context i32) (param $state i32) (result i32)
  (local $generatorFloors i32)
  (local $ptr i32)
  (local $chip i32)

  ;; (call $day11.print (local.get $context) (local.get $state))
  ;; (call $debugger)

  (local.set $ptr (i32.add (local.get $state) (i32.const 1)))
  (loop $generatorScan
    (if (i32.load8_u (local.get $ptr))
      (then
        (local.set $generatorFloors
          (i32.or
            (local.get $generatorFloors)
            (i32.shl (i32.const 1) (i32.load8_u (local.get $ptr)))))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 2)))
        (br $generatorScan))))

  (local.set $ptr (i32.add (local.get $state) (i32.const 1)))
  (loop $validate
    (if (i32.load8_u (local.get $ptr))
      (then
        (block $continue
          (if
            (i32.eq
              (local.tee $chip (i32.load8_u (i32.add (local.get $ptr) (i32.const 1))))
              (i32.load8_u (local.get $ptr)))
            (then (br $continue)))
          (if (i32.and (local.get $generatorFloors) (i32.shl (i32.const 1) (local.get $chip)))
            (then (return (i32.const 0)))))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 2)))
        (br $validate))))
  (i32.const 1))

(func $day11.bfs
  (param $context i32) (param $oldStates i32) (param $stateFinal i32) (param $depth i32) (param $seen i32)
  (result i32)

  (local $newStates i32)
  (local $newState i32)
  (local $state i32)
  (local $stateScratch i32)
  (local $n i32)
  (local $elevator i32)
  (local $i i32)
  (local $j i32)
  (local $dir i32)

  ;; (call $print.nl)
  ;; (call $printI32 (local.get $depth))
  ;; (call $printI32.nl (call $set.count (local.get $oldStates)))

  (if (call $set.contains (local.get $oldStates) (local.get $stateFinal))
    (then (return (local.get $depth))))

  (if (i32.eqz (call $set.count (local.get $oldStates)))
    (then (return (i32.const -2))))

  (local.set $n (i32.load (local.get $context)))
  (local.set $stateScratch
    (call $malloc (i32.add (i32.mul (local.get $n) (i32.const 2)) (i32.const 2))))
  (local.set $oldStates (call $map.elements (local.get $oldStates)))
  (local.set $newStates (call $set.empty (global.get $fns.strcmp)))

  (loop $loop
    (if (local.tee $state (i32.load (local.get $oldStates)))
      (then
        (local.set $elevator (i32.load8_u (local.get $state)))
        (local.set $dir (i32.const 1))
        (loop $loop.dir
          (block $loop.dir.continue
            (if (i32.ge_u (i32.sub (i32.add (local.get $elevator) (local.get $dir)) (i32.const 1)) (i32.const 4))
              (then (br $loop.dir.continue)))
            (local.set $i (i32.add (i32.mul (local.get $n) (i32.const 2)) (i32.const 1)))
            (loop $loop.i
              (local.set $i (i32.sub (local.get $i) (i32.const 1)))
              (if (i32.gt_s (local.get $i) (i32.const 0))
                (then
                  ;; check if item i is on same floor as elevator
                  (if (i32.ne (local.get $elevator) (i32.load8_u (i32.add (local.get $state) (local.get $i))))
                    (then (br $loop.i)))

                  (local.set $j (local.get $i))
                  ;; Never descend with 2 items
                  (if (i32.eq (local.get $dir) (i32.const -1))
                    (then (local.set $j (i32.const 1))))
                  (loop $loop.j
                    (local.set $j (i32.sub (local.get $j) (i32.const 1)))
                    (if (i32.ge_s (local.get $j) (i32.const 0))
                      (then
                        ;; check if item j is on same floor as elevator
                        (if (i32.ne (local.get $elevator) (i32.load8_u (i32.add (local.get $state) (local.get $j))))
                          (then (br $loop.j)))
                        (memory.copy
                          (local.get $stateScratch)
                          (local.get $state)
                          (i32.add (i32.mul (local.get $n) (i32.const 2)) (i32.const 1)))

                        (i32.store8
                          (i32.add (local.get $stateScratch) (local.get $i))
                          (i32.add (local.get $elevator) (local.get $dir)))
                        (i32.store8
                          (i32.add (local.get $stateScratch) (local.get $j))
                          (i32.add (local.get $elevator) (local.get $dir)))
                        (i32.store8
                          (local.get $stateScratch)
                          (i32.add (local.get $elevator) (local.get $dir)))
                        (if (call $day11.checkState (local.get $context) (local.get $stateScratch))
                          (then
                            (if (i32.eqz (call $set.contains (local.get $seen) (local.get $stateScratch)))
                              (then
                                (local.set $newStates
                                  (call $set.insert
                                    (local.get $newStates)
                                    (local.tee $newState (call $strdup (local.get $stateScratch)))))
                                (local.set $seen
                                  (call $set.insert
                                    (local.get $seen)
                                    (local.get $newState)))))))
                        (br $loop.j))))
                  (br $loop.i)))))

          (if (i32.eq (local.get $dir) (i32.const 1))
            (then
              (local.set $dir (i32.const -1))
              (br $loop.dir))))

        (local.set $oldStates (i32.add (local.get $oldStates) (i32.const 8)))
        (br $loop))))

  (return_call $day11.bfs
    (local.get $context)
    (local.get $newStates)
    (local.get $stateFinal)
    (i32.add (local.get $depth) (i32.const 1))
    (local.get $seen)))

(func $day11.part0 (param $filename i32) (result i64)
  (local $context i32)
  (local $state0 i32)
  (local $stateFinal i32)

  (local.set $context
    (local.set $state0
      (call $day11.parse (local.get $filename) (i32.const 0))))

  (local.set $stateFinal
    (call $malloc (i32.add (i32.mul (i32.load (local.get $context)) (i32.const 2)) (i32.const 2))))
  (memory.fill
    (local.get $stateFinal)
    (i32.const 4)
    (i32.add (i32.mul (i32.load (local.get $context)) (i32.const 2)) (i32.const 1)))

  (local.set $state0 (call $set.singleton (global.get $fns.strcmp) (local.get $state0)))

  (i64.extend_i32_s
    (call $day11.bfs
      (local.get $context)
      (local.get $state0)
      (local.get $stateFinal)
      (i32.const 0)
      (local.get $state0))))

(func $day11.part1 (param $filename i32) (result i64)
  (local $context i32)
  (local $state0 i32)
  (local $stateFinal i32)

  (local.set $context
    (local.set $state0
      (call $day11.parse (local.get $filename) (i32.const 1))))

  (local.set $stateFinal
    (call $malloc (i32.add (i32.mul (i32.load (local.get $context)) (i32.const 2)) (i32.const 2))))
  (memory.fill
    (local.get $stateFinal)
    (i32.const 4)
    (i32.add (i32.mul (i32.load (local.get $context)) (i32.const 2)) (i32.const 1)))

  ;; (call $day11.print (local.get $context) (local.get $state0))

  (local.set $state0 (call $set.singleton (global.get $fns.strcmp) (local.get $state0)))

  (i64.extend_i32_s
    (call $day11.bfs
      (local.get $context)
      (local.get $state0)
      (local.get $stateFinal)
      (i32.const 0)
      (local.get $state0))))

(elem (table $mains) (i32.const 11) $day11.main)
(func $day11.main (result i32)
  (i32.const 0)
  (i32.add
    (call $checkInputI64
      (i32.const 0x0b_8000)
      (i32.const 0x0b0) (i64.const 11)
      (i32.const 0x0b1) (i64.const -2)))
  (i32.add
    (call $checkInputI64
      (i32.const 0x0b_8020)
      (i32.const 0x0b0) (i64.const 37)
      (i32.const 0x0b1) (i64.const 61)))
  nop)
