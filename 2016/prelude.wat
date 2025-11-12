;; External dependencies
;; https://github.com/WebAssembly/WASI/blob/main/legacy/preview1/docs.md
;; example implementation: https://github.com/nodejs/uvwasi/blob/v0.0.23/src/uvwasi.c
(import "wasi_snapshot_preview1" "clock_time_get"
  (func $clock_time_get
    (param $clockid i32)
    (param $precision i64)
    (param $out_result i32)
    (result i32)))
(import "wasi_snapshot_preview1" "fd_close"
  (func $fd_close
    (param $fd i32)
    (result i32)))
(import "wasi_snapshot_preview1" "fd_read"
  (func $fd_read
    (param $fd i32)
    (param $iovs.0 i32)
    (param $iovs.1 i32)
    (param $out_result i32)
    (result i32)))
(import "wasi_snapshot_preview1" "fd_seek"
  (func $fd_seek
    (param $fd i32)
    (param $offset i64)
    (param $whence i32)
    (param $out_result i32)
    (result i32)))
(import "wasi_snapshot_preview1" "fd_write"
  (func $fd_write
    (param $fd i32)
    (param $iovs.0 i32)
    (param $iovs.1 i32)
    (param $out_result i32)
    (result i32)))
(import "wasi_snapshot_preview1" "path_open"
  (func $path_open
    (param $fd i32)
    (param $dirflags i32)
    (param $path.0 i32)
    (param $path.1 i32)
    (param $oflags i32)
    (param $fs_rights_base i64)
    (param $fs_rights_inheriting i64)
    (param $fdflags i32)
    (param $out_result i32)
    (result i32)))
(import "wasi_snapshot_preview1" "proc_exit"
  (func $proc_exit
    (param $rval i32)))
;; This function is abused for a debugger hook
(import "wasi_snapshot_preview1" "sched_yield"
  (func $sched_yield (result i32)))

;; Each day gets its own page! (prelude gets page 0, start gets page 26)
(memory (export "memory") 27)

;; Each day gets 16 table entries
(table $fns 0x1a0 funcref)

;; Some useful, common function pointers
(func $_fns.i32.cmp (param $a i32) (param $b i32) (result i32)
  (i32.sub (local.get $b) (local.get $a)))

(global $fns.i32.cmp i32 (i32.const 0))
(elem (table $fns) (i32.const 0) $_fns.i32.cmp)

(global $fns.strcmp i32 (i32.const 1))
(elem (table $fns) (i32.const 1) $strcmp)

;; must be a power of 2
(global $malloc.align i32 (i32.const 8))
(global $malloc.end (mut i32) (i32.const 0x1b_0000))

;; Currently no free
(func $malloc (param $size i32) (result i32)
  (local $result i32)
  (local $newCount i32)
  (local $pagesNeeded i32)

  ;; round size up to alignment
  (local.set $size
    (i32.and
      (i32.add (local.get $size) (i32.sub (global.get $malloc.align) (i32.const 1)))
      (i32.sub (i32.const 0) (global.get $malloc.align))))

  (local.set $result (global.get $malloc.end))
  (local.set $newCount (i32.add (local.get $result) (local.get $size)))
  (local.set $pagesNeeded
    (i32.sub (i32.shr_u (i32.add (local.get $newCount) (i32.const 0xffff)) (i32.const 16))
      (memory.size)))
  (if (i32.gt_s (local.get $pagesNeeded) (i32.const 0))
    (then
      (call $assert (i32.ge_s (memory.grow (local.get $pagesNeeded)) (i32.const 0)))))
  (global.set $malloc.end (local.get $newCount))
  (local.get $result))

(func $memoryReset
  (global.set $malloc.end (i32.const 0x1b_0000))
  (memory.fill
    (global.get $malloc.end)
    (i32.const 0)
    (i32.sub
      (i32.mul (memory.size) (i32.const 0x1_0000))
      (global.get $malloc.end))))

(func $assert_not (param $in i32)
  (if (i32.ne (local.get $in) (i32.const 0))
    (then
      (call $proc_exit (local.get $in))
      (unreachable))))

(func $assert (param $in i32)
  (if (i32.eqz (local.get $in))
    (then
      (call $debugger)
      unreachable)))

(func $strlen (param $string i32) (result i32)
  (local $ptr i32)
  (local.set $ptr (local.get $string))
  (loop $loop
    (if (i32.ne (i32.load8_u (local.get $ptr)) (i32.const 0))
      (then
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
        (br $loop))))
  (i32.sub (local.get $ptr) (local.get $string)))

(func $strcmp (param $s0 i32) (param $s1 i32) (result i32)
  (local $c0 i32)
  (local $c1 i32)
  (loop $loop
    (if
      (i32.eq
        (local.tee $c0 (i32.load8_u (local.get $s0)))
        (local.tee $c1 (i32.load8_u (local.get $s1))))
      (then
        (if (local.get $c0)
          (then
            (local.set $s0 (i32.add (local.get $s0) (i32.const 1)))
            (local.set $s1 (i32.add (local.get $s1) (i32.const 1)))
            (br $loop)))
        (return (i32.const 0)))))
  (i32.sub (local.get $c0) (local.get $c1)))

(func $memcmp (param $p0 i32) (param $p1 i32) (param $len i32) (result i32)
  (local $c0 i32)
  (local $c1 i32)
  (loop $loop
    (if (i32.eqz (local.get $len))
      (then (return (i32.const 0))))
    (if
      (i32.eq
        (local.tee $c0 (i32.load8_u (local.get $p0)))
        (local.tee $c1 (i32.load8_u (local.get $p1))))
      (then
        (local.set $p0 (i32.add (local.get $p0) (i32.const 1)))
        (local.set $p1 (i32.add (local.get $p1) (i32.const 1)))
        (local.set $len (i32.sub (local.get $len) (i32.const 1)))
        (br $loop))))
  (select (i32.const 1) (i32.const -1)
    (i32.lt_u (local.get $c0) (local.get $c1))))

(func $strdup (param $s i32) (result i32)
  (local $len i32)
  (local $buf i32)
  (local.set $len (call $strlen (local.get $s)))
  (local.set $buf (call $malloc (i32.add (local.get $len) (i32.const 1))))
  (memory.copy (local.get $buf) (local.get $s) (local.get $len))
  (local.get $buf))

(func $strrchr (param $str i32) (param $c i32) (result i32)
  (local $ptr i32)
  (local.set $ptr (i32.add (local.get $str) (call $strlen (local.get $str))))
  (loop $loop
    (if (i32.lt_u (local.get $ptr) (local.get $str))
      (then (return (i32.const 0))))
    (if (i32.ne (i32.load8_u (local.get $ptr)) (local.get $c))
      (then
        (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
        (br $loop))))
  (local.get $ptr))

(func $strchr (param $str i32) (param $c i32) (result i32)
  (local $d i32)
  (loop $loop
    (if (local.tee $d (i32.load8_u (local.get $str)))
      (then
        (if (i32.eq (local.get $d) (local.get $c))
          (then (return (local.get $str))))
        (local.set $str (i32.add (local.get $str) (i32.const 1)))
        (br $loop))))
  (i32.const 0))

(func $print.nl
  (i32.store (i32.const 0x108) (i32.const 0xa))
  (call $printStr (i32.const 0x108)))

(func $print.nl.indent (param $indent i32)
  (call $print.nl)
  (i32.store (i32.const 0x108) (i32.const 0x20))
  (loop $loop
    (if (i32.gt_s (local.get $indent) (i32.const 0))
      (then
        (call $printStr (i32.const 0x108))
        (local.set $indent (i32.sub (local.get $indent) (i32.const 1)))
        (br $loop)))))

(func $printI32.nl (param $i i32)
  (call $printI64 (i64.extend_i32_s (local.get $i)))
  (call $print.nl))

(func $printI32 (param $i i32)
  (call $printI64 (i64.extend_i32_s (local.get $i))))

(func $printI64.nl (param $i i64)
  (call $printI64 (local.get $i))
  (call $print.nl))

(func $printI64 (param $i i64)
  (call $formatI64._impl (local.get $i) (i32.const 0x200))
  (call $printStr (i32.const 0x200)))

(func $printI32.hex.nl (param $i i32)
  (call $formatI64.hex._impl (i64.extend_i32_s (local.get $i)) (i32.const 0x200))
  (call $printStr.nl (i32.const 0x208)))

(func $printI32.hex (param $i i32)
  (call $formatI64.hex._impl (i64.extend_i32_s (local.get $i)) (i32.const 0x200))
  (call $printStr (i32.const 0x208)))

(func $formatI64 (param $i i64) (result i32)
  (local $buf i32)
  (call $formatI64._impl (local.get $i)
    (local.tee $buf (call $malloc (i32.const 21))))
  (local.get $buf))

(func $formatI64.hex (param $i i64) (result i32)
  (local $buf i32)
  (call $formatI64.hex._impl (local.get $i)
    (local.tee $buf (call $malloc (i32.const 17))))
  (local.get $buf))

(func $formatI32 (param $i i32) (result i32)
  (call $formatI64 (i64.extend_i32_s (local.get $i))))

(func $asciiHex (param $i i32) (result i32)
  (call $assert (i32.lt_u (local.get $i) (i32.const 0x10)))
  (if (i32.lt_u (local.get $i) (i32.const 10))
    (then (return (i32.add (i32.const 0x30) (local.get $i)))))
  (return (i32.add (i32.const 0x57) (local.get $i))))

;; $buf should have 21 bytes of space
(func $formatI64._impl (param $i i64) (param $buf i32)
  (local $neg i32)
  (local $pos i32)
  (local $ptr i32)
  (local $digit i32)

  (if (local.tee $neg (i64.lt_s (local.get $i) (i64.const 0)))
    (then (local.set $i (i64.sub (i64.const 0) (local.get $i)))))

  (memory.fill (local.get $buf) (i32.const 0x20 (;' ';)) (i32.const 20))
  (i32.store8 (i32.add (local.get $buf) (i32.const 20)) (i32.const 0))
  (local.set $ptr (i32.add (local.get $buf) (i32.const 19)))
  (loop $loop
    (local.set $digit (i32.wrap_i64 (i64.rem_u (local.get $i) (i64.const 10))))
    (local.set $i (i64.div_u (local.get $i) (i64.const 10)))
    (i32.store8 (local.get $ptr) (i32.add (i32.const 0x30 (;'0';)) (local.get $digit)))
    (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
    (if (i32.wrap_i64 (local.get $i))
      (then (br $loop))))
  (if (local.get $neg)
    (then (i32.store8 (local.get $ptr) (i32.const 0x2d (;'-';))))))

;; $buf should have 17 bytes of space
(func $formatI64.hex._impl (param $i i64) (param $buf i32)
  (local $pos i32)
  (local $ptr i32)
  (local $digit i32)

  (memory.fill (local.get $buf) (i32.const 0x30 (;'0';)) (i32.const 16))
  (i32.store8 (i32.add (local.get $buf) (i32.const 16)) (i32.const 0))
  (local.set $ptr (i32.add (local.get $buf) (i32.const 15)))
  (loop $loop
    (local.set $digit (i32.wrap_i64 (i64.rem_u (local.get $i) (i64.const 0x10))))
    (local.set $i (i64.div_u (local.get $i) (i64.const 0x10)))
    (i32.store8 (local.get $ptr) (call $asciiHex (local.get $digit)))
    (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
    (br_if $loop (i64.ne (local.get $i) (i64.const 0)))))

(func $formatI8.hex.into (param $x i32) (param $outPtr i32)
  (i32.store8
    (local.get $outPtr)
    (call $asciiHex (i32.and (i32.const 0xf) (i32.shr_u (local.get $x) (i32.const 4)))))
  (i32.store8
    (i32.add (local.get $outPtr) (i32.const 1))
    (call $asciiHex (i32.and (i32.const 0xf) (local.get $x)))))

(func $formatI32.le.hex.into (param $x i32) (param $outPtr i32)
  (call $formatI8.hex.into
    (i32.shr_u (local.get $x) (i32.const 0))
    (i32.add (local.get $outPtr) (i32.const 0)))
  (call $formatI8.hex.into
    (i32.shr_u (local.get $x) (i32.const 8))
    (i32.add (local.get $outPtr) (i32.const 2)))
  (call $formatI8.hex.into
    (i32.shr_u (local.get $x) (i32.const 16))
    (i32.add (local.get $outPtr) (i32.const 4)))
  (call $formatI8.hex.into
    (i32.shr_u (local.get $x) (i32.const 24))
    (i32.add (local.get $outPtr) (i32.const 6))))

(func $printStr.nl (param $i i32)
  (call $printStr (local.get $i))
  (call $print.nl))

(func $printStr (param $string i32)
  (if (i32.eqz (local.get $string))
    (then
      (local.set $string (i32.const 0x108))
      (i64.store (i32.const 0x108) (i64.const 0x0000_3e6c_6c75_6e3c))))
  (i32.store (i32.const 0x100) (local.get $string))
  (i32.store (i32.const 0x104) (call $strlen (local.get $string)))
  (call $assert_not
    (call $fd_write
      (i32.const 1)
      (i32.const 0x100)
      (i32.const 1)
      (i32.const 0x8))))

(func $countChar (param $string i32) (param $targetChar i32) (result i32)
  (local $count i32)
  (local $current i32)
  (loop $loop
    (if (local.tee $current (i32.load8_u (local.get $string)))
      (then
        (if (i32.eq (local.get $current) (local.get $targetChar))
          (then (local.set $count (i32.add (local.get $count) (i32.const 1)))))
        (local.set $string (i32.add (local.get $string) (i32.const 1)))
        (br $loop))))
  (local.get $count))

(func $splitDestructively (param $string i32) (param $sep i32) (param $allowEmpty i32) (result i32 i32)
  (local $strings i32)
  (local $workingString i32)
  (local $oldWork i32)
  (local $stringsPtr i32)
  (local $len i32)
  (local $c i32)

  (local.set $strings
    (call $malloc
      (i32.mul (i32.const 4)
        (i32.add (i32.const 1)
          (local.tee $len
            (i32.add (i32.const 1)
              (call $countChar (local.get $string) (local.get $sep))))))))
  (local.set $stringsPtr (local.get $strings))
  (local.set $workingString (local.get $string))

  (loop $loop
    (if (local.tee $c (i32.load8_u (local.get $string)))
      (then
        (if (i32.eq (local.get $c) (local.get $sep))
          (then
            (i32.store8 (local.get $string) (i32.const 0))
            (local.set $oldWork (local.get $workingString))
            (local.set $workingString (i32.add (local.get $string) (i32.const 1)))
            (block $record
              (if (i32.eq (local.get $string) (local.get $oldWork))
                (then
                  (if (i32.eqz (local.get $allowEmpty))
                    (then
                      (br $record)))))
              (i32.store (local.get $stringsPtr) (local.get $oldWork))
              (local.set $stringsPtr (i32.add (local.get $stringsPtr) (i32.const 4))))))
        (local.set $string (i32.add (local.get $string) (i32.const 1)))
        (br $loop))))

  (block $record
    (if (i32.eq (local.get $string) (local.get $workingString))
      (then
        (if (i32.eqz (local.get $allowEmpty))
          (then
            (br $record)))))
    (i32.store (local.get $stringsPtr) (local.get $workingString))
    (local.set $stringsPtr (i32.add (local.get $stringsPtr) (i32.const 4))))

  (local.set $len
    (i32.shr_u
      (i32.sub (local.get $stringsPtr) (local.get $strings))
      (i32.const 2)))

  (local.get $strings) (local.get $len))

;; string -> (string, size)
(func $readFile (param $path i32) (result i32 i32)
  (local $err i32)
  (local $fd i32)
  (local $len i32)
  (local $buffer i32)

  (call $assert_not
    (call $path_open
      (; $fd ;) (i32.const 3)
      (; $dirflags ;) (i32.const 0x1) ;; symlink_follow=1
      (; $path.0 ;) (local.get $path)
      (; $path.1 ;) (call $strlen (local.get $path))
      (; $oflags ;) (i32.const 0x0)
      (; $fs_rights_base ;) (i64.const 0x6) ;; fd_read | fd_seek
      (; $fs_rights_inheriting ;) (i64.const 0x6) ;; fd_read | fd_seek
      (; $fdflags ;) (i32.const 0x0)
      (; $out_result ;) (i32.const 8)))
  (local.set $fd (i32.load (i32.const 8)))

  (call $assert_not
    (call $fd_seek
      (; $fd ;) (local.get $fd)
      (; $offset ;) (i64.const 0)
      (; $whence ;) (i32.const 2) ;; end
      (; $out_result ;) (i32.const 8)))
  (local.set $len (i32.load (i32.const 8)))

  ;; check that file is reasonable
  (call $assert (i64.lt_u (i64.load (i32.const 8)) (i64.const 0x8000_0000)))

  ;; Return to start
  (call $assert_not
    (call $fd_seek
      (; $fd ;) (local.get $fd)
      (; $offset ;) (i64.const 0)
      (; $whence ;) (i32.const 0) ;; set
      (; $out_result ;) (i32.const 8)))

  ;; allocate space
  (local.set $buffer
    (call $malloc (i32.add (i32.const 1) (local.get $len))))

  ;; read file
  (i32.store (i32.const 8) (local.get $buffer))
  (i32.store (i32.const 12) (local.get $len))
  (call $assert_not
    (call $fd_read
      (; $fd ;) (local.get $fd)
      (; $iovs.0 ;) (i32.const 8)
      (; $iovs.1 ;) (i32.const 1)
      (; $out_result ;) (i32.const 16)))
  (call $assert (i32.eq (i32.load (i32.const 16)) (local.get $len)))

  (call $assert_not (call $fd_close (local.get $fd)))

  (local.get $buffer) (local.get $len))

(func $parseI32 (param $string i32) (result i32)
  (local $result i32)
  (local $c i32)
  (local $digit i32)
  (local $negative i32)

  (if (i32.eq (i32.load8_u (local.get $string)) (i32.const 0x2d (;'-';)))
    (then
      (local.set $negative (i32.const 1))
      (local.set $string (i32.add (local.get $string) (i32.const 1)))))

  (loop $loop
    (if (local.tee $c (i32.load8_u (local.get $string)))
      (then
        (if (i32.lt_u (local.tee $digit (i32.sub (local.get $c) (i32.const 0x30))) (i32.const 10))
          (then
            (local.set $result
              (i32.add
                (i32.mul (local.get $result) (i32.const 10))
                (local.get $digit)))
            (local.set $string (i32.add (local.get $string) (i32.const 1)))
            (br $loop))))))

  (if (local.get $negative)
    (then (local.set $result (i32.sub (i32.const 0) (local.get $result)))))
  (local.get $result))

(func $i64.abs (param $i i64) (result i64)
  (if (i64.lt_s (local.get $i) (i64.const 0))
    (then (local.set $i (i64.sub (i64.const 0) (local.get $i)))))
  (local.get $i))

(func $i32.abs (param $i i32) (result i32)
  (if (i32.lt_s (local.get $i) (i32.const 0))
    (then (local.set $i (i32.sub (i32.const 0) (local.get $i)))))
  (local.get $i))

(func $i32.max (param $a i32) (param $b i32) (result i32)
  (if (i32.gt_s (local.get $b) (local.get $a))
    (then (local.set $a (local.get $b))))
  (local.get $a))

(func $i32.min (param $a i32) (param $b i32) (result i32)
  (if (i32.lt_s (local.get $b) (local.get $a))
    (then (local.set $a (local.get $b))))
  (local.get $a))

(func $i32.mod_s (param $a i32) (param $b i32) (result i32)
  (i32.rem_s
    (i32.add
      (i32.rem_s
        (local.get $a)
        (local.get $b))
      (local.get $b))
    (local.get $b)))

(func $getClock (result i64)
  (call $assert_not
    (call $clock_time_get
      (i32.const 1)
      (i64.const 0)
      (i32.const 0x100)))
  (i64.load (i32.const 0x100)))

(func $debugger
  (drop (call $sched_yield)))

(func $cons (param $car i32) (param $cdr i32) (result i32)
  (local $result i32)
  (local.set $result (call $malloc (i32.const 8)))
  (i32.store (local.get $result) (local.get $car))
  (i32.store (i32.add (local.get $result) (i32.const 4)) (local.get $cdr))
  (local.get $result))

(func $cons.3 (param $car i32) (param $cdr i32) (param $cgr i32) (result i32)
  (local $result i32)
  (local.set $result (call $malloc (i32.const 12)))
  (i32.store (local.get $result) (local.get $car))
  (i32.store (i32.add (local.get $result) (i32.const 4)) (local.get $cdr))
  (i32.store (i32.add (local.get $result) (i32.const 8)) (local.get $cgr))
  (local.get $result))

(func $cons.4 (param $car i32) (param $cdr i32) (param $cgr i32) (param $cjr i32) (result i32)
  (local $result i32)
  (local.set $result (call $malloc (i32.const 16)))
  (i32.store (local.get $result) (local.get $car))
  (i32.store (i32.add (local.get $result) (i32.const 4)) (local.get $cdr))
  (i32.store (i32.add (local.get $result) (i32.const 8)) (local.get $cgr))
  (i32.store (i32.add (local.get $result) (i32.const 12)) (local.get $cjr))
  (local.get $result))

(func $car (param $cell i32) (result i32)
  (i32.load (local.get $cell)))

(func $cdr (param $cell i32) (result i32)
  (i32.load (i32.add (local.get $cell) (i32.const 4))))
(func $cgr (param $cell i32) (result i32)
  (i32.load (i32.add (local.get $cell) (i32.const 8))))

(func $setCar (param $cell i32) (param $newCar i32)
  (i32.store (local.get $cell) (local.get $newCar)))

(func $setCdr (param $cell i32) (param $newCdr i32)
  (i32.store (i32.add (local.get $cell) (i32.const 4)) (local.get $newCdr)))

(func $setCgr (param $cell i32) (param $newCgr i32)
  (i32.store (i32.add (local.get $cell) (i32.const 8)) (local.get $newCgr)))

(func $uncons (param $cell i32) (result i32 i32)
  (i32.load (local.get $cell))
  (i32.load (i32.add (local.get $cell) (i32.const 4))))

(func $uncons.3 (param $cell i32) (result i32 i32 i32)
  (i32.load (local.get $cell))
  (i32.load (i32.add (local.get $cell) (i32.const 4)))
  (i32.load (i32.add (local.get $cell) (i32.const 8))))

;; returns new list and index of string in that list
(func $stringList.addToList (param $list i32) (param $str i32) (result i32 i32)
  (local $i i32)

  ;; end of list, add to end
  (if (i32.eqz (local.get $list))
    (then (return (call $cons (local.get $str) (i32.const 0)) (i32.const 0))))

  ;; found string, return index
  (if (i32.eqz (call $strcmp (call $car (local.get $list)) (local.get $str)))
    (then (return (local.get $list) (i32.const 0))))

  ;; not found, recur
  (call $cons
    (call $car (local.get $list))
    (local.set $i
      (call $stringList.addToList (call $cdr (local.get $list)) (local.get $str))))
  (i32.add (local.get $i) (i32.const 1)))

(func $list.len (param $list i32) (result i32)
  (local $i i32)
  (loop $loop
    (if (local.get $list)
      (then
        (local.set $list (call $cdr (local.get $list)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop))))
  (local.get $i))

(func $list.nth (param $list i32) (param $i i32) (result i32)
  (loop $loop
    (call $assert (local.get $list))
    (if (local.get $i)
      (then
        (local.set $list (call $cdr (local.get $list)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $loop))))
  (call $car (local.get $list)))

(func $swap (param $a i32) (param $b i32) (result i32 i32)
  (local.get $b) (local.get $a))

(func $u32.digitCount (param $i i32) (result i32)
  (if (i32.lt_u (local.get $i) (i32.const 10))
    (then (return (i32.const 1))))
  (if (i32.lt_u (local.get $i) (i32.const 100))
    (then (return (i32.const 2))))
  (if (i32.lt_u (local.get $i) (i32.const 1_000))
    (then (return (i32.const 3))))
  (if (i32.lt_u (local.get $i) (i32.const 10_000))
    (then (return (i32.const 4))))
  (if (i32.lt_u (local.get $i) (i32.const 100_000))
    (then (return (i32.const 5))))
  (if (i32.lt_u (local.get $i) (i32.const 1_000_000))
    (then (return (i32.const 6))))
  (if (i32.lt_u (local.get $i) (i32.const 10_000_000))
    (then (return (i32.const 7))))
  (if (i32.lt_u (local.get $i) (i32.const 100_000_000))
    (then (return (i32.const 8))))
  (if (i32.lt_u (local.get $i) (i32.const 1_000_000_000))
    (then (return (i32.const 9))))
  (i32.const 10))
