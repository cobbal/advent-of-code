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

;; (import "env" "debugger" (func $debugger))

;; Each day gets its own page! (prelude gets page 0, start gets page 26)
(memory (export "memory") 27)

;; Each day gets 16 table entries
(table $fns 0x1a0 funcref)

;; TODO
;; (global $malloc.align i32 (i32.const 16))
(global $malloc.end (mut i32) (i32.const 0x1b_0000))

;; Currently no free
(func $malloc (param $size i32) (result i32)
  (local $result i32)
  (local $newCount i32)
  (local $pagesNeeded i32)
  (local.set $result (global.get $malloc.end))
  (local.set $newCount (i32.add (local.get $result) (local.get $size)))
  (local.set $pagesNeeded
    (i32.sub (i32.shr_u (i32.add (local.get $newCount) (i32.const 0xffff)) (i32.const 16))
        (memory.size)))
  (if (i32.gt_s (local.get $pagesNeeded) (i32.const 0))
      (then (call $assert (i32.ge_s (memory.grow (local.get $pagesNeeded)) (i32.const 0)))))
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
   (then unreachable)))

(func $strlen (param $string i32) (result i32)
  (local $ptr i32)
  (local.set $ptr (local.get $string))
  (loop $loop
    (if (i32.ne (i32.load8_u (local.get $ptr)) (i32.const 0))
      (then 
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
        (br $loop))))
  (i32.sub (local.get $ptr) (local.get $string)))

(func $memmove (param $destination i32) (param $source i32) (param $num i32)
  (loop $loop
    (if (local.get $num)
      (then
        (i32.store8 (local.get $destination) (i32.load8_u (local.get $source)))
        (local.set $source (i32.add (local.get $source) (i32.const 1)))
        (local.set $destination (i32.add (local.get $destination) (i32.const 1)))
        (local.set $num (i32.sub (local.get $num) (i32.const 1)))
        (br $loop)))))

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
  (select (i32.const 1) (i32.const -1)
    (i32.lt_u (local.get $c0) (local.get $c1))))

(func $strdup (param $s i32) (result i32)
  (local $len i32)
  (local $buf i32)
  (local.set $len (call $strlen (local.get $s)))
  (local.set $buf (call $malloc (i32.add (local.get $len) (i32.const 1))))
  (call $memmove (local.get $buf) (local.get $s) (local.get $len))
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

(func $print.nl
  (i32.store (i32.const 0x108) (i32.const 0xa))
  (call $printStr (i32.const 0x108)))

(func $printI32.nl (param $i i32)
  (call $printI64 (i64.extend_i32_s (local.get $i)))
  (call $print.nl))

(func $printI32 (param $i i32)
  (call $printI64 (i64.extend_i32_s (local.get $i))))

(func $printI64.nl (param $i i64)
  (call $printI64 (local.get $i))
  (call $print.nl))

(func $printI64 (param $i i64) 
  (call $formatI64._impl (local.get $i) (i32.const 0x100))
  (i32.store (i32.const 0x120) (i32.const 0x100))
  (i32.store (i32.const 0x124) (i32.const 20))
  (call $assert_not
    (call $fd_write
      (i32.const 1)
      (i32.const 0x120)
      (i32.const 1)
      (i32.const 0x8))))

(func $formatI64 (param $i i64) (result i32)
  (local $buf i32)
  (call $formatI64._impl (local.get $i)
    (local.tee $buf (call $malloc (i32.const 21))))
  (local.get $buf))

(func $formatI32 (param $i i32) (result i32)
  (call $formatI64 (i64.extend_i32_s (local.get $i))))

;; $buf should have 21 bytes of space
(func $formatI64._impl (param $i i64) (param $buf i32)
  (local $neg i32)
  (local $pos i32)
  (local $ptr i32)
  (local $digit i32)

  (if (local.tee $neg (i64.lt_s (local.get $i) (i64.const 0)))
    (then (local.set $i (i64.sub (i64.const 0) (local.get $i)))))

  (memory.fill (local.get $buf) (i32.const 0x20 (;' ';)) (i32.const 20))
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


(func $printStr.nl (param $i i32)
  (call $printStr (local.get $i))
  (call $print.nl))

(func $printStr (param $string i32)
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

(func $getClock (result i64)
  (call $assert_not
    (call $clock_time_get
      (i32.const 1)
      (i64.const 0)
      (i32.const 0x100)))
  (i64.load (i32.const 0x100)))
