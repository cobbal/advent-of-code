(type $i64Solver (func (param i32) (result i64)))

(func $dayUtils.printGoodBad (param $isGood i32)
  (if (local.get $isGood)
    (then (call $printStr.small (i64.const 0x00_64_6f_6f_67_20_20_20 (;'   good\0';))))
    (else (call $printStr.small (i64.const 0x00_64_61_62_20_20_20_20 (;'    bad\0';))))))

;; int checkInputInt(char *path, partSolverInt part0, int64_t expected0, partSolverInt part1, int64_t expected1) {
(func $checkInputI64
  (param $path i32)
  (param $solver0 i32)
  (param $expected0 i64)
  (param $solver1 i32)
  (param $expected1 i64)
  (result i32)

  (local $len i32)
  (local $result0 i64)
  (local $result1 i64)
  (local $correct0 i32)
  (local $correct1 i32)

  ;; printf("%22s: ", path);
  (memory.fill (i32.const 0x200) (i32.const 0x20) (i32.const 0x16))
  (local.set $len (call $i32.min (i32.const 0x16) (call $strlen (local.get $path))))
  (memory.copy
    (i32.sub (i32.const 0x216) (local.get $len))
    (local.get $path)
    (local.get $len))
  (call $printStr (i32.const 0x200))
  (call $printStr.small (i64.const 0x203a (;': ';)))

  ;; int64_t result0 = part0(arena, f);
  (local.set $result0
    (call_indirect (param i32) (result i64)
      (local.get $path)
      (local.get $solver0)))
  ;; printf("%20lld ", result0);
  (call $printI64 (local.get $result0))
  (call $memoryReset)

  ;; int64_t result1 = part1(arena, f);
  (local.set $result1
    (call_indirect (param i32) (result i64)
      (local.get $path)
      (local.get $solver1)))
  ;; printf("%20lld ", result1);
  (call $printI64 (local.get $result1))
  (call $memoryReset)

  ;; printf(part0Correct ? "  good" : "   bad");
  (call $dayUtils.printGoodBad
    (local.tee $correct0 (i64.eq (local.get $result0) (local.get $expected0))))
  ;; printf(part1Correct ? "  good\n" : "   bad\n");
  (call $dayUtils.printGoodBad
    (local.tee $correct1 (i64.eq (local.get $result1) (local.get $expected1))))
  (call $print.nl)
  ;; return !(part0Correct && part1Correct);
  (i32.eqz (i32.and (local.get $correct0) (local.get $correct1))))

(func $checkInputStr
  (param $path i32)
  (param $solver0 i32)
  (param $expected0 i32)
  (param $solver1 i32)
  (param $expected1 i32)
  (result i32)

  (local $len i32)
  (local $result0 i32)
  (local $result1 i32)
  (local $correct0 i32)
  (local $correct1 i32)

  ;; printf("%22s: ", path);
  (memory.fill (i32.const 0x200) (i32.const 0x20) (i32.const 0x16))
  (local.set $len (call $i32.min (i32.const 0x16) (call $strlen (local.get $path))))
  (memory.copy
    (i32.sub (i32.const 0x216) (local.get $len))
    (local.get $path)
    (local.get $len))
  (call $printStr (i32.const 0x200))
  (call $printStr.small (i64.const 0x203a (;': ';)))

  ;; int64_t result0 = part0(arena, f);
  (local.set $result0
    (call_indirect (param i32) (result i32)
      (local.get $path)
      (local.get $solver0)))
  ;; printf("%20lld ", result0);
  (call $printStr (local.get $result0))
  (call $printStr.small (i64.const 0x20))
  (if (i32.eqz (i32.or (i32.eqz (local.get $result0)) (i32.eqz (local.get $expected0))))
    (then
      (local.set $correct0
        (i32.eqz (call $strcmp (local.get $result0) (local.get $expected0))))))
  (call $memoryReset)

  ;; int64_t result1 = part1(arena, f);
  (local.set $result1
    (call_indirect (param i32) (result i32)
      (local.get $path)
      (local.get $solver1)))
  ;; printf("%20lld ", result1);
  (call $printStr (local.get $result1))
  (if (i32.eqz (i32.or (i32.eqz (local.get $result1)) (i32.eqz (local.get $expected1))))
    (then
      (local.set $correct1
        (i32.eqz (call $strcmp (local.get $result1) (local.get $expected1))))))
  (call $memoryReset)

  ;; printf(part0Correct ? "  good" : "   bad");
  (call $dayUtils.printGoodBad (local.get $correct0))
  ;; printf(part1Correct ? "  good\n" : "   bad\n");
  (call $dayUtils.printGoodBad (local.get $correct1))
  (call $print.nl)
  ;; return !(part0Correct && part1Correct);
  (i32.eqz (i32.and (local.get $correct0) (local.get $correct1))))
