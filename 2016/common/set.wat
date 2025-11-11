(func $set.empty (param $cmp i32) (result i32)
  (return_call $map.empty (local.get $cmp)))

(func $set.singleton (param $cmp i32) (param $x i32) (result i32)
  (return_call $set.insert (call $set.empty (local.get $cmp)) (local.get $x)))

(func $set.insert (param $set i32) (param $x i32) (result i32)
  (drop (drop (call $map.insert (local.get $set) (local.get $x) (i32.const 0)))))

(func $set.contains (param $set i32) (param $x i32) (result i32)
  (drop (call $map.lookup (local.get $set) (local.get $x))))

(func $set.count (param $set i32) (result i32)
  (return_call $map.count (local.get $set)))

(func $set.min (param $set i32) (result i32 i32)
  (drop (call $map.min (local.get $set))))

(func $set.max (param $set i32) (result i32 i32)
  (drop (call $map.max (local.get $set))))
