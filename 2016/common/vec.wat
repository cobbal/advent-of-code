;; struct vec {
;;   count: i32;
;;   capacity: i32;
;;   elements: i32*;
;; }

(func $vec.create (result i32)
  (local $result i32)
  (local.set $result (call $malloc (i32.const 76)))
  (i32.store (i32.add (local.get $result) (i32.const 4)) (i32.const 16))
  (i32.store
    (i32.add (local.get $result) (i32.const 8))
    (i32.add (local.get $result) (i32.const 12)))
  (local.get $result))

(func $vec.push (param $vec i32) (param $elem i32)
  (local $count i32)
  (local $capacity i32)
  (local $storage i32)
  (local $newStorage i32)

  (local.set $count (i32.load (local.get $vec)))
  (local.set $capacity (i32.load (i32.add (local.get $vec) (i32.const 4))))
  (local.set $storage (i32.load (i32.add (local.get $storage) (i32.const 8))))

  (if (i32.eq (local.get $count) (local.get $capacity))
    (then
      (local.set $capacity
        (i32.add (local.get $capacity) (i32.shl (local.get $capacity) (i32.const 1))))
      (local.set $newStorage (call $malloc (i32.shl (local.get $capacity) (i32.const 2))))
      (memory.copy
        (local.get $newStorage)
        (local.get $storage)
        (i32.shl (local.get $count) (i32.const 2)))
      (i32.store
        (i32.add (local.get $vec) (i32.const 8))
        (local.get $newStorage))))

  (i32.store
    (i32.add
      (local.get $storage)
      (i32.shl (local.get $count) (i32.const 2)))
    (local.get $elem))
  (i32.store
    (i32.add (local.get $vec) (i32.const 4))
    (i32.add (local.get $count) (i32.const 1))))
