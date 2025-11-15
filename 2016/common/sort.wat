(func $sort.i32.lt_s (param $elements i32) (param $len i32)
  (return_call $sort.i32 (global.get $fns.i32.cmp_s) (local.get $elements) (local.get $len)))

(func $sort.i32.lt_u (param $elements i32) (param $len i32)
  (return_call $sort.i32 (global.get $fns.i32.cmp_u) (local.get $elements) (local.get $len)))

;; based on https://en.wikipedia.org/w/index.php?title=Heapsort&oldid=1318342787#Standard_implementation
;; procedure heapsort(a, count) is
;;     input: an unordered array a of length count
(func $sort.i32 (param $cmp i32) (param $elements i32) (param $len i32)
  (local $start i32)
  (local $end i32)
  (local $tmp0 i32)
  (local $tmp1 i32)
  (local $p0 i32)
  (local $p1 i32)
  (local $root i32)
  (local $root.leftChild i32)
  (local $child i32)

  (local.set $start (i32.shr_u (local.get $len) (i32.const 1)))
  ;; start ← floor(count/2)
  (local.set $end (local.get $len))
  ;; end ← count

  ;; while end > 1 do
  (loop $outer
    (if (local.get $end)
      (then
      ;; if start > 0 then    (Heap construction)
        (if (local.get $start)
          ;; start ← start − 1
          (then (local.set $start (i32.sub (local.get $start) (i32.const 1))))
          ;; else                 (Heap extraction)
          (else
          ;; end ← end − 1
            (local.set $end (i32.sub (local.get $end) (i32.const 1)))
            ;; swap(a[end], a[0])
            (local.set $tmp0 (i32.load (local.get $elements)))
            (i32.store (local.get $elements)
              (i32.load
                (local.tee $p0
                  (i32.add (local.get $elements)
                    (i32.shl (local.get $end) (i32.const 2))))))
            (i32.store (local.get $p0) (local.get $tmp0))))

        ;; (The following is siftDown(a, start, end))
        ;; root ← start
        (local.set $root (local.get $start))
        ;; while iLeftChild(root) < end do
        (loop $inner
          (local.set $root.leftChild (i32.add (i32.shl (local.get $root) (i32.const 1)) (i32.const 1)))
          (if (i32.lt_u (local.get $root.leftChild) (local.get $end))
            (then
            ;; child ← iLeftChild(root)
            (local.set $child (local.get $root.leftChild))
            ;; (If there is a right child and that child is greater)
            ;; if child+1 < end and a[child] < a[child+1] then
            (if (i32.lt_u (i32.add (local.get $child) (i32.const 1)) (local.get $end))
              (then
                (if
                  (i32.lt_s
                    (call_indirect $fns (param i32 i32) (result i32)
                      (i32.load
                        (i32.add
                          (local.get $elements) (i32.shl (local.get $child) (i32.const 2))))
                      (i32.load
                        (i32.add
                          (local.get $elements)
                          (i32.shl (i32.add (local.get $child) (i32.const 1)) (i32.const 2))))
                      (local.get $cmp))
                    (i32.const 0))
                  (then
                    ;; child ← child + 1
                    (local.set $child (i32.add (local.get $child) (i32.const 1)))))))

            ;; if a[root] < a[child] then
            (if
              (i32.lt_s
                (call_indirect $fns (param i32 i32) (result i32)
                  (local.tee $tmp0
                    (i32.load
                      (local.tee $p0
                        (i32.add
                          (local.get $elements) (i32.shl (local.get $root) (i32.const 2))))))
                  (local.tee $tmp1
                    (i32.load
                      (local.tee $p1
                        (i32.add
                          (local.get $elements) (i32.shl (local.get $child) (i32.const 2))))))
                  (local.get $cmp))
                (i32.const 0))
              (then
                ;; swap(a[root], a[child])
                (i32.store (local.get $p0) (local.get $tmp1))
                (i32.store (local.get $p1) (local.get $tmp0))
                ;; root ← child         (repeat to continue sifting down the child now)
                (local.set $root (local.get $child))
                (br $inner))
              ;; else
              ;;     break                (return to outer loop)
            ))))
        (br $outer)))))
