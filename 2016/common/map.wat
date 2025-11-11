;; enum { R = 0, B = 1 } Color;
;; struct Node {
;;     color: i32
;;     left: Node *
;;     kv: i64
;;     right: Node *
;; }

(func $map._create
  (param $color i32) (param $left i32) (param $kv i64) (param $right i32)
  (result i32)

  (local $tree i32)
  (local.set $tree (call $malloc (i32.const 20)))
  (i32.store (i32.add (local.get $tree) (i32.const 0)) (local.get $color))
  (i32.store (i32.add (local.get $tree) (i32.const 4)) (local.get $left))
  (i64.store (i32.add (local.get $tree) (i32.const 8)) (local.get $kv))
  (i32.store (i32.add (local.get $tree) (i32.const 16)) (local.get $right))
  (local.get $tree))

;; struct Map {
;;     count: i32
;;     cmp: (i32, i32) -> (i32)
;;     tree: Node *
;; }

;; static Tree lbalance(Arena arena, Color color, CTree left, GKeyValue kv, CTree right) {
(func $map._lbalance
  (param $color i32) (param $left i32) (param $kv i64) (param $right i32)
  (result i32)

  (local $a i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)

  (local $x i64)
  (local $y i64)
  (local $z i64)

  (local $redLeft i32)
  (local $leftLeft i32)
  (local $leftRight i32)

  ;; bool redLeft = color == B && left && left->color == R;
  (if (local.get $color)
    (then
      (if (local.get $left)
        (then (local.set $redLeft (i32.eqz (i32.load (local.get $left))))))))

  ;; if (redLeft && left->left && left->left->color == R) {
  (block $ifBlock
    (if (local.get $redLeft)
      (then
        (if (local.tee $leftLeft (i32.load (i32.add (local.get $left) (i32.const 4))))
          (then
            (if (i32.eqz (i32.load (local.get $leftLeft)))
              (then
                ;; a = left->left->left;
                (local.set $a (i32.load (i32.add (local.get $leftLeft) (i32.const 4))))
                ;; x = left->left->kv;
                (local.set $x (i64.load (i32.add (local.get $leftLeft) (i32.const 8))))
                ;; b = left->left->right;
                (local.set $b (i32.load (i32.add (local.get $leftLeft) (i32.const 16))))
                ;; y = left->kv;
                (local.set $y (i64.load (i32.add (local.get $left) (i32.const 8))))
                ;; c = left->right;
                (local.set $c (i32.load (i32.add (local.get $left) (i32.const 16))))
                ;; z = kv;
                (local.set $z (local.get $kv))
                ;; d = right;
                (local.set $d (local.get $right))
                (br $ifBlock)))))))

    ;; } else if (redLeft && left->right && left->right->color == R) {
    (if (local.get $redLeft)
      (then
        (if (local.tee $leftRight (i32.load (i32.add (local.get $left) (i32.const 16))))
          (then
            (if (i32.eqz (i32.load (local.get $leftRight)))
              (then
                ;; a = left->left;
                (local.set $a (i32.load (i32.add (local.get $left) (i32.const 4))))
                ;; x = left->kv;
                (local.set $x (i64.load (i32.add (local.get $left) (i32.const 8))))
                ;; b = left->right->left;
                (local.set $b (i32.load (i32.add (local.get $leftRight) (i32.const 4))))
                ;; y = left->right->kv;
                (local.set $y (i64.load (i32.add (local.get $leftRight) (i32.const 8))))
                ;; c = left->right->right;
                (local.set $c (i32.load (i32.add (local.get $leftRight) (i32.const 16))))
                ;; z = kv;
                (local.set $z (local.get $kv))
                ;; d = right;
                (local.set $d (local.get $right))
                (br $ifBlock)))))))
    ;; } else { return treeCreate(arena, color, left, kv, right); }
    (return (call $map._create (local.get $color) (local.get $left) (local.get $kv) (local.get $right))))

  ;; Tree newLeft = treeCreate(arena, B, a, x, b);
  ;; Tree newRight = treeCreate(arena, B, c, z, d);
  ;; return treeCreate(arena, R, newLeft, y, newRight);
  (call $map._create
    (i32.const 0)
    (call $map._create (i32.const 1) (local.get $a) (local.get $x) (local.get $b))
    (local.get $y)
    (call $map._create (i32.const 1) (local.get $c) (local.get $z) (local.get $d))))


;; static Tree rbalance(Arena arena, Color color, CTree left, GKeyValue kv, CTree right) {
(func $map._rbalance
  (param $color i32) (param $left i32) (param $kv i64) (param $right i32)
  (result i32)

  (local $a i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)

  (local $x i64)
  (local $y i64)
  (local $z i64)

  (local $redRight i32)
  (local $rightLeft i32)
  (local $rightRight i32)

  ;; bool redRight = color == B && right && right->color == R;
  (if (local.get $color)
    (then
      (if (local.get $right)
        (then (local.set $redRight (i32.eqz (i32.load (local.get $right))))))))

  ;; if (redRight && right->left && right->left->color == R) {
  (block $ifBlock
    (if (local.get $redRight)
      (then
        (if (local.tee $rightLeft (i32.load (i32.add (local.get $right) (i32.const 4))))
          (then
            (if (i32.eqz (i32.load (local.get $rightLeft)))
              (then
                ;; a = left;
                (local.set $a (local.get $left))
                ;; x = kv;
                (local.set $x (local.get $kv))
                ;; b = right->left->left;
                (local.set $b (i32.load (i32.add (local.get $rightLeft) (i32.const 4))))
                ;; y = right->left->kv;
                (local.set $y (i64.load (i32.add (local.get $rightLeft) (i32.const 8))))
                ;; c = right->left->right;
                (local.set $c (i32.load (i32.add (local.get $rightLeft) (i32.const 16))))
                ;; z = right->kv;
                (local.set $z (i64.load (i32.add (local.get $right) (i32.const 8))))
                ;; d = right->right;
                (local.set $d (i32.load (i32.add (local.get $right) (i32.const 16))))
                (br $ifBlock)))))))
    ;; } else if (redRight && right->right && right->right->color == R) {
    (if (local.get $redRight)
      (then
        (if (local.tee $rightRight (i32.load (i32.add (local.get $right) (i32.const 16))))
          (then
            (if (i32.eqz (i32.load (local.get $rightRight)))
              (then
                ;; a = left;
                (local.set $a (local.get $left))
                ;; x = kv;
                (local.set $x (local.get $kv))
                ;; b = right->left;
                (local.set $b (i32.load (i32.add (local.get $right) (i32.const 4))))
                ;; y = right->kv;
                (local.set $y (i64.load (i32.add (local.get $right) (i32.const 8))))
                ;; c = right->right->left;
                (local.set $c (i32.load (i32.add (local.get $rightRight) (i32.const 4))))
                ;; z = right->right->kv;
                (local.set $z (i64.load (i32.add (local.get $rightRight) (i32.const 8))))
                ;; d = right->right->right;
                (local.set $d (i32.load (i32.add (local.get $rightRight) (i32.const 16))))
                (br $ifBlock)))))))
    ;; } else { return treeCreate(arena, color, left, kv, right); }
    (return (call $map._create (local.get $color) (local.get $left) (local.get $kv) (local.get $right))))

  ;; Tree newLeft = treeCreate(arena, B, a, x, b);
  ;; Tree newRight = treeCreate(arena, B, c, z, d);
  ;; return treeCreate(arena, R, newLeft, y, newRight);
  (call $map._create
    (i32.const 0)
    (call $map._create (i32.const 1) (local.get $a) (local.get $x) (local.get $b))
    (local.get $y)
    (call $map._create (i32.const 1) (local.get $c) (local.get $z) (local.get $d))))

;; static struct InsertHelperResult {
;;     Tree tree;
;;     bool replaced;
;;     void *replacedValue;
;; } insertHelper(Arena arena, const Node *tree, CompareFn cmp, GKeyValue kv) {
(func $map._insertHelper (param $tree i32) (param $cmp i32) (param $kv i64) (result i32 i32 i32)
  (local $order i32)
  (local $newSub i32)
  (local $replaced i32)
  (local $replacedValue i32)

  ;; if (!tree) {
  (if (i32.eqz (local.get $tree))
    (then
      ;; return (struct InsertHelperResult){
      ;;     treeCreate(arena, R, nullptr, kv, nullptr),
      ;;     false,
      ;;     nullptr
      ;; };
      (return
        (call $map._create (i32.const 0) (i32.const 0) (local.get $kv) (i32.const 0))
        (i32.const 0)
        (i32.const 0))))
  ;; int order = cmp(kv.key, tree->kv.key);
  (local.set $order
    (call_indirect (param i32 i32) (result i32)
      (i32.wrap_i64 (local.get $kv))
      (i32.load (i32.add (local.get $tree) (i32.const 8)))
      (local.get $cmp)))

  ;; if (order < 0) {
  (if (i32.lt_s (local.get $order) (i32.const 0))
    (then
      ;; auto newLeft = insertHelper(arena, tree->left, cmp, kv);
      (local.set $newSub
        (local.set $replaced
          (local.set $replacedValue
            (call $map._insertHelper
              (i32.load (i32.add (local.get $tree) (i32.const 4)))
              (local.get $cmp)
              (local.get $kv)))))
      ;; newLeft.tree = lbalance(arena, tree->color, newLeft.tree, tree->kv, tree->right);
      ;; return newLeft;
      (return
        (call $map._lbalance
          (i32.load (local.get $tree))
          (local.get $newSub)
          (i64.load (i32.add (local.get $tree) (i32.const 8)))
          (i32.load (i32.add (local.get $tree) (i32.const 16))))
        (local.get $replaced)
        (local.get $replacedValue))))
  ;; } else if (order > 0) {
  (if (i32.gt_s (local.get $order) (i32.const 0))
    (then
      ;; auto newRight = insertHelper(arena, tree->right, cmp, kv);
      (local.set $newSub
        (local.set $replaced
          (local.set $replacedValue
            (call $map._insertHelper
              (i32.load (i32.add (local.get $tree) (i32.const 16)))
              (local.get $cmp)
              (local.get $kv)))))
      ;; newRight.tree = rbalance(arena, tree->color, tree->left, tree->kv, newRight.tree);
      ;; return newRight;
      (return
        (call $map._rbalance
          (i32.load (local.get $tree))
          (i32.load (i32.add (local.get $tree) (i32.const 4)))
          (i64.load (i32.add (local.get $tree) (i32.const 8)))
          (local.get $newSub))
        (local.get $replaced)
        (local.get $replacedValue))))

  ;; return (struct InsertHelperResult){
  ;;     treeCreate(arena, tree->color, tree->left, kv, tree->right),
  ;;     true,
  ;;     tree->kv.value
  ;; };
  (call $map._create
    (i32.load (local.get $tree))
    (i32.load (i32.add (local.get $tree) (i32.const 4)))
    (local.get $kv)
    (i32.load (i32.add (local.get $tree) (i32.const 16))))
  (i32.const 1)
  (i32.load (i32.add (local.get $tree) (i32.const 12))))

(func $map.empty (param $cmp i32) (result i32)
  (return_call $cons.3 (i32.const 0) (local.get $cmp) (i32.const 0)))

(func $map.count (param $map i32) (result i32)
  (return_call $car (local.get $map)))

;; struct GMapLookupResult {
;;     bool found;
;;     void *value;
;; } gmapLookup(GMap map, const void *key) {
(func $map.lookup (param $map i32) (param $key i32) (result i32 i32)
  (local $tree i32)
  (local $cmp i32)
  (local $order i32)

  (local.set $cmp (call $cdr (local.get $map)))
  ;; CTree tree = map->tree;
  (local.set $tree (call $cgr (local.get $map)))

  ;; while (tree) {
  (loop $loop
    (if (local.get $tree)
      (then
        ;; int order = map->cmp(key, tree->kv.key);
        (local.set $order
          (call_indirect (param i32 i32) (result i32)
            (local.get $key)
            (i32.load (i32.add (local.get $tree) (i32.const 8)))
            (local.get $cmp)))
        ;; if (order < 0) {
        (if (i32.lt_s (local.get $order) (i32.const 0))
          (then
            ;; tree = tree->left;
            (local.set $tree (i32.load (i32.add (local.get $tree) (i32.const 4))))
            (br $loop)))
        ;; } else if (order > 0) {
        (if (i32.gt_s (local.get $order) (i32.const 0))
          (then
            ;; tree = tree->right;
            (local.set $tree (i32.load (i32.add (local.get $tree) (i32.const 16))))
            (br $loop)))
        ;; } else { return (struct GMapLookupResult){true, tree->kv.value};
        (return (i32.const 1) (i32.load (i32.add (local.get $tree) (i32.const 12)))))))
  ;; return (struct GMapLookupResult){false, nullptr};
  (i32.const 0) (i32.const 0))

;; struct GMapInsertResult {
;;     GMap map;
;;     bool replaced;
;;     void *replacedValue;
;; } gmapInsert(GMap map, const void *key, void *value) {
(func $map.insert (param $map i32) (param $key i32) (param $value i32) (result i32 i32 i32)
  (local $count i32)
  (local $cmp i32)
  (local $tree i32)
  (local $replaced i32)
  (local $replacedValue i32)

  (local.set $count (local.set $cmp (local.set $tree (call $uncons.3 (local.get $map)))))

  ;; auto subResult = insertHelper(map->arena, map->tree, map->cmp, (GKeyValue){key, value});
  (local.set $tree
    (local.set $replaced
      (local.set $replacedValue
        (call $map._insertHelper
          (local.get $tree)
          (local.get $cmp)
          (i64.or
            (i64.extend_i32_s (local.get $key))
            (i64.shl (i64.extend_i32_s (local.get $value)) (i64.const 32)))))))
  ;; subResult.tree->color = B;
  (i32.store (local.get $tree) (i32.const 1))

  ;; return (struct GMapInsertResult){
  ;;     gmapCreateFill(map->count + !subResult.replaced, map->arena, map->cmp, subResult.tree),
  ;;     subResult.replaced,
  ;;     subResult.replacedValue
  ;; };
  (call $cons.3
    (i32.add (local.get $count) (i32.eqz (local.get $replaced)))
    (local.get $cmp)
    (local.get $tree))
  (local.get $replaced)
  (local.get $replacedValue))

(func $map._fillElements (param $tree i32) (param $destination i32) (param $space i32) (result i32 i32)
  (if (i32.eqz (local.get $tree))
    (then (return (local.get $destination) (local.get $space))))

  (local.set $destination
    (local.set $space
      (call $map._fillElements
        (i32.load (i32.add (local.get $tree) (i32.const 4)))
        (local.get $destination)
        (local.get $space))))

  (call $assert (i32.gt_s (local.get $space) (i32.const 0)))
  (i64.store
    (local.get $destination)
    (i64.load (i32.add (local.get $tree) (i32.const 8))))

  (return_call $map._fillElements
    (i32.load (i32.add (local.get $tree) (i32.const 16)))
    (i32.add (local.get $destination) (i32.const 8))
    (i32.sub (local.get $space) (i32.const 1))))

(func $map.elements (param $map i32) (result i32)
  (local $count i32)
  (local $tree i32)
  (local $result i32)

  (local.set $count (call $car (local.get $map)))
  (local.set $tree (call $cgr (local.get $map)))
  (local.set $result (call $malloc (i32.mul (i32.add (local.get $count) (i32.const 1)) (i32.const 8))))

  (drop (drop (call $map._fillElements (local.get $tree) (local.get $result) (local.get $count))))

  (local.get $result))

(data (i32.const 0x300) "{ count = ")
(data (i32.const 0x310) ", tree = ")
(data (i32.const 0x320) "}")
(data (i32.const 0x330) "<null>")
(data (i32.const 0x340) "{ R")
(data (i32.const 0x350) "{ B")
(data (i32.const 0x360) ", l = ")
(data (i32.const 0x370) ", k = ")
(data (i32.const 0x380) ", v = ")
(data (i32.const 0x390) ", r = ")

(func $map.dumpStructure (param $map i32)
  (local $count i32)
  (local $cmp i32)
  (local $tree i32)

  (local.set $count (local.set $cmp (local.set $tree (call $uncons.3 (local.get $map)))))
  (call $printStr (i32.const 0x300))
  (call $printI32.nl (local.get $count))
  (call $printStr (i32.const 0x310))
  (call $map._printTree (local.get $tree) (i32.const 2))
  (call $print.nl)
  (call $printStr.nl (i32.const 0x320)))

(func $map._printTree (param $tree i32) (param $indent i32)
  (if (local.get $tree)
    (then
      (if (i32.load (local.get $tree))
        (then (call $printStr (i32.const 0x350)))
        (else (call $printStr (i32.const 0x340))))
      (call $print.nl.indent (local.get $indent))
      (call $printStr (i32.const 0x360))
      (call $map._printTree
        (i32.load (i32.add (local.get $tree) (i32.const 4)))
        (i32.add (local.get $indent) (i32.const 2)))
      (call $print.nl.indent (local.get $indent))
      (call $printStr (i32.const 0x370))
      (call $printI32.hex (i32.load (i32.add (local.get $tree) (i32.const 8))))
      (call $printStr (i32.const 0x380))
      (call $printI32.hex (i32.load (i32.add (local.get $tree) (i32.const 12))))
      (call $print.nl.indent (local.get $indent))
      (call $printStr (i32.const 0x390))
      (call $map._printTree
        (i32.load (i32.add (local.get $tree) (i32.const 16)))
        (i32.add (local.get $indent) (i32.const 2)))
      (call $printStr (i32.const 0x320))
      nop)
    (else
      (call $printStr (i32.const 0x330)))))

(func $map.min (param $map i32) (result i32 i32 i32)
  (local $minNode i32)

  (local.set $map (call $cgr (local.get $map)))
  (loop $loop
    (if (local.get $map)
      (then
        (local.set $minNode (local.get $map))
        (local.set $map (i32.load (i32.add (local.get $map) (i32.const 4))))
        (br $loop))))
  (if (local.get $minNode)
    (then
      (return
        (i32.const 1)
        (i32.load (i32.add (local.get $minNode) (i32.const 8)))
        (i32.load (i32.add (local.get $minNode) (i32.const 12))))))
  (i32.const 0) (i32.const 0) (i32.const 0))

(func $map.max (param $map i32) (result i32 i32 i32)
  (local $maxNode i32)

  (local.set $map (call $cgr (local.get $map)))
  (loop $loop
    (if (local.get $map)
      (then
        (local.set $maxNode (local.get $map))
        (local.set $map (i32.load (i32.add (local.get $map) (i32.const 16))))
        (br $loop))))
  (if (local.get $maxNode)
    (then
      (return
        (i32.const 1)
        (i32.load (i32.add (local.get $maxNode) (i32.const 8)))
        (i32.load (i32.add (local.get $maxNode) (i32.const 12))))))
  (i32.const 0) (i32.const 0) (i32.const 0))
