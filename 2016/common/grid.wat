(func $grid.create (param $height i32) (param $width i32) (result i32)
  (local $grid i32)
  (local.set $grid
    (call $malloc
      (i32.add
        (i32.const 8)
        (i32.mul (i32.add (local.get $width) (i32.const 1)) (local.get $height)))))
  (i32.store (local.get $grid) (local.get $height))
  (i32.store (i32.add (local.get $grid) (i32.const 4)) (local.get $width))
  (local.get $grid))

(func $grid.fill (param $grid i32) (param $fillChar i32)
  (local $y i32)
  (local $width i32)
  (local $height i32)
  (local $data i32)

  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (local.set $data (i32.add (local.get $grid) (i32.const 8)))

  (loop $loop
    (if (i32.lt_u (local.get $y) (local.get $height))
      (then
        (memory.fill
          (i32.add (local.get $data)
            (i32.mul (local.get $y) (i32.add (local.get $width) (i32.const 1))))
          (local.get $fillChar)
          (local.get $width))
        (local.set $y (i32.add (local.get $y) (i32.const 1)))
        (br $loop)))))

(func $grid.print (param $grid i32)
  (local $y i32)
  (local $width i32)
  (local $height i32)
  (local $data i32)

  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (local.set $data (i32.add (local.get $grid) (i32.const 8)))

  (call $print.nl)
  (loop $loop
    (if (i32.lt_u (local.get $y) (local.get $height))
      (then
        (call $printStr.nl
          (i32.add (local.get $data)
            (i32.mul (local.get $y) (i32.add (local.get $width) (i32.const 1)))))
        (local.set $y (i32.add (local.get $y) (i32.const 1)))
        (br $loop)))))

(func $grid.check (param $grid i32) (param $y i32) (param $x i32)
  (local $width i32)
  (local $height i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (call $assert
    (i32.and
      (i32.lt_u (local.get $y) (local.get $height))
      (i32.lt_u (local.get $x) (local.get $width)))))

(func $grid._ptr (param $grid i32) (param $y i32) (param $x i32) (result i32)
  (local $width i32)
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (i32.add
    (i32.add (local.get $grid) (i32.const 8))
    (i32.add
      (i32.mul (local.get $y) (i32.add (local.get $width) (i32.const 1)))
      (local.get $x))))

(func $grid.set (param $grid i32) (param $y i32) (param $x i32) (param $newValue i32)
  (call $grid.check (local.get $grid) (local.get $y) (local.get $x))
  (i32.store8
    (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))
    (local.get $newValue)))

(func $grid.get (param $grid i32) (param $y i32) (param $x i32) (result i32)
  (call $grid.check (local.get $grid) (local.get $y) (local.get $x))
  (i32.load8_u (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))))

(func $grid.getWrap (param $grid i32) (param $y i32) (param $x i32) (result i32)
  (local $width i32)
  (local $height i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))

  (local.set $y (call $i32.mod_s (local.get $y) (local.get $height)))
  (local.set $x (call $i32.mod_s (local.get $x) (local.get $width)))

  (i32.load8_u (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))))

(func $grid.setWrap (param $grid i32) (param $y i32) (param $x i32) (param $newValue i32)
  (local $width i32)
  (local $height i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))

  (local.set $y (call $i32.mod_s (local.get $y) (local.get $height)))
  (local.set $x (call $i32.mod_s (local.get $x) (local.get $width)))

  (i32.store8
    (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))
    (local.get $newValue)))

(func $grid.getSafe (param $grid i32) (param $y i32) (param $x i32) (param $default i32) (result i32)
  (local $width i32)
  (local $height i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (if
    (i32.and
      (i32.lt_u (local.get $y) (local.get $height))
      (i32.lt_u (local.get $x) (local.get $width)))
      (then (return (i32.load8_u (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))))))
  (local.get $default))

(func $grid.getHeight (param $grid i32) (result i32)
  (i32.load (local.get $grid)))

(func $grid.getWidth (param $grid i32) (result i32)
  (i32.load (i32.add (local.get $grid) (i32.const 4))))

(func $grid.count (param $grid i32) (param $target i32) (result i32)
  (local $width i32)
  (local $height i32)
  (local $y i32)
  (local $x i32)
  (local $result i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (loop $loopY
    (if (i32.lt_u (local.get $y) (local.get $height))
      (then
        (local.set $x (i32.const 0))
        (loop $loopX
          (if (i32.lt_u (local.get $x) (local.get $width))
            (then
              (if (i32.eq (i32.load8_u (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))) (local.get $target))
                (then
                  (local.set $result (i32.add (local.get $result) (i32.const 1)))))
              (local.set $x (i32.add (local.get $x) (i32.const 1)))
              (br $loopX))))
        (local.set $y (i32.add (local.get $y) (i32.const 1)))
        (br $loopY))))
  (local.get $result))

(func $grid.find (param $grid i32) (param $target i32) (result i32)
  (local $width i32)
  (local $height i32)
  (local $y i32)
  (local $x i32)
  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (loop $loopY
    (if (i32.lt_u (local.get $y) (local.get $height))
      (then
        (local.set $x (i32.const 0))
        (loop $loopX
          (if (i32.lt_u (local.get $x) (local.get $width))
            (then
              (if (i32.eq (i32.load8_u (call $grid._ptr (local.get $grid) (local.get $y) (local.get $x))) (local.get $target))
                (then (return (i32.add (i32.mul (local.get $y) (local.get $width)) (local.get $x)))))
              (local.set $x (i32.add (local.get $x) (i32.const 1)))
              (br $loopX))))
        (local.set $y (i32.add (local.get $y) (i32.const 1)))
        (br $loopY))))
  (i32.const -1))

(func $grid.index.pack (param $grid i32) (param $y i32) (param $x i32) (result i32)
  (local $width i32)
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (i32.add (i32.mul (local.get $y) (local.get $width)) (local.get $x)))

(func $grid.index.unpack (param $grid i32) (param $yx i32) (result i32 i32)
  (local $width i32)
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))
  (i32.div_u (local.get $yx) (local.get $width))
  (i32.rem_u (local.get $yx) (local.get $width)))

(func $grid.format (param $grid i32) (result i32)
  (local $width i32)
  (local $height i32)
  (local $byteLen i32)
  (local $result i32)
  (local $y i32)

  (local.set $height (i32.load (local.get $grid)))
  (local.set $width (i32.load (i32.add (local.get $grid) (i32.const 4))))

  (local.set $byteLen
    (i32.mul (local.get $height) (i32.add (local.get $width) (i32.const 1))))

  (local.set $result (call $malloc (i32.add (local.get $byteLen) (i32.const 2))))
  (memory.copy
    (i32.add (local.get $result) (i32.const 1))
    (i32.add (local.get $grid) (i32.const 8))
    (local.get $byteLen))
  (i32.store8 (local.get $result) (i32.const 0x0a))

  (loop $loop
    (if (i32.le_u (local.get $y) (local.get $height))
      (then
        (i32.store8
          (i32.add
            (local.get $result)
            (i32.mul (local.get $y) (i32.add (local.get $width) (i32.const 1))))
          (i32.const 0x0a))
        (local.set $y (i32.add (local.get $y) (i32.const 1)))
        (br $loop))))

  (local.get $result))
