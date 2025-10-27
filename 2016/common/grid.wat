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
