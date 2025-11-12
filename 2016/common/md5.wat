;; From https://en.wikipedia.org/wiki/MD5

;;        context layout
;; 0x000-0x039 | s constants (64 bytes)
;; 0x040-0x139 | k constants (256 bytes)
;; 0x140-0x1b9 | chunk scratch (128 bytes)

(func $md5.createContext (result i32)
  (local $s i32)
  (local $k i32)

  (local.set $k
    (i32.add (i32.const 64)
      (local.tee $s (call $malloc (i32.const 0x240)))))

  (i32.store (i32.add (local.get $s) (i32.const 0x00)) (i32.const 0x16110c07))
  (i32.store (i32.add (local.get $s) (i32.const 0x04)) (i32.const 0x16110c07))
  (i32.store (i32.add (local.get $s) (i32.const 0x08)) (i32.const 0x16110c07))
  (i32.store (i32.add (local.get $s) (i32.const 0x0c)) (i32.const 0x16110c07))
  (i32.store (i32.add (local.get $s) (i32.const 0x10)) (i32.const 0x140e0905))
  (i32.store (i32.add (local.get $s) (i32.const 0x14)) (i32.const 0x140e0905))
  (i32.store (i32.add (local.get $s) (i32.const 0x18)) (i32.const 0x140e0905))
  (i32.store (i32.add (local.get $s) (i32.const 0x1c)) (i32.const 0x140e0905))
  (i32.store (i32.add (local.get $s) (i32.const 0x20)) (i32.const 0x17100b04))
  (i32.store (i32.add (local.get $s) (i32.const 0x24)) (i32.const 0x17100b04))
  (i32.store (i32.add (local.get $s) (i32.const 0x28)) (i32.const 0x17100b04))
  (i32.store (i32.add (local.get $s) (i32.const 0x2c)) (i32.const 0x17100b04))
  (i32.store (i32.add (local.get $s) (i32.const 0x30)) (i32.const 0x150f0a06))
  (i32.store (i32.add (local.get $s) (i32.const 0x34)) (i32.const 0x150f0a06))
  (i32.store (i32.add (local.get $s) (i32.const 0x38)) (i32.const 0x150f0a06))
  (i32.store (i32.add (local.get $s) (i32.const 0x3c)) (i32.const 0x150f0a06))

  (i32.store (i32.add (local.get $k) (i32.const 0x00)) (i32.const 0xd76aa478))
  (i32.store (i32.add (local.get $k) (i32.const 0x04)) (i32.const 0xe8c7b756))
  (i32.store (i32.add (local.get $k) (i32.const 0x08)) (i32.const 0x242070db))
  (i32.store (i32.add (local.get $k) (i32.const 0x0c)) (i32.const 0xc1bdceee))
  (i32.store (i32.add (local.get $k) (i32.const 0x10)) (i32.const 0xf57c0faf))
  (i32.store (i32.add (local.get $k) (i32.const 0x14)) (i32.const 0x4787c62a))
  (i32.store (i32.add (local.get $k) (i32.const 0x18)) (i32.const 0xa8304613))
  (i32.store (i32.add (local.get $k) (i32.const 0x1c)) (i32.const 0xfd469501))
  (i32.store (i32.add (local.get $k) (i32.const 0x20)) (i32.const 0x698098d8))
  (i32.store (i32.add (local.get $k) (i32.const 0x24)) (i32.const 0x8b44f7af))
  (i32.store (i32.add (local.get $k) (i32.const 0x28)) (i32.const 0xffff5bb1))
  (i32.store (i32.add (local.get $k) (i32.const 0x2c)) (i32.const 0x895cd7be))
  (i32.store (i32.add (local.get $k) (i32.const 0x30)) (i32.const 0x6b901122))
  (i32.store (i32.add (local.get $k) (i32.const 0x34)) (i32.const 0xfd987193))
  (i32.store (i32.add (local.get $k) (i32.const 0x38)) (i32.const 0xa679438e))
  (i32.store (i32.add (local.get $k) (i32.const 0x3c)) (i32.const 0x49b40821))
  (i32.store (i32.add (local.get $k) (i32.const 0x40)) (i32.const 0xf61e2562))
  (i32.store (i32.add (local.get $k) (i32.const 0x44)) (i32.const 0xc040b340))
  (i32.store (i32.add (local.get $k) (i32.const 0x48)) (i32.const 0x265e5a51))
  (i32.store (i32.add (local.get $k) (i32.const 0x4c)) (i32.const 0xe9b6c7aa))
  (i32.store (i32.add (local.get $k) (i32.const 0x50)) (i32.const 0xd62f105d))
  (i32.store (i32.add (local.get $k) (i32.const 0x54)) (i32.const 0x02441453))
  (i32.store (i32.add (local.get $k) (i32.const 0x58)) (i32.const 0xd8a1e681))
  (i32.store (i32.add (local.get $k) (i32.const 0x5c)) (i32.const 0xe7d3fbc8))
  (i32.store (i32.add (local.get $k) (i32.const 0x60)) (i32.const 0x21e1cde6))
  (i32.store (i32.add (local.get $k) (i32.const 0x64)) (i32.const 0xc33707d6))
  (i32.store (i32.add (local.get $k) (i32.const 0x68)) (i32.const 0xf4d50d87))
  (i32.store (i32.add (local.get $k) (i32.const 0x6c)) (i32.const 0x455a14ed))
  (i32.store (i32.add (local.get $k) (i32.const 0x70)) (i32.const 0xa9e3e905))
  (i32.store (i32.add (local.get $k) (i32.const 0x74)) (i32.const 0xfcefa3f8))
  (i32.store (i32.add (local.get $k) (i32.const 0x78)) (i32.const 0x676f02d9))
  (i32.store (i32.add (local.get $k) (i32.const 0x7c)) (i32.const 0x8d2a4c8a))
  (i32.store (i32.add (local.get $k) (i32.const 0x80)) (i32.const 0xfffa3942))
  (i32.store (i32.add (local.get $k) (i32.const 0x84)) (i32.const 0x8771f681))
  (i32.store (i32.add (local.get $k) (i32.const 0x88)) (i32.const 0x6d9d6122))
  (i32.store (i32.add (local.get $k) (i32.const 0x8c)) (i32.const 0xfde5380c))
  (i32.store (i32.add (local.get $k) (i32.const 0x90)) (i32.const 0xa4beea44))
  (i32.store (i32.add (local.get $k) (i32.const 0x94)) (i32.const 0x4bdecfa9))
  (i32.store (i32.add (local.get $k) (i32.const 0x98)) (i32.const 0xf6bb4b60))
  (i32.store (i32.add (local.get $k) (i32.const 0x9c)) (i32.const 0xbebfbc70))
  (i32.store (i32.add (local.get $k) (i32.const 0xa0)) (i32.const 0x289b7ec6))
  (i32.store (i32.add (local.get $k) (i32.const 0xa4)) (i32.const 0xeaa127fa))
  (i32.store (i32.add (local.get $k) (i32.const 0xa8)) (i32.const 0xd4ef3085))
  (i32.store (i32.add (local.get $k) (i32.const 0xac)) (i32.const 0x04881d05))
  (i32.store (i32.add (local.get $k) (i32.const 0xb0)) (i32.const 0xd9d4d039))
  (i32.store (i32.add (local.get $k) (i32.const 0xb4)) (i32.const 0xe6db99e5))
  (i32.store (i32.add (local.get $k) (i32.const 0xb8)) (i32.const 0x1fa27cf8))
  (i32.store (i32.add (local.get $k) (i32.const 0xbc)) (i32.const 0xc4ac5665))
  (i32.store (i32.add (local.get $k) (i32.const 0xc0)) (i32.const 0xf4292244))
  (i32.store (i32.add (local.get $k) (i32.const 0xc4)) (i32.const 0x432aff97))
  (i32.store (i32.add (local.get $k) (i32.const 0xc8)) (i32.const 0xab9423a7))
  (i32.store (i32.add (local.get $k) (i32.const 0xcc)) (i32.const 0xfc93a039))
  (i32.store (i32.add (local.get $k) (i32.const 0xd0)) (i32.const 0x655b59c3))
  (i32.store (i32.add (local.get $k) (i32.const 0xd4)) (i32.const 0x8f0ccc92))
  (i32.store (i32.add (local.get $k) (i32.const 0xd8)) (i32.const 0xffeff47d))
  (i32.store (i32.add (local.get $k) (i32.const 0xdc)) (i32.const 0x85845dd1))
  (i32.store (i32.add (local.get $k) (i32.const 0xe0)) (i32.const 0x6fa87e4f))
  (i32.store (i32.add (local.get $k) (i32.const 0xe4)) (i32.const 0xfe2ce6e0))
  (i32.store (i32.add (local.get $k) (i32.const 0xe8)) (i32.const 0xa3014314))
  (i32.store (i32.add (local.get $k) (i32.const 0xec)) (i32.const 0x4e0811a1))
  (i32.store (i32.add (local.get $k) (i32.const 0xf0)) (i32.const 0xf7537e82))
  (i32.store (i32.add (local.get $k) (i32.const 0xf4)) (i32.const 0xbd3af235))
  (i32.store (i32.add (local.get $k) (i32.const 0xf8)) (i32.const 0x2ad7d2bb))
  (i32.store (i32.add (local.get $k) (i32.const 0xfc)) (i32.const 0xeb86d391))
  (local.get $s))

(func $md5.digest (param $context i32) (param $message i32) (param $len i32) (result i32 i32 i32 i32)
  (local $state.a i32)
  (local $state.b i32)
  (local $state.c i32)
  (local $state.d i32)
  (local $spilloverLen i32)
  (local $chunkCount i32)
  (local $lastChunks i32)
  (local $lastChunkCount i32)
  (local $chunkIndex i32)
  (local $m i32)

  (local $a i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)

  (local $i i32)
  (local $f i32)
  (local $g i32)
  (local $word i32)

  (local.set $state.a (i32.const 0x67452301))
  (local.set $state.b (i32.const 0xefcdab89))
  (local.set $state.c (i32.const 0x98badcfe))
  (local.set $state.d (i32.const 0x10325476))

  (local.set $spilloverLen (i32.rem_u (local.get $len) (i32.const 64)))
  (local.set $chunkCount (i32.div_u (i32.sub (local.get $len) (local.get $spilloverLen)) (i32.const 64)))

  (local.set $lastChunks (i32.add (local.get $context) (i32.const 0x140)))
  (memory.fill (local.get $lastChunks) (i32.const 0) (i32.const 128))
  (memory.copy
    (local.get $lastChunks)
    (i32.add (local.get $message) (i32.shl (local.get $chunkCount)  (i32.const 6)))
    (local.get $spilloverLen))
  (i32.store8 (i32.add (local.get $lastChunks) (local.get $spilloverLen)) (i32.const 0x80))
  (local.set $lastChunkCount (i32.sub (i32.const 2) (i32.lt_u (local.get $spilloverLen) (i32.const 56))))
  (i64.store
    (i32.add
      (local.get $lastChunks)
      (i32.sub
        (i32.shl (local.get $lastChunkCount) (i32.const 6))
        (i32.const 8)))
    (i64.shl (i64.extend_i32_s (local.get $len)) (i64.const 3)))

  ;; Process the message in successive 512-bit chunks:
  (loop $chunkLoop
    (if (i32.lt_u (local.get $chunkIndex) (i32.add (local.get $chunkCount) (local.get $lastChunkCount)))
      (then
        (if (i32.lt_u (local.get $chunkIndex) (local.get $chunkCount))
          (then
            (local.set $m
              (i32.add
                (local.get $message)
                (i32.shl (local.get $chunkIndex) (i32.const 6)))))
          (else
            (local.set $m
              (i32.add
                (local.get $lastChunks)
                (i32.shl (i32.sub (local.get $chunkIndex) (local.get $chunkCount)) (i32.const 6))))))

        (local.set $a (local.get $state.a))
        (local.set $b (local.get $state.b))
        (local.set $c (local.get $state.c))
        (local.set $d (local.get $state.d))

        (local.set $i (i32.const 0))
        (block $breakLoop
          (loop $roundLoop
            (block $postPhase
              (block $phase3
                (block $phase2
                  (block $phase1
                    (block $phase0
                      (br_table $phase0 $phase1 $phase2 $phase3 $breakLoop (i32.shr_u (local.get $i) (i32.const 4))))
                    ;; phase 0
                    (local.set $f
                      (i32.or
                        (i32.and (local.get $b) (local.get $c))
                        (i32.and (i32.xor (i32.const -1) (local.get $b)) (local.get $d))))
                    (local.set $g (local.get $i))
                    (br $postPhase))
                  ;; phase 1
                  (local.set $f
                    (i32.or
                      (i32.and (local.get $d) (local.get $b))
                      (i32.and (i32.xor (i32.const -1) (local.get $d)) (local.get $c))))
                  (local.set $g
                    (i32.and (i32.const 0xf)
                      (i32.add (i32.mul (i32.const 5) (local.get $i)) (i32.const 1))))
                  (br $postPhase))
                ;; phase 2
                (local.set $f (i32.xor (local.get $b) (i32.xor (local.get $c) (local.get $d))))
                (local.set $g
                  (i32.and (i32.const 0xf)
                    (i32.add (i32.mul (i32.const 3) (local.get $i)) (i32.const 5))))
                (br $postPhase))
              ;; phase 3
              (local.set $f (i32.xor (local.get $c) (i32.or (local.get $b) (i32.xor (i32.const -1) (local.get $d)))))
              (local.set $g
                (i32.and (i32.const 0xf)
                  (i32.mul (i32.const 7) (local.get $i))))
              (br $postPhase))
            ;; post phase
            (local.set $word (i32.load (i32.add (local.get $m) (i32.shl (local.get $g) (i32.const 2)))))
            (local.set $f
              (i32.add
                (local.get $f)
                (i32.add
                  (local.get $a)
                  (i32.add
                    (i32.load (i32.add (local.get $context) (i32.add (i32.const 0x40) (i32.shl (local.get $i) (i32.const 2)))))
                    (local.get $word)))))
            (local.set $a (local.get $d))
            (local.set $d (local.get $c))
            (local.set $c (local.get $b))
            (local.set $b
              (i32.add (local.get $b)
                (i32.rotl (local.get $f)
                  (i32.load8_u (i32.add (local.get $context) (local.get $i))))))

            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $roundLoop)))

        (local.set $state.a (i32.add (local.get $state.a) (local.get $a)))
        (local.set $state.b (i32.add (local.get $state.b) (local.get $b)))
        (local.set $state.c (i32.add (local.get $state.c) (local.get $c)))
        (local.set $state.d (i32.add (local.get $state.d) (local.get $d)))

        (local.set $chunkIndex (i32.add (local.get $chunkIndex) (i32.const 1)))
        (br $chunkLoop))))
    (local.get $state.a) (local.get $state.b) (local.get $state.c) (local.get $state.d))

;; outPtr should have 33 bytes of space
(func $md5.hexString.into (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $outPtr i32)
  (call $formatI32.le.hex.into (local.get $a) (i32.add (local.get $outPtr) (i32.const 0)))
  (call $formatI32.le.hex.into (local.get $b) (i32.add (local.get $outPtr) (i32.const 8)))
  (call $formatI32.le.hex.into (local.get $c) (i32.add (local.get $outPtr) (i32.const 16)))
  (call $formatI32.le.hex.into (local.get $d) (i32.add (local.get $outPtr) (i32.const 24)))
  (i32.store8 (i32.add (local.get $outPtr) (i32.const 32)) (i32.const 0)))

(func $md5.hexString (param $a i32) (param $b i32) (param $c i32) (param $d i32) (result i32)
  (local $result i32)
  (call $md5.hexString.into
    (local.get $a) (local.get $b) (local.get $c) (local.get $d)
    (local.tee $result (call $malloc (i32.const 33))))
  (local.get $result))
