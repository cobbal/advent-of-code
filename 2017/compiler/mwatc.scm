(use-modules
  (ice-9 format)
  (ice-9 match)
  (ice-9 pretty-print)
  (ice-9 receive)
  (rnrs bytevectors)
  (srfi srfi-1)
  (srfi srfi-9 gnu)
  (srfi srfi-26)
  (srfi srfi-111))

(define (displayln . args) (apply display args) (newline))
(define (sym-coerce sym-or-str)
  (cond
    [(symbol? sym-or-str) sym-or-str]
    [(string? sym-or-str) (string->symbol sym-or-str)]
    [#t (error "Expected string or symbol, got:" sym-or-str)]))
(define (sym-append . syms-or-strs) (apply symbol-append (map sym-coerce syms-or-strs)))

(define safe-failure (make-symbol "safe-failure"))
(define (car-safe x) (if (pair? x) (car x) safe-failure))
(define (cdr-safe x) (if (pair? x) (cdr x) safe-failure))

(define (read* f)
  (let loop ([acc '()])
    (let ([form (read f)])
      (if (eof-object? form)
        (reverse acc)
        (loop (cons form acc))))))

(define-immutable-record-type static-allocator
  (make-static-allocator next-ptr strings)
  static-allocator?
  (next-ptr static-allocator-next-ptr set-static-allocator-next-ptr)
  (strings static-allocator-strings set-static-allocator-strings))

(define-immutable-record-type env
  (make-env namespace i-width static-allocator)
  env?
  (namespace env-namespace set-env-namespace)
  (i-width env-i-width set-env-i-width)
  (static-allocator env-static-allocator))

(define-immutable-record-type comment-record
  (comment contents)
  comment?
  (contents comment-contents))
(set-record-type-printer! comment-record
  (lambda (record port)
    (format port "(; ~a ;)" (comment-contents record))))

(define (static-alloc env string)
  (define alloc-box (env-static-allocator env))
  (define result (static-allocator-next-ptr (unbox alloc-box)))
  (set-box! alloc-box
    (make-static-allocator
      (+ result (string-utf8-length string) 1)
      (cons `(data (i32.const ,result) ,string) (static-allocator-strings (unbox alloc-box)))))
  result)

(define (env-push-namespace env name)
  (set-env-namespace env (string-append (env-namespace env) (symbol->string name) ".")))

(define (take-while-car target lst)
  (let loop ([matches '()] [lst lst])
    (if (eq? target (car-safe (car-safe lst)))
      (loop (cons (cdr (car lst)) matches) (cdr lst))
      (list (reverse matches) lst))))

(define (lookup env name)
  (let ([str (symbol->string name)])
    (if (string-prefix? "$" str)
      name
      (string->symbol (string-append (env-namespace env) str)))))

(define (process/module env)
  (match-lambda
    [`(namespace ,name . ,forms)
      (concatenate (map (process/module (env-push-namespace env name)) forms))]
    [`(import . ,imports) `((import ,@imports))]
    [`(func ,name . ,forms)
      (match-let* ([`(,params ,forms) (take-while-car 'param forms)]
                    [`(,results ,forms) (take-while-car 'result forms)])
        `((func ,(lookup env name)
            ,@(map (cut cons 'param <>) params)
            ,@(map (cut cons 'result <>) results)
            ,@(map (process/instr env) forms))))]
    [form (error "unrecognized form:" form)]))

(define (process/instr* env)
  (match-lambda
    [`(i32 . ,forms) (concatenate (map (process/instr* (set-env-i-width env 'i32)) forms))]
    [`(i64 . ,forms) (concatenate (map (process/instr* (set-env-i-width env 'i64)) forms))]
    [form (list ((process/instr env) form))]))

(define (process/instr env)
  (define (arith suffix . forms)
    (cons (sym-append (env-i-width env) "." suffix) forms))
  (define (recur* f) ((process/instr* env) f))
  (define (recur f) ((process/instr env) f))
  (match-lambda
    [`(i32 ,form) ((process/instr (set-env-i-width env 'i32)) form)]
    [`(i64 ,form) ((process/instr (set-env-i-width env 'i64)) form)]
    [(? integer? i) (arith 'const i)]
    [`(+ ,form0 . ,forms)
      (fold (lambda (x y) (arith 'add y x))
        (recur form0)
        (map recur forms))]
    [`(call ,fn . ,args)
      `(call ,(lookup env fn) . ,(map recur args))]
    [(? string? s)
      `(i32.const ,(static-alloc env s) ,(comment (format #f "~s" s)))]
    [form `(UNRECOGNIZED ,form)]
    [form (error "unrecognized form:" form)]))

(define (process* env forms)
  (concatenate (map (process/module env) forms)))

(define (hoist-imports forms static-allocator)
  (define datae (reverse (static-allocator-strings static-allocator)))
  (define memory-size (static-allocator-next-ptr static-allocator))
  (define memory `(memory (export "memory") ,(ceiling-quotient memory-size #x4000)))
  (receive (imports rest)
    (partition (lambda (x) (eq? (car-safe x) 'import)) forms)
    (append imports datae (list memory) rest)))

(define (process-inputs paths)
  (define out-wasm "build/2017.wat")
  (format #t "Compiling ~a to ~a...\n" paths out-wasm)
  (define env (make-env "" 'i32 (box (make-static-allocator 1024 '()))))
  (define modules (map (lambda (path) (process* env (call-with-input-file path read*))) paths))
  (call-with-output-file out-wasm
    (cut pretty-print
      `(module
         ,@(hoist-imports (concatenate modules) (unbox (env-static-allocator env))))
      <>)))

(process-inputs (cdr (program-arguments)))
