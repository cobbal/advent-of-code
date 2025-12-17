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

(define (one-of elems) (cut memq <> elems))

(define (read* f)
  (let loop ([acc '()])
    (let ([form (read f)])
      (if (eof-object? form)
        (reverse acc)
        (loop (cons form acc))))))

(define-immutable-record-type static-allocator
  (make-static-allocator next-ptr strings fns)
  static-allocator?
  (next-ptr static-allocator-next-ptr set-static-allocator-next-ptr)
  (strings static-allocator-strings set-static-allocator-strings)
  (fns static-allocator-fns set-static-allocator-fns))

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

(define (static-alloc! env string)
  (define alloc-box (env-static-allocator env))
  (define result (static-allocator-next-ptr (unbox alloc-box)))
  (set-box! alloc-box
    (make-static-allocator
      (+ result (string-utf8-length string) 1)
      (cons `(data (i32.const ,result) ,string) (static-allocator-strings (unbox alloc-box)))
      (static-allocator-fns (unbox alloc-box))))
  result)

(define (push-fn-table! env fn-name)
  (define alloc-box (env-static-allocator env))
  (set-box! alloc-box
    (set-static-allocator-fns (unbox alloc-box)
      (cons fn-name (static-allocator-fns (unbox alloc-box))))))

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

(define (process/type env)
  (match-lambda
    [`(ref any) `(ref any)]
    [`(ref null any) `(ref null any)]
    [`(ref ,id) `(ref ,(lookup env id))]
    [`(ref null ,id) `(ref null ,(lookup env id))]
    [t t]))

(define (process/typedef env)
  (match-lambda
    [`(array (mut ,type)) `(array (mut ,((process/type env) type)))]
    [`(struct . ,fields)
      `(struct
         ,@(map
             (match-lambda
               [`(field ,id ,type) `(field ,(lookup env id) ,((process/type env) type))]
               [`(field ,type) `(field ,((process/type env) type))]
               [form (error "unrecognized field form:" form)])
             fields))]
    [t `(UNRECOGNIZED ,@t)]))


(define (process/params-results-body env forms)
  (match-let* ([`(,params ,forms) (take-while-car 'param forms)]
                [`(,results ,forms) (take-while-car 'result forms)])
    (list
      (map
        (match-lambda
          [`(,(? symbol? name) ,type) `(param ,(lookup env name) ,((process/type env) type))]
          [`(,type) `(param ,((process/type env) type))]
          [form (error "unrecognized param form:" `(param ,@form))])
        params)
      (map
        (match-lambda
          [`(,type) `(result ,((process/type env) type))]
          [form (error "unrecognized result form:" `(result ,@form))])
        results)
      forms)))

(define (process/module env)
  (match-lambda
    [`(namespace ,name . ,forms)
      (concatenate (map (process/module (env-push-namespace env name)) forms))]
    [`(import . ,imports) `((import ,@imports))]
    [`(,(? (one-of '(func indirect_func)) op) ,name . ,forms)
      (match-let* ([`(,params ,results ,forms) (process/params-results-body env forms)])
        (let ([fn-name (lookup env name)])
          (if (eq? op 'indirect_func)
            (push-fn-table! env fn-name))
          `((func ,fn-name
              ,@params ,@results
              ,@(concatenate (map (process/instr* env) forms))))))]
    [`(global ,name ,ty ,value)
      `((global ,name ,ty ,((process/instr env) value)))]
    [`(type ,id ,def)
      `((type ,(lookup env id) ,((process/typedef env) def)))]
    [form (error "unrecognized form:" form)]))

(define (process/instr* env)
  (match-lambda
    [`(i32 . ,forms) (concatenate (map (process/instr* (set-env-i-width env 'i32)) forms))]
    [`(i64 . ,forms) (concatenate (map (process/instr* (set-env-i-width env 'i64)) forms))]
    [`(locals ,ty . ,names) (map (cut list 'local <> ty) names)]
    [form (list ((process/instr env) form))]))

(define (process/instr env)
  (define (arith suffix . forms)
    (cons (sym-append (env-i-width env) "." suffix) forms))
  (define (arith* suffix form0 forms)
    (fold (lambda (x y) (arith suffix y x))
      (recur form0)
      (map recur forms)))

  (define (recur* f) ((process/instr* env) f))
  (define (recur f) ((process/instr env) f))
  (match-lambda
    [`(i32 ,form) ((process/instr (set-env-i-width env 'i32)) form)]
    [`(i64 ,form) ((process/instr (set-env-i-width env 'i64)) form)]
    [`(i32.const ,i) `(i32.const ,i)]
    [`(i64.const ,i) `(i64.const ,i)]
    [(? integer? i) (arith 'const i)]
    [(? char? c) (arith 'const (char->integer c) (comment (format #f "~s" c)))]

    [`(+ ,form0 . ,forms) (arith* 'add form0 forms)]
    [`(* ,form0 . ,forms) (arith* 'mul form0 forms)]
    [`(& ,form0 . ,forms) (arith* 'and form0 forms)]
    [`(- ,form0) (arith 'sub (arith 'const 0) (recur form0))]
    [`(- ,form0 . ,forms)
      (fold (lambda (x y) (arith 'sub y x))
        (recur form0)
        (map recur forms))]
    [`(,(? (one-of
             '(div_s div_u rem_s rem_u
                lt_s lt_u gt_s gt_u le_s le_u ge_s ge_u
                shl shr_s shr_u rotl rotr))
          op) ,form0 ,form1)
      (arith op (recur form0) (recur form1))]
    [`(= ,form0 ,form1) (arith 'eq (recur form0) (recur form1))]
    [`(! ,form0) (arith 'eqz (recur form0))]

    [`(,(? (one-of '(call return_call)) op) ,name . ,args)
      `(call ,(lookup env name) ,@(map recur args))]
    [`(,(? (one-of '(call_indirect return_call_indirect)) op) . ,forms)
      (match-let* ([`(,params ,results ,forms) (process/params-results-body env forms)])
        `(,op ,@params ,@results ,@(map recur forms)))]
    [`(,(? (one-of '(loop block)) op) ,label . ,forms)
      `(,op ,label ,@(concatenate (map recur* forms)))]
    [`(br ,id) `(br ,id)]
    [`(br_if ,id ,form) `(br_if ,id ,(recur form))]

    ;; [`(local.get ,name) `(local.get ,name)]
    [`(,(? (one-of '(local.set local.tee)) op) ,name ,form) `(,op ,name ,(recur form))]
    [`(global.get ,name) `(global.get ,(lookup env name))]
    [`(global.set ,name ,form) `(global.set ,(lookup env name) ,(recur form))]

    [`(ref.null ,type) `(ref.null ,(lookup env type))]

    [`(array.new ,type ,fill ,count) `(array.new ,type ,(recur fill) ,(recur count))]
    [`(array.new_default ,type ,count) `(array.new_default ,type ,(recur count))]
    [`(array.set ,type ,arr ,idx ,value) `(array.set ,type ,(recur arr) ,(recur idx) ,(recur value))]
    [`(,(? (one-of '(array.get array.get_u array.get_s)) op) ,type ,arr ,idx)
      `(,op ,type ,(recur arr) ,(recur idx))]
    [`(array.len ,type ,arr) `(array.len ,type ,(recur arr))]

    [`(struct.new ,type . ,fields)
      `(struct.new ,(lookup env type) ,@(map recur fields))]
    [`(struct.get ,type ,field ,val)
      `(struct.get ,(lookup env type) ,field ,(recur val))]

    [`(funcref ,name) `(global.get ,(symbol-append '$fns. (lookup env name)))]
    [`(add! ,var ,n) (recur `(local.set ,var (+ ,var ,(recur n))))]

    ['drop '(drop)]
    ['unreachable '(unreachable)]
    [(or 'debugger '(debugger)) '(drop (call $sched_yield))]

    [(? string? s)
      `(i32.const ,(static-alloc! env s) ,(comment (format #f "~s" s)))]
    [`(,(? symbol? op) . ,args)
      `(,op ,@(map recur args))]
    [(? symbol? id) `(local.get ,(lookup env id))]

    [form `(UNRECOGNIZED ,form)]
    [form (error "unrecognized form:" form)]))

(define (process* env forms)
  (concatenate (map (process/module env) forms)))

(define (hoist-imports forms static-allocator)
  (define datae (reverse (static-allocator-strings static-allocator)))
  (define memory-size (static-allocator-next-ptr static-allocator))
  (define memory
    `((memory (export "memory") ,(ceiling-quotient memory-size #x10000))
       (global $_io.alloc.end (mut i32) (i32.const ,memory-size))))

  (define indirect-fns (reverse (static-allocator-fns static-allocator)))
  (define fn-table
    `((table $fns (export "fns") funcref
        (elem ,@indirect-fns))
       ,@(let loop ([i 0] [indirect-fns indirect-fns])
           (if (null? indirect-fns)
             '()
             `((global ,(symbol-append '$fns. (car indirect-fns)) i32 (i32.const ,i))
                ,@(loop (+ i 1) (cdr indirect-fns)))))))
  (receive (imports rest)
    (partition (lambda (x) (eq? (car-safe x) 'import)) forms)
    `(,@imports
       ,@datae
       ,@memory
       ,@fn-table
       (export "_start" (func $_start))
       ;; (start $_start)
       ,@rest)))

(define (process-inputs paths)
  (define out-wasm "build/2017.wat")
  (format #t "Compiling ~a to ~a...\n" paths out-wasm)
  (define env (make-env "" 'i32 (box (make-static-allocator 1024 '() '()))))
  (define modules (map (lambda (path) (process* env (call-with-input-file path read*))) paths))
  (call-with-output-file out-wasm
    (cut pretty-print
      `(module
         ,@(hoist-imports (concatenate modules) (unbox (env-static-allocator env))))
      <>)))

(process-inputs (cdr (program-arguments)))
