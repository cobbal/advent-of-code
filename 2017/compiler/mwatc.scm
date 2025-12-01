(use-modules
  (ice-9 format)
  (ice-9 match)
  (ice-9 pretty-print)
  (ice-9 receive)
  (srfi srfi-1)
  (srfi srfi-9 gnu)
  (srfi srfi-26))

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

(define-immutable-record-type env
  (make-env namespace)
  env?
  (namespace env-namespace set-env-namespace))

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
            ,@(map (process/instr 'i32) forms))))]
    [form (error "unrecognized form:" form)]))

(define (process/instr* i-width)
  (match-lambda
    [`(i64 . ,forms) (concatenate (map (process/instr* 'i64) forms))]
    [`(i32 . ,forms) (concatenate (map (process/instr* 'i32) forms))]
    [form (list ((process/instr i-width) form))]))

(define (process/instr i-width)
  (define (arith suffix . forms)
    (cons (sym-append i-width "." suffix) forms))
  (define (recur* f) ((process/instr* i-width) f))
  (define (recur f) ((process/instr i-width) f))
  (match-lambda
    [`(i64 ,form) ((process/instr 'i64) form)]
    [`(i32 ,form) ((process/instr 'i32) form)]
    [(? integer? i) (arith 'const i)]
    [`(+ ,form0 . ,forms)
      (fold (lambda (x y) (arith 'add y x))
        (recur form0)
        (map recur forms))]
    [form `(UNRECOGNIZED ,form)]
    [form (error "unrecognized form:" form)]))

(define (process* forms)
  (concatenate (map (process/module (make-env "")) forms)))

(define (hoist-imports forms)
  (receive (imports rest)
    (partition (lambda (x) (eq? (car-safe x) 'import)) forms)
    (append imports rest)))

(define (process-inputs paths)
  (define out-wasm "build/2017.wat")
  (format #t "Compiling ~a to ~a...\n" paths out-wasm)
  (define modules (map (lambda (path) (process* (call-with-input-file path read*))) paths))
  (call-with-output-file out-wasm
    (cut pretty-print `(module ,@(hoist-imports (concatenate modules))) <>)))

(process-inputs (cdr (program-arguments)))
