(use-modules
  (ice-9 format)
  (ice-9 match)
  (ice-9 pretty-print)
  (srfi srfi-1)
  (srfi srfi-9)
  (srfi srfi-26))

(define (displayln . args) (apply display args) (newline))

(define (read* f)
  (let loop ([acc '()])
    (let ([form (read f)])
      (if (eof-object? form)
        (reverse acc)
        (loop (cons form acc))))))

(define-record-type module
  (make-module wit wasm)
  module?
  (wit module-wit)
  (wasm module-wasm))

(define (format-wit port indent form)
  (define (format-type type)
    (match type
      [('func . argsAndRets)
        (format port "func(")
        (let loop ([argsAndRets argsAndRets] [first #t])
          (match argsAndRets
            [([name type] . argsAndRets)
              (format port "~a~a: " (if first "" ", ") name)
              (format-type type)
              (loop argsAndRets #f)]
            [(or ('-> . rets) (and '() rets))
              (format port ")")
              (match rets
                [() #f]
                [(ret)
                  (format port " -> ")
                  (format-type ret)]
                [(ret . rets)
                  (format port " -> tuple<")
                  (format-type ret)
                  (for-each
                    (lambda (ret)
                      (format port ", ")
                      (format-type ret))
                    rets)
                  (format port ">")])]))]
      [(? symbol? name)
        (format port "~a" name)]))

  (format port "~a" (make-string indent #\space))
  (match form
    [((and (or 'package 'import) type) name)
      (format port "~a ~a;" type name)]
    [((and (or 'world 'interface) type) name . forms)
      (format port "~a ~a {\n" type name)
      (for-each (cut format-wit port (+ indent 2) <>) forms)
      (format port "}")]
    [('export name type)
      (format port "export ~a: " name)
      (format-type type)
      (format port ";")]
    )
  (newline port))

(define (process form)
  (match form
    [('wit wit ...)
      (make-module wit '())]
    [_
      (make-module '() (list form))]))

(define (process* forms)
  (define modules (map process forms))
  (make-module
    (concatenate (map module-wit modules))
    (concatenate (map module-wasm modules))))

(define (process-input path)
  (define out-wit (format #f "build/~a.wit" (basename path ".mwat")))
  (define out-wasm (format #f "build/~a.wat" (basename path ".mwat")))
  (format #t "Compiling ~a to ~a and ~a...\n" path out-wit out-wasm)
  (define module (process* (call-with-input-file path read*)))
  (call-with-output-file out-wit
    (lambda (port)
      (for-each (cut format-wit port 0 <>) (module-wit module))))
  (call-with-output-file out-wasm
    (cut pretty-print `(component ,@(module-wasm module)) <>)))

(for-each process-input (cdr (program-arguments)))
