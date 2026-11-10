#lang picolink/r-lua/base

(require (only lua/table  insert)

         (only lua/ext/table  table-ref table-set! table-length)

         (only r-lua/prelude/arith      +)
         (only r-lua/prelude/binding    let*)
         (only r-lua/prelude/definition define)
         (only r-lua/prelude/equality   equal?)
         (only r-lua/prelude/for        for/fold)
         (only r-lua/prelude/if         if)
         (only r-lua/prelude/interface  impl)
         (only r-lua/prelude/lambda     λ)
         (only r-lua/prelude/list       for/list in-list)
         (only r-lua/prelude/print      print)
         (only r-lua/prelude/sequence   sequence-map in-range)
         (only r-lua/prelude/string     ToString to-string string-append)
         (only r-lua/prelude/table      table)
         (only r-lua/prelude/type       define-type ascribe))

(provide (all-defined))

(define-type vector:type
  #:printed-name vector)

(define (vector . [: vs values])
  (ascribe vector:type (table vs)))

(define (vector-ref self idx)
  (table-ref self (+ 1 idx)))

(define (vector-set! self idx val)
  (table-set! self (+ 1 idx) val))

(define vector-length table-length)

(define (vector-push vec val)
  (insert vec val)
  vec)

(impl ToString vector:type
      (define (to-string self)
        (string-append
         (for/fold ([acc "#("])
                   ([i (in-range 0 (vector-length self))])
           (let* ([val (vector-ref self i)]
                  [str (to-string val)])
             (if (equal? i 0)
                 (string-append acc str)
                 (string-append acc " " str))))
         ")")))

(define (in-vector vec)
  (sequence-map (in-range 0 (vector-length vec))
                (λ (idx)
                  (vector-ref vec idx))))

(define-macro for/vector
  [(_ (for-clause ...) body ...)

   (syntax/loc this-syntax
     (for/fold ([acc (vector)])
               (for-clause ...)
       (vector-push acc (begin body ...))))])

(define (list->vector lst)
  (for/vector ([v (in-list lst)])
    v))

(define (vector->list vec)
  (for/list ([v (in-vector vec)])
    v))
