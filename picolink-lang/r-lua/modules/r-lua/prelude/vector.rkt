#lang picolink/r-lua/base

(require (only lua
               tostring)

         (only lua/table
               insert)

         (only lua/ext/table
               table-ref
               table-set!
               table-length)

         r-lua/prelude/arith
         r-lua/prelude/binding
         r-lua/prelude/definition
         r-lua/prelude/equality
         r-lua/prelude/for
         r-lua/prelude/if
         r-lua/prelude/interface
         r-lua/prelude/lambda
         r-lua/prelude/list
         r-lua/prelude/print
         r-lua/prelude/sequence
         r-lua/prelude/string
         r-lua/prelude/s-lua
         r-lua/prelude/type)

(provide (all-defined))

(define-type vector:type
  #:printed-name vector)

(define vector
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call ascribe
                         vector:type
                         (#%table #%vararg)))))))

(define (vector-ref self idx)
  (table-ref self (+ 1 idx)))

(define (vector-set! self idx val)
  (table-set! self (+ 1 idx) val))

(define vector-length table-length)

(define (vector-push vec val)
  (insert vec val)
  vec)

(impl ToString vector:type

      (let ([to-string
             (λ (self)
               (string-append
                (for/fold ([acc "#("])
                          ([i (in-range 0 (vector-length self))])
                  (let* ([val (vector-ref self i)]
                         [str (tostring val)])
                    (if (eq? i 0)
                        (string-append acc str)
                        (string-append acc " " str))))
                ")"))])

        (s-lua (#%table ["to-string" to-string]))))

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
