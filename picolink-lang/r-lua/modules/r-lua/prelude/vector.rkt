#lang picolink/r-lua/base

(require (only lua
               setmetatable
               getmetatable
               tostring)

         (only lua/table
               insert)

         (only lua/ext/table
               table-ref
               table-set!
               table-length)

         r-lua/prelude/arith
         r-lua/prelude/binding
         r-lua/prelude/equality
         r-lua/prelude/for
         r-lua/prelude/interface
         r-lua/prelude/logic
         r-lua/prelude/print
         r-lua/prelude/surface
         r-lua/prelude/string
         r-lua/prelude/type
         r-lua/prelude/void)

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

(define vector-push insert)

(impl ToString vector:type

      (let ([to-string
             (λ (self)
               (string-append
                (for/fold ([acc "#("])
                          ([i (in-range 0 (- (vector-length self) 1))])
                  (let* ([val (vector-ref self i)]
                         [str (tostring val)])
                    (if (eq? i 0)
                        (string-append acc str)
                        (string-append acc " " str))))
                ")"))])

        (s-lua (#%table ["to-string" to-string]))))
