#lang picolink/r-lua/base

(require (only lua
               setmetatable
               getmetatable
               tostring)

         (only lua/ext/table
               member
               length)

         prelude/arith
         prelude/binding
         prelude/equality
         prelude/for
         prelude/interface
         prelude/logic
         prelude/print
         prelude/surface
         prelude/string
         prelude/type
         prelude/void)

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
  (member self (+ 1 idx)))

(define (vector-length self idx)
  (length self))

(impl ToString vector:type

      (let ([to-string
             (λ (self)
               (string-append
                (for/fold ([acc "#("])
                          ([i (in-range 0 (- (length self) 1))])
                  (let* ([val (vector-ref self i)]
                         [str (tostring val)])
                    (if (eq? i 0)
                        (string-append acc str)
                        (string-append acc " " str))))
                ")"))])

        (s-lua (#%table ["to-string" to-string]))))

(define inst (vector 2 4 6 8))

(print "inst:")
inst

(print "(vector-length inst):")
(vector-length inst)

(print "(vector-ref inst 0):")
(vector-ref inst 0)

(print "(vector-ref inst 1):")
(vector-ref inst 1)

(print "(vector-ref inst 2):")
(vector-ref inst 2)

(print "(vector-ref inst 3):")
(vector-ref inst 3)
