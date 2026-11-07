#lang picolink/r-lua/base

(require (only lua/string format)
         r-lua/prelude/arith
         r-lua/prelude/assignment
         r-lua/prelude/binding
         r-lua/prelude/condition
         r-lua/prelude/definition
         r-lua/prelude/function
         r-lua/prelude/if
         r-lua/prelude/print
         r-lua/prelude/struct)

(provide (all-defined))

(define-struct sequence-null [])

(define (sequence-map seq f)
  (thunk
   (let ([idx (seq)])
     (if (sequence-null? idx)
         idx
         (f idx)))))

(define (in-naturals [start 0])

  (let ([acc start])
    (thunk
      (let ([out acc])
        (set! acc (+ acc 1))
        out))))

(define (in-range from to [step 1])
  (let ([acc from]
        [cmp (if (< 0 step) < >)])
    (thunk
     (if (cmp acc to)
         (let ([out acc])
           (set! acc (+ acc step))
           out)
         (sequence-null)))))
