#lang picolink/r-lua/base

(require (only r-lua/prelude/arith            +)
         (only r-lua/prelude/assignment       set!)
         (only r-lua/prelude/binding          let)
         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/function         thunk)
         (only r-lua/prelude/if               if)
         (only r-lua/prelude/interface/order  < >)
         (only r-lua/prelude/struct           define-struct))

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
