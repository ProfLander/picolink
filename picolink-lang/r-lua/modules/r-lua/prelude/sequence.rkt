#lang picolink/r-lua/base

(require (only lua/ext/ops + < >)

         (only r-lua/prelude/assignment           set!)
         (only r-lua/prelude/binding              let)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/function             thunk const)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/unit                 define-unit))

(provide (all-defined))

(define-unit sequence-null)

(impl [ToString sequence-null:type]
      (define to-string (const "#<sequence-null>")))

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
         sequence-null))))
