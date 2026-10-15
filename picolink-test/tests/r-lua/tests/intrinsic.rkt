#lang picolink/r-lua

(require (in lua setmetatable))

; FIXME: Function return values do not lift
; FIXME: `quote` cannot be used in #%intrinsic due to literal space

#;(define cons-index
    (λ (key)
      (if (#%intrinsic (#%eq key "__type"))
          "pair"
          (void))))

(define cons
  (λ (a b)
    (setmetatable (#%intrinsic
                   (#%table a b))

                  (#%intrinsic
                   (#%table #;[__index cons-index])))))

(let ([a 1]
      [b 2]
      [c 3])

  (cons a (cons b c)))
