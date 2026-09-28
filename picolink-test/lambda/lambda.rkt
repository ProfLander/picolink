#lang picolink/lambda

(require (in lua print))

(define foo 1234)

;; FIXME: Nested lets don't hoist
#;(print
   (let ([a (let ([b 2]
                  [a 3])
              b)])))

(let ([a (λ (x) x)]
      [b (λ (x) (λ (x) x))]
      [c (λ (x) (x))]
      [d (λ (x) (λ () x))])
  (print ((a (b 1)) (c (d 2)))))
