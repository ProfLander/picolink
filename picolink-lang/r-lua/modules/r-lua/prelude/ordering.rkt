#lang picolink/r-lua/base

(require r-lua/prelude/definition
         r-lua/prelude/interface
         r-lua/prelude/s-lua)

(provide (all-defined))

(define-interface LessThan
  [lt __lt
     (λ (a b)
       (s-lua (#%lt a b)))])

(define < lt)

(define-interface LessEqual
  [le __le
      (λ (a b)
        (s-lua (#%le a b)))])

(define <= le)

(define (gt a b)
  (s-lua (#%gt a b)))

(define > gt)

(define (ge a b)
  (s-lua (#%ge a b)))

(define >= ge)
