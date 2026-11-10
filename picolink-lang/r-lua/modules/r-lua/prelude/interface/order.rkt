#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/interface   define-interface)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define-interface Order
  [less-than? %less-than
              (λ (a b)
                (s-lua (#%lt a b)))]

  [less-or-equal? %less-or-equal
                  (λ (a b)
                    (s-lua (#%le a b)))])

(define (greater-than? a b)
  (s-lua (#%gt a b)))

(define (greater-or-equal? a b)
  (s-lua (#%ge a b)))

(define < less-than?)
(define <= less-or-equal?)
(define > greater-than?)
(define >= greater-or-equal?)
