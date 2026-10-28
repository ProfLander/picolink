#lang picolink/r-lua/base

(require (only prelude/equality
               eq?)

         (only lua
               print)

         prelude/surface)

(provide (all-defined))

(define = eq?)

(define (< a b)
  (s-lua (#%lt a b)))

(define (> a b)
  (s-lua (#%gt a b)))

(define (<= a b)
  (s-lua (#%le a b)))

(define (>= a b)
  (s-lua (#%ge a b)))

(define (%add a b)
  (s-lua (#%add a b)))

(define-macro +

  [(_ a b rest ...+)
   #'(let ([add (+ a b)])
       (+ add rest ...))]

  [(_ a b)
   #'(%add a b)])

(define (%sub a b)
  (s-lua (#%sub a b)))

(define (%neg a)
  (s-lua (#%neg a)))

(define-macro -

  [(_ a b rest ...+)
   #'(let ([sub (- a b)])
       (- sub rest ...))]

  [(_ a b)
   #'(%sub a b)]

  [(_ a)
   #'(%neg a)])

(define (%mul a b)
  (s-lua (#%mul a b)))

(define-macro *

  [(_ a b rest ...+)
   #'(let ([mul (* a b)])
       (* mul rest ...))]

  [(_ a b)
   #'(%mul a b)])

(define (%div a b)
  (s-lua (#%div a b)))

(define-macro /

  [(_ a b rest ...+)
   #'(let ([div (/ a b)])
       (/ div rest ...))]

  [(_ a b)
   #'(%div a b)])

(define (expt a b)
  (s-lua (#%exp a b)))

(define (modulo a b)
  (s-lua (#%mod a b)))
