#lang picolink/r-lua/base

(require (only prelude/equality
               eq?)

         (only lua
               print)

         prelude/surface)

(provide (all-defined))

(define = eq?)

(define (< a b)
  (#%intrinsic (#%lt a b)))

(define (> a b)
  (#%intrinsic (#%gt a b)))

(define (<= a b)
  (#%intrinsic (#%le a b)))

(define (>= a b)
  (#%intrinsic (#%ge a b)))

(define (%add a b)
  (#%intrinsic (#%add a b)))

(define-macro +

  [(_ a b rest ...+)
   #'(let ([add (+ a b)])
       (+ add rest ...))]

  [(_ a b)
   #'(%add a b)])

(define (%sub a b)
  (#%intrinsic (#%sub a b)))

(define (%neg a)
  (#%intrinsic (#%neg a)))

(define-macro -

  [(_ a b rest ...+)
   #'(let ([sub (- a b)])
       (- sub rest ...))]

  [(_ a b)
   #'(%sub a b)]

  [(_ a)
   #'(%neg a)])

(define (%mul a b)
  (#%intrinsic (#%mul a b)))

(define-macro *

  [(_ a b rest ...+)
   #'(let ([mul (* a b)])
       (* mul rest ...))]

  [(_ a b)
   #'(%mul a b)])

(define (%div a b)
  (#%intrinsic (#%div a b)))

(define-macro /

  [(_ a b rest ...+)
   #'(let ([div (/ a b)])
       (/ div rest ...))]

  [(_ a b)
   #'(%div a b)])

(define (expt a b)
  (#%intrinsic (#%exp a b)))

(define (modulo a b)
  (#%intrinsic (#%mod a b)))
