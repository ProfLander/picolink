#lang picolink/r-lua/base

(require (only prelude/equality
               eq?)

         (only lua
               print))

(provide (all-defined))

(define = eq?)

(define <
  (λ (a b)
    (#%intrinsic (#%lt a b))))

(define >
  (λ (a b)
    (#%intrinsic (#%gt a b))))

(define <=
  (λ (a b)
    (#%intrinsic (#%le a b))))

(define >=
  (λ (a b)
    (#%intrinsic (#%ge a b))))

(define %add
  (λ (a b)
    (#%intrinsic (#%add a b))))

(define-macro +

  [(_ a b rest ...+)
   #'(let ([add (+ a b)])
       (+ add rest ...))]

  [(_ a b)
   #'(%add a b)])

(define %sub
  (λ (a b)
    (#%intrinsic (#%sub a b))))

(define %neg
  (λ (a)
    (#%intrinsic (#%neg a))))

(define-macro -

  [(_ a b rest ...+)
   #'(let ([sub (- a b)])
       (- sub rest ...))]

  [(_ a b)
   #'(%sub a b)]

  [(_ a)
   #'(%neg a)])

(define %mul
  (λ (a b)
    (#%intrinsic (#%mul a b))))

(define-macro *

  [(_ a b rest ...+)
   #'(let ([mul (* a b)])
       (* mul rest ...))]

  [(_ a b)
   #'(%mul a b)])

(define %div
  (λ (a b)
    (#%intrinsic (#%div a b))))

(define-macro /

  [(_ a b rest ...+)
   #'(let ([div (/ a b)])
       (/ div rest ...))]

  [(_ a b)
   #'(%div a b)])

(define expt
  (λ (a b)
    (#%intrinsic (#%exp a b))))

(define modulo
  (λ (a b)
    (#%intrinsic (#%mod a b))))
