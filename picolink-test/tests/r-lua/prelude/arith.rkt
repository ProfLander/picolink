#lang picolink/r-lua

(require (only prelude/equality
               eq?)

         (only lua
               print))

(provide =
         <
         >
         <=
         >=
         +
         -
         *
         /
         expt
         modulo)

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

(print "+:")
(+ 1 2 3 4)

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

(print "-:")
(- 10 4 2 1)
(- 10)

(define %mul
  (λ (a b)
    (#%intrinsic (#%mul a b))))

(define-macro *
  [(_ a b rest ...+)
   #'(let ([mul (* a b)])
       (* mul rest ...))]
  [(_ a b)
   #'(%mul a b)])

(print "*:")
(* 2 2 2)

(define %div
  (λ (a b)
    (#%intrinsic (#%div a b))))

(define-macro /
  [(_ a b rest ...+)
   #'(let ([div (/ a b)])
       (/ div rest ...))]
  [(_ a b)
   #'(%div a b)])

(print "/:")
(/ 8 2 2)

(define expt
  (λ (a b)
    (#%intrinsic (#%exp a b))))

(define modulo
  (λ (a b)
    (#%intrinsic (#%mod a b))))
