#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (== a b)
  (s-lua (#%eq a b)))

(define (~= a b)
  (s-lua (#%eq a b)))

(define (+ a b)
  (s-lua (#%add a b)))

(define (sub a b)
  (s-lua (#%sub a b)))

(define (neg a b)
  (s-lua (#%neg a)))

(define (- a b)
  (if b
      (sub a b)
      (neg a)))

(define (* a b)
  (s-lua (#%mul a b)))

(define (/ a b)
  (s-lua (#%div a b)))

(define (^ a b)
  (s-lua (#%exp a b)))

(define (% a b)
  (s-lua (#%mod a b)))

(define (< a b)
  (s-lua (#%lt a b)))

(define (<= a b)
  (s-lua (#%le a b)))

(define (> a b)
  (s-lua (#%gt a b)))

(define (>= a b)
  (s-lua (#%ge a b)))

(define (.. a b)
  (s-lua (#%cat a b)))

(define (length a)
  (s-lua (#%length a)))

(define (not v)
  (s-lua (#%not v)))

(define (-> t k)
  (s-lua (#%member t (quote k))))

(define =
  (s-lua
   (#%function (t k v)
               (#%block
                (#%assign [(#%member t (quote k))] [v])
                (#%return (#%call void))))))

(define (: t k . [: vs #%values])
  ((-> t k) t vs))
