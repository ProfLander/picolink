#lang picolink/r-lua/base

(require (only lua
               setmetatable
               print)

         (only lua/string
               format)

         prelude/tag
         prelude/equality
         prelude/vector
         prelude/hash
         prelude/void)

(provide null?
         null

         pair?
         cons

         car
         cdr
         cadr
         cddr)

(define %tag-null "null")

(define null?
  (λ (v)
    (eq? (tag v) %tag-null)))

(define null

  (let ([index     (λ (self key)
                     (if (tag? key)
                         %tag-null
                         (void)))]

        [to-string (λ (self) "()")])

    (setmetatable (%vector)
                  (%hash __index    index
                         __tostring to-string))))

(define car
  (λ (v)
    (#%intrinsic (#%member v 1))))

(define cdr
  (λ (v)
    (#%intrinsic (#%member v 2))))

(define cadr
  (λ (v)
    (car (cdr v))))

(define cddr
  (λ (v)
    (cdr (cdr v))))

(define %tag-pair "pair")

(define pair?
  (λ (v)
    (eq? (tag v) %tag-pair)))

(define cons
  (λ (a b)

    (let ([index     (λ (self key)
                       (if (tag? key)
                           %tag-pair
                           (void)))]

          [to-string (λ (self)
                       (let ([lhs (car self)]
                             [rhs (cdr self)])
                         (format "(%s . %s)" lhs rhs)))])

      (setmetatable (%vector a b)
                    (%hash __index    index
                           __tostring to-string)))))

