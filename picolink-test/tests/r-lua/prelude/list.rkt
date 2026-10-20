#lang picolink/r-lua

(require (only lua
               setmetatable
               print)

         (only lua/string
               format)

         (only prelude/tag
               tag?
               tag)

         (only prelude/equality
               eq?)

         (only prelude/vector
               %vector)

         (only prelude/hash
               %hash))

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

(define cell (cons 1 (cons 2 null)))

(print "cell:")
cell

(print "null?:")
(null? cell)

(print "pair?:")
(pair? cell)

(print "car:")
(car cell)

(print "cdr:")
(cdr cell)

(print "cadr:")
(cadr cell)

(print "cddr:")
(cddr cell)
