#lang picolink/r-lua/base

(require (only lua
               tostring
               getmetatable
               setmetatable)

         (only lua/ext/table
               table-ref
               table-set!)

         (only lua/ext/values
               foldr-values)

         (only lua/string
               format)

         prelude/arith
         prelude/binding
         prelude/function
         prelude/equality
         prelude/hash
         prelude/interface
         prelude/logic
         prelude/print
         prelude/struct
         prelude/surface
         prelude/type
         prelude/void)

(provide null?
         null

         pair?
         cons
         car
         cdr
         cadr
         cddr

         list?
         list)

(define-struct %null []
  #:predicate-name null?)

(impl ToString %null:type
      (let ([to-string (const "()")])
        (s-lua (#%table ["to-string" to-string]))))

(define null (%null))

(define-struct pair [ar dr]
  #:constructor-name cons)

(define (car v)
  (pair-ref v 0))

(define (cdr v)
  (pair-ref v 1))

(define (cadr v)
  (car (cdr v)))

(define (cddr v)
  (cdr (cdr v)))

(define (list? v)
  (if (pair? v)
      (list? (cdr v))
      (null? v)))

(define (print-list self)
  (letrec ([%print-list
            (λ (self)
              (let ([lhs (car self)]
                    [rhs (cdr self)])
                (if (null? rhs)
                    (tostring lhs)
                    (format "%s %s" lhs (%print-list rhs)))))])

    (format "(%s)" (%print-list self))))

(define (print-pair self)
  (letrec ([%print-pair
            (λ (self)
              (let ([lhs (car self)]
                    [rhs (cdr self)])
                (if (pair? rhs)
                    (format "%s %s" lhs (%print-pair rhs))
                    (format "%s . %s" lhs rhs))))])

    (format "(%s)" (%print-pair self))))

(impl ToString pair:type
      (let ([to-string (λ (self)
                         (if (list? self)
                             (print-list self)
                             (print-pair self)))])
        (s-lua (#%table ["to-string" to-string]))))

(define list
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldr-values
                         cons null #%vararg))))))
