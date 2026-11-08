#lang picolink/r-lua/base

(require (only lua
               tostring
               error)

         (only lua/ext/values
               foldr-values)

         (only lua/string
               format)

         r-lua/prelude/arith
         r-lua/prelude/assignment
         r-lua/prelude/binding
         r-lua/prelude/condition
         r-lua/prelude/definition
         r-lua/prelude/function
         r-lua/prelude/if
         r-lua/prelude/interface
         r-lua/prelude/interface/length
         r-lua/prelude/lambda
         r-lua/prelude/print
         r-lua/prelude/string
         r-lua/prelude/struct
         (only r-lua/prelude/sequence
               sequence-null)
         r-lua/prelude/s-lua)

(provide null?
         null

         pair?
         cons
         car
         cdr
         cadr
         cddr

         list?
         list

         reverse

         in-list
         for/list)

(define-struct %null []
  #:predicate-name null?)

(impl ToString %null:type
      (let ([to-string (const "()")])
        (s-lua (#%table ["to-string" to-string]))))

(impl Length %null:type
      (let ([length (const 0)])
        (s-lua (#%table ["length" length]))))

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

(impl Length pair:type

      (letrec ([%length
                (λ (acc lst)
                  (if (pair? lst)
                      (+ acc 1 (length* (cdr lst)))
                      acc))]

               [length*
                (λ (lst)

                  (unless (list? lst)
                    (error (format "value is not a list: %s" lst)))

                  (%length 0 lst))])

        (s-lua (#%table ["length" length*]))))

(define list
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldr-values
                         cons null #%vararg))))))

(define (reverse lst)
  (let loop ([acc null]
             [lst lst])
    (if (null? lst)
        acc
        (loop (cons (car lst) acc)
              (cdr lst)))))

(define (in-list lst)
  (let ([acc lst])
    (thunk
     (if (null? acc)
         (sequence-null)
         (begin
           (let ([out (car acc)])
             (set! acc (cdr acc))
             out))))))

(define-macro for/list
  [(_ (for-clause ...) body ...)

   (syntax/loc this-syntax
     (reverse
      (for/fold ([acc null])
                (for-clause ...)
        (cons (begin body ...) acc))))])
