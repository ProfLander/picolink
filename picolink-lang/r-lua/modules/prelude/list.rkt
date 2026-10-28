#lang picolink/r-lua/base

(require (only lua
               tostring
               setmetatable
               print)

         (only lua/ext/table
               member)

         (only lua/string
               format)

         prelude/binding
         prelude/function
         prelude/equality
         prelude/hash
         prelude/logic
         prelude/surface
         prelude/tag
         prelude/vector
         prelude/void)

(provide null?
         null

         pair?
         cons

         list?
         list

         car
         cdr
         cadr
         cddr

         vararg->list)

(define %tag-null "null")

(define (null? v)
  (eq? (tag v) %tag-null))

(define null

  (let ([index     (λ (self key)
                     (if (tag? key)
                         %tag-null
                         (void)))]

        [to-string (const "()")])

    (setmetatable (%vector)
                  (%hash __index    index
                         __tostring to-string))))

(define (car v)
  (member v 1))

(define (cdr v)
  (member v 2))

(define (cadr v)
  (car (cdr v)))

(define (cddr v)
  (cdr (cdr v)))

(define %tag-pair "pair")

(define (pair? v)
  (eq? (tag v) %tag-pair))

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

(define (cons a b)
  (let* ([index      (λ (self key)
                       (if (tag? key)
                           %tag-pair
                           (void)))]

         [to-string  (λ (self)
                       (if (list? self)
                           (print-list self)
                           (print-pair self)))])

    (setmetatable (%vector a b)
                  (%hash __index    index
                         __tostring to-string))))

(define-macro list

  [(_ a rest ...+)
   #'(let ([tail (list rest ...)])
       (cons a tail))]

  [(_ a)
   #'(cons a null)])

(define vararg->list
  (#%intrinsic
   (#%function (#%vararg)
               (#%block
                (#%if (#%gt (#%call select "#" #%vararg) 0)

                      (#%then
                       (#%block
                        (#%return
                         (#%call cons
                                 '(#%call select 1 #%vararg)
                                 (#%call vararg->list
                                         (#%call select 2 #%vararg))))))

                      (#%else
                       (#%block
                        (#%return null))))))))
