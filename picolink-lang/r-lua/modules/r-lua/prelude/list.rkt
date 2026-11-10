#lang picolink/r-lua/base

(require (only lua/ext/values foldr-values)

         (only r-lua/prelude/arith             +)
         (only r-lua/prelude/assignment        set!)
         (only r-lua/prelude/binding           let letrec)
         (only r-lua/prelude/condition         unless)
         (only r-lua/prelude/definition        define)
         (only r-lua/prelude/error             error)
         (only r-lua/prelude/format            format)
         (only r-lua/prelude/function          const thunk)
         (only r-lua/prelude/if                if)
         (only r-lua/prelude/interface         impl)
         (only r-lua/prelude/interface/length  Length length)
         (only r-lua/prelude/lambda            λ)
         (only r-lua/prelude/print             print)
         (only r-lua/prelude/string            ToString to-string)
         (only r-lua/prelude/struct            define-struct)
         (only r-lua/prelude/sequence          sequence-null)
         (only r-lua/prelude/table             table))

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

(impl [ToString %null:type]
       (define to-string (const "()")))

(impl [Length %null:type]
      (define length (const 0)))

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
                    (to-string lhs)
                    (format "~a ~a" lhs (%print-list rhs)))))])

    (format "(~a)" (%print-list self))))

(define (print-pair self)
  (letrec ([%print-pair
            (λ (self)
              (let ([lhs (car self)]
                    [rhs (cdr self)])
                (if (pair? rhs)
                    (format "~a ~a" lhs (%print-pair rhs))
                    (format "~a . ~a" lhs rhs))))])

    (format "(~a)" (%print-pair self))))

(impl [ToString pair:type]
      (define (to-string self)
        (if (list? self)
            (print-list self)
            (print-pair self))))

(impl [Length pair:type]
      (define length
        (letrec ([length
                  (λ (lst)

                    (unless (list? lst)
                      (error (format "value is not a list: ~a" lst)))

                    (%length 0 lst))]

                 [%length
                  (λ (acc lst)
                    (if (pair? lst)
                        (+ acc 1 (length (cdr lst)))
                        acc))])

          length)))

(define (list . [: vs #%values])
  (foldr-values cons null vs))

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
