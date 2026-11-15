#lang picolink/r-lua/base

(require (only lua/ext/values  foldr-values)

         (only r-lua/runtime/pool/type  copy-handle! drop-handle!)

         (only r-lua/prelude/arith                +)
         (only r-lua/prelude/assignment           set!)
         (only r-lua/prelude/binding              let letrec)
         (only r-lua/prelude/condition            unless)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/error                error)
         (only r-lua/prelude/format               format)
         (only r-lua/prelude/function             const thunk)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/length     Length length)
         (only r-lua/prelude/interface/to-string  ToString to-string)
         (only r-lua/prelude/lambda               λ)
         (only r-lua/prelude/struct               define-struct)
         (only r-lua/prelude/sequence             sequence-null)
         (only r-lua/prelude/unit                 define-unit)
         (only r-lua/prelude/type                 define-type))

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

(define-unit null)

(impl [Copy  null:type])
(impl [Drop  null:type])
(impl [Equal null:type])

(impl [ToString null:type]
      (define to-string (const "()")))

(impl [Length null:type]
      (define length (const 0)))

(define-type pair:type)

(define-struct pair [ar dr]
  #:type pair:type
  #:constructor-name cons)

(define car pair-ar)

(define cdr pair-dr)

(define (cadr v)
  (car (cdr v)))

(define (cddr v)
  (cdr (cdr v)))

(define (list? v)
  (if (pair? v)
      (list? (cdr v))
      (null? v)))

(impl [Copy pair:type]
      (define copy copy-handle!))

(impl [Drop pair:type]
      (define drop drop-handle!))

(impl [Equal pair:type])

(define (list->string self)
  (letrec ([%list->string
            (λ (self)
              (let ([lhs (car self)]
                    [rhs (cdr self)])
                (if (null? rhs)
                    (to-string lhs)
                    (format "~a ~a" lhs (%list->string rhs)))))])

    (format "(~a)" (%list->string self))))

(define (pair->string self)
  (letrec ([%pair->string
            (λ (self)
              (let ([lhs (car self)]
                    [rhs (cdr self)])
                (if (pair? rhs)
                    (format "~a ~a" lhs (%pair->string rhs))
                    (format "~a . ~a" lhs rhs))))])

    (format "(~a)" (%pair->string self))))

(impl [ToString pair:type]
      (define (to-string self)
        (if (list? self)
            (list->string self)
            (pair->string self))))

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
         sequence-null
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
