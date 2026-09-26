#lang racket/base

(require racket/contract
         racket/function
         racket/set

         syntax/parse

         picolink/language
         picolink/binding)

(provide (all-defined-out))

(struct intrinsic (path source)
  #:transparent
  #:methods gen:language
  [(define (source self)
     (intrinsic-source self))

   (define (provides self)
     (make-module-provides (map make-binding (syntax-e (source self)))))

   (define (compiler self _name)
     (const self))])

(define/contract (make-intrinsic path stx)
  (-> syntax? syntax? intrinsic?)
  (let ([path (syntax-parse path
                #:datum-literals [quote]
                [(quote same) 'same]
                [_:id         this-syntax]
                [#f           #f])])
    (intrinsic path stx)))
