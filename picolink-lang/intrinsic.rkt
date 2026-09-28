#lang racket/base

(require racket/contract
         racket/function
         racket/struct

         syntax/parse

         picolink/language
         picolink/binding)

(provide (all-defined-out))

(struct intrinsic (path provides)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'intrinsic)
      (λ (self) (list (cons 'path (intrinsic-path self))
                      (cons 'provides (intrinsic-provides self))))))]

  #:methods gen:language
  [(define (source self)
     #f)

   (define (provides self)
     (intrinsic-provides self))

   (define (compiler self _name)
     (const self))])

(define/contract (make-intrinsic path stx)
  (-> syntax? syntax? intrinsic?)
  (let ([path (syntax-parse path
                #:datum-literals [quote]
                ['(~datum same) 'same]
                [_:id         this-syntax]
                [#f           #f])])
    (intrinsic path
               (make-module-provides
                (map make-binding (syntax-e stx))))))
