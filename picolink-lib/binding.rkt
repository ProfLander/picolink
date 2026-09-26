#lang racket/base

(require racket/set
         racket/string
         racket/generic
         racket/contract)

(provide (all-defined-out))

(struct binding (ident)
  #:methods gen:custom-write
  [(define/generic call-write-proc write-proc)

   (define (write-proc self out _mode)
     (define buf (open-output-string))
     (write (binding-ident self) buf)
     (define s (get-output-string buf))
     (display
      (if (string-prefix? s "#<syntax")
          (string-append "#<binding" (substring s 8))
          s)
      out))]
  
  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (and (binding? b)
          (recur (syntax-e (binding-ident a))
                 (syntax-e (binding-ident b)))))

   (define (hash-proc a recur)
     (recur (syntax-e (binding-ident a))))

   (define (hash2-proc a recur)
     (recur (syntax-e (binding-ident a))))])

(define/contract (binding-symbol binding)
  (-> binding? symbol?)
  (syntax-e (binding-ident binding)))

(define/contract (make-binding ident)
  (-> identifier? binding?)
  (binding ident))

(define/contract (lift-binding ident-or-binding)
  (-> (or/c identifier? binding?) binding?)
  (cond
    [(binding? ident-or-binding) ident-or-binding]
    [(identifier? ident-or-binding) (binding ident-or-binding)]))
