#lang racket/base

(require racket/list
         racket/syntax
         syntax/parse
         (for-template picolink/r-lua/ir))

(provide (all-defined-out))

(define current-debug
  (make-parameter #f))

(define current-debug-binds
  (make-parameter #f))

(define (r-lua->r-lua-anf stx)

  (when (current-debug)
    (displayln "r-lua->r-lua-anf:")
    (println (syntax->datum stx))
    (displayln ""))

  (syntax-parse stx
    [mod:module-level
     #'mod.anf]))

(define-syntax-class datum
  (pattern (~or _:id
                _:boolean
                _:string
                _:number)))

(define-syntax-class lambda
  #:literal-sets [r-lua-ir-literals]
  (pattern (#%abs ~! args body:expr)))

(define datum?
  (syntax-parser
    [_:datum #t]
    [_ #f]))

(define lambda?
  (syntax-parser
    [_:lambda #t]
    [_ #f]))

(define (must-bind? stx)
  (or (datum? stx)
      (lambda? stx)))

(define-syntax-class module-level
  #:literal-sets [r-lua-ir-literals]

  (pattern (#%begin ~! top:top-level ...)

           #:attr anf
           (in-r-lua
            (syntax/loc this-syntax
              (#%begin top.anf ...)))))

(define-syntax-class top-level
  #:literal-sets [r-lua-ir-literals]

  (pattern (#%begin ~! top:top-level ...)

           #:attr atom (and (= 1 (length (attribute top)))
                            (car (attribute top.atom)))

           #:attr anf
           (if (< 1 (length (attribute top)))

               (in-r-lua
                (syntax/loc this-syntax
                  (#%begin top.anf ...)))

               (car (attribute top.anf))))

  (pattern (#%define ~! ident expr:expr)

           #:attr atom #f

           #:attr anf
           (in-r-lua
            (syntax/loc this-syntax
              (#%define ident expr.anf))))

  (pattern (~and temp
                 (~parse expr:expr
                         (in-r-lua
                           #'(#%app print temp))))

           #:cut

           #:attr atom (attribute expr.atom)

           #:attr anf #'expr.anf))

(define-syntax-class expr

  #:literal-sets [r-lua-ir-literals]

  (pattern (~or _:id
                _:boolean
                _:string
                _:number)

           #:cut

           #:attr atom #t

           #:attr anf this-syntax)

  (pattern (#%begin ~! body ...)

           #:with (head:expr ...) (drop-right (attribute body) 1)
           #:with tail:expr (last (attribute body))

           #:attr atom (and (= 0 (length (attribute head)))
                            (attribute tail.atom))

           #:attr anf
           (if (< 0 (length (attribute head)))

               (for/foldr ([acc #'tail.anf])
                          ([head (in-list (attribute head.anf))])

                 (if (must-bind? head)

                     #`(#%let-values ([(_) #,head])
                                     #,acc)

                     #`(#%begin
                        #,head
                        #,acc)))

               (attribute tail.anf)))

  (pattern (#%if ~! cond:expr
                 then:expr
                 else:expr)

           #:attr atom #f

           #:attr anf
           (if (attribute cond.atom)

               #'(#%if cond.anf
                       then.anf
                       else.anf)

               (with-syntax ([cond-name (generate-temporary 'cond)])
                 #'(#%let-values ([(cond-name) cond.anf])
                                 (#%if cond-name
                                       then.anf
                                       else.anf)))))

  (pattern (#%let-values ~! ([(ident:id ...) expr:expr] ...)
                         body:expr)

           #:attr atom (and (= 0 (length (attribute expr)))
                            (attribute body.atom))

           #:attr anf
           (if (< 0 (length (attribute expr)))

               (in-r-lua
                (quasisyntax/loc this-syntax
                  (#%let-values ([(ident ...) expr.anf] ...)
                                body.anf)))

               (attribute body.anf)))

  (pattern (#%letrec-values ~!
                            ([(ident:id ...) expr:expr] ...)
                            body:expr)

           #:attr atom (and (= 0 (length (attribute expr)))
                            (attribute body.atom))

           #:attr anf
           (if (< 0 (length (attribute expr)))

               (in-r-lua
                (quasisyntax/loc this-syntax
                  (#%letrec-values ([(ident ...) expr.anf] ...)
                                   body.anf)))

               (attribute body.anf)))

  (pattern (#%set! ~! ident expr:expr)

           #:attr atom #f

           #:attr anf
           (in-r-lua
            (syntax/loc this-syntax
              (#%begin
               (#%set! ident expr.anf)
               (#%app void)))))

  (pattern (#%abs ~! args body:expr)

           #:attr atom #t

           #:attr anf
           (in-r-lua
            (syntax/loc this-syntax
              (#%abs args body.anf))))

  (pattern (#%app ~! arg:expr ...)

           #:do 
           [(define-values (binds args)

              (let-values
                  ([(binds args)
                    (for/fold ([binds null]
                               [args  null])

                              ([atom (in-list (attribute arg.atom))]
                               [arg  (in-list (attribute arg.anf))])

                      (if atom
                          (values binds (cons arg args))
                          (let ([ident (generate-temporary 'arg)])
                            (values (cons #`((#,ident) #,arg) binds)
                                    (cons ident args)))))])

                (values (reverse binds)
                        (reverse args))))]

           #:attr atom (= 0 (length binds))

           #:attr anf
           (if (attribute atom)

               (in-r-lua
                (quasisyntax/loc this-syntax
                  (#%app #,@args)))

               (in-r-lua
                (quasisyntax/loc this-syntax
                  (#%let-values (#,@binds)
                                (#%app #,@args))))))

  (pattern (#%s-lua ~! form)
           #:attr atom #f
           #:attr anf this-syntax))
