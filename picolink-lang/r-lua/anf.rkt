#lang racket/base

(require racket/list
         racket/match
         racket/syntax
         syntax/parse
         (for-template picolink/r-lua/ir))

(provide (all-defined-out))

(define current-debug
  (make-parameter #f))

(struct let-bind [ident expr]
  #:transparent)

(struct letrec-bind [ident expr]
  #:transparent)

(struct declaration [ident]
  #:transparent)

(struct statement [expr]
  #:transparent)

(struct close-bind []
  #:transparent)

(define (bind-expr bind)
  (cond
    [(let-bind? bind)    (let-bind-expr bind)]
    [(letrec-bind? bind) (letrec-bind-expr bind)]
    [(declaration? bind) (error "declaration has no expression part")]
    [(statement? bind)    (statement-expr bind)]))

(define (r-lua->r-lua-anf stx)

  (when (current-debug)
    (displayln "r-lua->r-lua-anf:")
    (println stx))

  (syntax-parse stx
    [mod:module-level
     #'mod.anf]))

(define-syntax-class module-level
  #:literal-sets [r-lua-ir-literals]

  (pattern (#%begin ~! top:top-level ...)

           #:attr binds null

           #:attr atom
           (in-r-lua
             (syntax/loc this-syntax
               (#%begin top.anf ...)))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom))))

(define-syntax-class top-level
  #:literal-sets [r-lua-ir-literals]

  (pattern (#%define-macro ~! _ ...)
           #:attr binds null
           #:attr atom this-syntax
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%define ~! ident expr:expr)

           #:attr binds null

           #:attr atom
           (in-r-lua
             (syntax/loc this-syntax
               (#%define ident expr.anf)))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (~and temp
                 (~parse expr:expr
                         (in-r-lua
                           #'(#%app print temp))))

           #:cut

           #:attr binds (attribute expr.binds)
           #:attr atom (attribute expr.atom)
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom))))

(define-syntax-class expr

  #:literal-sets [r-lua-ir-literals]

  (pattern (~or _:id
                _:boolean
                _:string
                _:number
                #%void)

           #:cut

           #:attr binds null
           #:attr atom this-syntax
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%begin ~! body:expr ...)

           #:do [(define %binds
                   (apply append
                          (for/list ([binds (attribute body.binds)]
                                     [atom (attribute body.atom)])
                            (append binds
                                    (list (close-bind)
                                          (let-bind (generate-temporary) atom))))))]

           #:attr binds (drop-right %binds 2)

           #:attr atom (let-bind-expr (last %binds))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%if ~! cond:expr then:expr else:expr)

           #:do [(define atom (generate-temporary))

                 (define then-anf
                   (binds+atom->r-lua-anf
                    (attribute then.binds)
                    (in-r-lua
                     (with-syntax ([atom atom])
                       #'(#%set! atom then.atom)))))

                 (define else-anf
                   (binds+atom->r-lua-anf
                    (attribute else.binds)
                    (in-r-lua
                     (with-syntax ([atom atom])
                       #'(#%set! atom else.atom)))))]

           #:attr binds
           (append (attribute cond.binds)
                   (list
                    (declaration atom)
                    (statement
                     (in-r-lua
                      #`(#%if cond.atom
                              #,then-anf
                              #,else-anf)))))

           #:attr atom atom

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%let ~! ([ident:id expr:expr] ...)
                  body:expr)

           #:attr binds
           (append (apply append (attribute expr.binds))

                   (list (close-bind))

                   (apply append
                          (list (map (syntax-parser
                                       [(ident atom)
                                        (let-bind #'ident #'atom)])
                                     (syntax-e
                                      #'([ident expr.atom] ...)))))

                   (attribute body.binds))

           #:attr atom (syntax/loc this-syntax body.atom)

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%letrec ~! ([ident:id expr:expr] ...)
                     body:expr)

           #:attr binds
           (append (apply append (attribute expr.binds))

                   (list (close-bind))

                   (apply append
                          (list (map (syntax-parser
                                       [(ident atom)
                                        (letrec-bind #'ident #'atom)])
                                     (syntax-e
                                      #'([ident expr.atom] ...)))))

                   (attribute body.binds))

           #:attr atom (syntax/loc this-syntax body.atom)

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%set! ~! ident expr:expr)

           #:attr binds (append (attribute expr.binds)
                                (list (statement this-syntax)))

           #:attr atom
           (in-r-lua
            (syntax/loc this-syntax
              #%void))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%abs ~! (arg ...) body:expr ...)

           #:attr binds null
           
           #:attr atom
           (in-r-lua
            (syntax/loc this-syntax
              (#%abs (arg ...)
                     body.anf ...)))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%app ~! expr:expr ...)

           #:attr binds
           (apply append (attribute expr.binds))

           #:attr atom
           (in-r-lua
             #'(#%app expr.atom ...))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%intrinsic ~! form)

           #:attr binds null
           
           #:attr atom this-syntax

           #:attr anf this-syntax))

(define (binds+atom->r-lua-anf binds atom)

  (when (current-debug)
    (displayln "binds+atom->r-lua-anf:")
    (displayln "binds:")
    (println binds)
    (displayln "atom:")
    (println atom))

  (if (pair? binds)

      (for/foldr ([stx atom]
                  [open #t]
                  #:result stx)
                 ([bind (in-list binds)])
        (cond
          [(let-bind? bind)
           (with-syntax ([ident (let-bind-ident bind)]
                         [expr  (let-bind-expr bind)])
             (syntax-parse stx
               #:literal-sets [r-lua-ir-literals]
               [(#%let ([stx-ident stx-expr] ...)
                       body)
                #:when open
                #:cut
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%let ([ident expr]
                                   [stx-ident stx-expr]
                                   ...)
                                  body)))
                        open)]
               [stx
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%let ([ident expr])
                                  stx)))
                        #t)]))]

          [(letrec-bind? bind)
           (with-syntax ([ident    (letrec-bind-ident bind)]
                         [expr     (letrec-bind-expr bind)])
             (syntax-parse stx
               #:literal-sets [r-lua-ir-literals]
               [(#%letrec ([stx-ident stx-expr] ...)
                          body)
                #:when open
                #:cut
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%letrec ([ident expr]
                                      [stx-ident stx-expr]
                                      ...)
                                     body)))
                        open)]
               [stx
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%letrec ([ident expr])
                                     stx)))
                        #t)]))]

          [(declaration? bind)
           (with-syntax ([ident (declaration-ident bind)])
             (syntax-parse stx
               #:literal-sets [r-lua-ir-literals]
               [(#%declare (stx-ident ...)
                           body)
                #:when open
                #:cut
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%declare (ident stx-ident ...)
                                      body)))
                        open)]
               [stx
                (values (in-r-lua
                         (syntax/loc this-syntax
                           (#%declare (ident)
                                      stx)))
                        #t)]))]

          [(statement? bind)
           (with-syntax ([stmt (statement-expr bind)]
                         [stx stx])

             (values (in-r-lua
                      (syntax/loc #'stmt
                        (#%begin
                         stmt
                         stx)))
                     #f))]

          [(close-bind? bind)
           (values stx #f)]))

      atom))
