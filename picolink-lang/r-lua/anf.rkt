#lang racket/base

(require racket/list
         racket/syntax
         syntax/parse)

(provide (all-defined-out))

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

  (syntax-parse stx
    [mod:module-level
     #'mod.anf]))

(define-syntax-class module-level
  #:datum-literals [#%begin]

  (pattern (#%begin top:top-level ...)

           #:attr binds null

           #:attr atom
           (syntax/loc this-syntax
             (#%begin top.anf ...))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom))))

(define-syntax-class top-level
  #:datum-literals [#%define
                    #%define-syntax]

  (pattern (#%define-syntax _ ...)
           #:attr binds null
           #:attr atom this-syntax
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%define ident expr:expr)

           #:attr binds null

           #:attr atom
           (syntax/loc this-syntax
             (#%define ident expr.anf))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (~and temp
                 (~parse expr:expr
                         #'(#%app print temp)))
           #:attr binds (attribute expr.binds)
           #:attr atom (attribute expr.atom)
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom))))

(define-syntax-class expr

  #:datum-literals [#%let
                    #%letrec
                    #%begin
                    #%set!
                    #%abs
                    #%app
                    #%if
                    #%void]

  (pattern (~or _:id
                _:boolean
                _:string
                _:number
                #%void)
           #:attr binds null
           #:attr atom this-syntax
           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%begin body:expr ...)

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

  (pattern (#%let ([ident:id expr:expr] ...)
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

  (pattern (#%letrec ([ident:id expr:expr] ...)
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

  (pattern (#%set! ident expr:expr)

           #:attr binds (append (attribute expr.binds)
                                (list (statement this-syntax)))

           #:attr atom
           (syntax/loc this-syntax
             #%void)

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%if cond:expr then:expr else:expr)

           #:do [(define atom (generate-temporary))]

           #:attr binds (append (attribute cond.binds)
                                (attribute then.binds)
                                (attribute else.binds)
                                (list
                                 (declaration atom)
                                 (statement
                                  #`(#%if cond
                                          (#%set! #,atom then)
                                          (#%set! #,atom else)))))

           #:attr atom atom

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%abs (arg ...) body:expr ...)

           #:attr binds null
           
           #:attr atom
           (syntax/loc this-syntax
             (#%abs (arg ...)
                    body.anf ...))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%app expr:expr ...)

           #:attr binds
           (apply append (attribute expr.binds))

           #:attr atom #'(#%app expr.atom ...)

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom))))

(define (binds+atom->r-lua-anf binds atom)

  (if (pair? binds)

      (let ([is-ident (identifier? atom)])

        (let ([binds (if is-ident
                         (drop-right binds 1)
                         binds)]
              [atom (if is-ident
                        (bind-expr (last binds))
                        atom)])

          (for/foldr ([stx atom]
                      [open #t]
                      #:result stx)
                     ([bind (in-list binds)])
            (cond
              [(let-bind? bind)
               (with-syntax ([ident (let-bind-ident bind)]
                             [expr  (let-bind-expr bind)])
                 (syntax-parse stx
                   #:datum-literals [#%let]
                   [(#%let ([stx-ident stx-expr] ...)
                           body)
                    #:when open
                    (values #'(#%let ([ident expr]
                                      [stx-ident stx-expr]
                                      ...)
                                     body)
                            open)]
                   [stx
                    (values #'(#%let ([ident expr])
                                     stx)
                            #t)]))]

              [(letrec-bind? bind)
               (with-syntax ([ident (letrec-bind-ident bind)]
                             [expr  (letrec-bind-expr bind)])
                 (syntax-parse stx
                   #:datum-literals [#%letrec]
                   [(#%letrec ([stx-ident stx-expr] ...)
                              body)
                    #:when open
                    (values #'(#%letrec ([ident expr]
                                         [stx-ident stx-expr]
                                         ...)
                                        body)
                            open)]
                   [stx
                    (values #'(#%letrec ([ident expr])
                                        stx)
                            #t)]))]

              [(declaration? bind)
               (with-syntax ([ident (declaration-ident bind)])
                 (syntax-parse stx
                   #:datum-literals [#%declare]
                   [(#%declare (stx-ident ...)
                               body)
                    #:when open
                    (values #'(#%declare (ident stx-ident ...)
                                         body)
                            open)]
                   [stx
                    (values #'(#%declare (ident)
                                         stx)
                            #t)]))]

              [(statement? bind)
               (with-syntax ([stmt (statement-expr bind)]
                             [stx stx])
                 (values #'(#%begin
                            stmt
                            stx)
                         #f))]

              [(close-bind? bind)
               (values stx #f)]))))

      atom))
