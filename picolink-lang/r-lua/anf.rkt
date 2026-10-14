#lang racket/base

(require racket/list
         racket/syntax
         syntax/parse)

(provide (all-defined-out))

(struct let-bind [ident expr]
  #:transparent)

(struct set-bind [ident expr]
  #:transparent)

(define (bind-expr bind)
  (cond
    [(let-bind? bind) (let-bind-expr bind)]
    [(set-bind? bind) (set-bind-expr bind)]))

(define r-lua->r-lua-anf
  (syntax-parser
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

           #:attr binds
           (apply append
                  (for/list ([binds (attribute body.binds)]
                             [atom (attribute body.atom)])
                    (append binds
                            (list (let-bind (generate-temporary) atom)))))
           
           #:attr atom (let-bind-ident (last (attribute binds)))

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%let ([ident:id expr:expr] ...)
                  body:expr)

           #:attr binds
           (append (apply append (attribute expr.binds))

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

  (pattern (#%set! ident expr:expr)

           #:attr binds (append (attribute expr.binds)
                                (list (set-bind #'ident #'expr.atom)))

           #:attr atom
           (syntax/loc this-syntax
             #%void)

           #:attr anf (binds+atom->r-lua-anf (attribute binds)
                                             (attribute atom)))

  (pattern (#%if cond:expr then:expr else:expr)

           #:attr binds (append (attribute cond.binds)
                                (attribute then.binds)
                                (attribute else.binds))

           #:attr atom (syntax/loc this-syntax
                         (#%if cond.atom
                               then.atom
                               else.atom))

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
          (for/foldr ([stx atom])
                     ([bind (in-list binds)])
            (cond
              [(let-bind? bind)
               (with-syntax ([ident (let-bind-ident bind)]
                             [expr  (let-bind-expr bind)])
                 #`(#%let ([ident expr])
                          #,stx))]
              [(set-bind? bind)
               (with-syntax ([ident (set-bind-ident bind)]
                             [expr  (set-bind-expr bind)])
                 #`(#%begin
                    (#%set! ident expr)
                    #,stx))]))))
      atom))
