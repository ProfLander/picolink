#lang racket/base

(require (for-syntax racket/base
                     racket/string
                     racket/syntax
                     racket/sequence
                     syntax/parse)
         syntax/parse)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(begin-for-syntax
  (define-syntax-class pattern-var
    (pattern %pat:id
             #:do [(define str (symbol->string (syntax-e #'%pat)))
                   (define idx (string-find str ":"))]
             #:fail-unless idx
             "identifier has no class annotation"

             #:with ident (datum->syntax #'%pat
                                         (string->symbol
                                          (substring str 0 idx))
                                         #'%pat)

             #:attr class (datum->syntax #'%pat
                                         (string->symbol
                                          (substring str (+ 1 idx)))
                                         #'%pat))
    (pattern ident:id
             #:attr class #f)))

#;(define-syntax ~cata
  (pattern-expander
   (syntax-parser

     [(_ pvar:pattern-var parser:id ...)
      #`(~and #,(if (attribute pvar.class)
                    #'(~var temp pvar.class)
                    #'temp)
              (~parse pvar.ident
                      (begin (parser #'temp)
                             ...)))]

     [(_ pvar:pattern-var parser:expr ...)
      (with-syntax ([temp #'temp])

        (define replace
          (syntax-parser
            [(e ...)
             (datum->syntax this-syntax
                            (map replace (attribute e))
                            this-syntax
                            this-syntax)]
            [(~datum _) #'#'temp]
            [ident #'ident]))

        (with-syntax ([(parser ...) (map replace (attribute parser))])
          #`(~and #,(if (attribute pvar.class)
                        #'(~var temp pvar.class)
                        #'temp)
                  (~parse pvar.ident (begin parser ...)))))])))

(define-syntax ~cata
  (pattern-expander
   (syntax-parser

     [(_ pvar:pattern-var parser:id ...)
      #`(~and #,(if (attribute pvar.class)
                    #'(~var temp pvar.class)
                    #'temp)
              (~bind [pvar.ident
                      (begin (parser #'temp)
                             ...)]))]

     [(_ pvar:pattern-var parser:expr ...)
      (with-syntax ([temp #'temp])

        (define replace
          (syntax-parser
            [(e ...)
             (datum->syntax this-syntax
                            (map replace (attribute e))
                            this-syntax
                            this-syntax)]
            [(~datum _) #'#'temp]
            [ident #'ident]))

        (with-syntax ([(parser ...) (map replace (attribute parser))])
          #`(~and #,(if (attribute pvar.class)
                        #'(~var temp pvar.class)
                        #'temp)
                  (~bind [pvar.ident (begin parser ...)]))))])))

(define (forward-macro lctx stx)
    (datum->syntax lctx (syntax-e stx) lctx lctx))

(define-syntax define-parser-like-syntax-class
  (syntax-parser
    [(_ name:id out:id
        (~seq kw:keyword expr:expr) ...
        [pat body ... ret] ...)
     #'(define-syntax-class name
         (~@ kw expr) ...
         (pattern pat
                  #:do [body ...]
                  #:attr out ret)
         ...)]))

(define (lift-requires+provides require? provide? bodies)
  (for/foldr ([requires null]
              [provides null]
              [bodies null])

             ([body (in-list bodies)])

    (syntax-parse body
      [req
       #:when (require? #'req)
       (values (cons this-syntax requires)
               provides
               bodies)]

      [prov
       #:when (provide? #'prov)
       (values requires
               (cons this-syntax provides)
               bodies)]

      [body
       (values requires
               provides
               (cons #'body bodies))])))

(define-syntax struct/cross-phase
  (syntax-parser
    [(_ name [field ...])

     (define (format-internal ident)
       (format-id #f "%~a" ident))

     (let ([field-count (length (attribute field))])
       (with-syntax* ([name-mod
                       (format-id #f "~a-mod" #'name)]

                      [struct:name
                       (format-id #'name "struct:~a" #'name)]

                      [%struct:name
                       (format-internal #'struct:name)]

                      [name
                       (format-id #'name "~a" #'name)]

                      [%name
                       (format-internal #'name)]

                      [name?
                       (format-id #'name "~a?" #'name)]

                      [%name?
                       (format-internal #'name?)]

                      [name-ref
                       (format-id #'name "~a-ref" #'name)]

                      [name-set!
                       (format-id #'name "~a-set!" #'name)]

                      [(name-field ...)
                       (for/list ([field (in-list (attribute field))])
                         (format-id field "~a-~a" #'name field))]

                      [(%name-field ...)
                       (for/list ([field (in-syntax #'(name-field ...))])
                         (format-internal field))]

                      [(field-ind ...)
                       (for/list ([i (in-range 0 field-count)])
                         i)]

                      [field-count field-count])

         #'(begin
             (module name-mod racket/kernel
               (#%declare #:cross-phase-persistent)

               (#%provide struct:name
                          name
                          name?

                          name-field ...)

               (define-values [struct:name
                               name
                               name?
                               name-ref
                               name-set!]

                 (make-struct-type 'name
                                   #f
                                   field-count
                                   0
                                   #f
                                   null
                                   #f
                                   #f
                                   '(field-ind ...)
                                   #f
                                   'name))

               (define-values
                 (name-field)
                 (lambda (m)
                   (name-ref m field-ind)))
               ...)

             (require (only-in (submod "." name-mod)
                               [struct:name %struct:name]
                               [name %name]
                               [name? %name?]
                               [name-field %name-field] ...))

             (define struct:name %struct:name)
             (define name %name)
             (define name? %name?)
             (define name-field %name-field) ...)))]))
