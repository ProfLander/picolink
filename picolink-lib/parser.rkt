#lang racket/base

(require racket/dict
         racket/syntax

         syntax/parse
         syntax/strip-context

         data/ddict

         picolink/ee-lib
         picolink/syntax
         picolink/program/scope
         picolink/module/scopes
         picolink/module/parsed
         picolink/module/import
         picolink/module/export)

(provide (all-defined-out))

(define current-debug
  (make-parameter #f))

(define (resolve-module lctx mod-path)

  (when (current-debug)
    (displayln "resolve-module:")
    (println lctx)
    (println mod-path))

  (define mod
    (lookup-in-program mod-path))

  (unless mod
    (raise-syntax-error
     #f
     "failed to resolve module"
     lctx
     mod-path))

  mod)

(define (resolve-provide lctx mod src-sym local-id)

  (when (current-debug)
    (displayln "resolve-provide:")
    (println lctx)
    (println mod)
    (println src-sym)
    (println local-id))

  (define prov
    (lookup-in (parsed-module-provide-scope mod)
               (datum->syntax #f src-sym)))

  (unless prov
    (raise-syntax-error
     #f
     (format "~a does not provide ~a"
             (syntax-e (parsed-module-ident mod))
             (syntax-e local-id))
     lctx
     local-id))

  prov)

(define (resolve-definition lctx local-id)

  (when (current-debug)
    (displayln "resolve-definition:")
    (println lctx)
    (println local-id))

  (define binding
    (lookup-in-module local-id))

  (unless binding
    (raise-syntax-error
     #f
     "unbound identifier"
     lctx
     local-id))

  binding)

(define (emplace-require! lctx mod local-id src-sym)

  (define mod-id (parsed-module-ident mod))

  (define local-id* (syntax-local-introduce local-id))

  (let ([import (import local-id*
                        src-sym
                        mod-id
                        lctx)])
    (let ([imports (lookup-in-require mod-id)])

      (unless imports
        (set! imports (make-mutable-ddict))
        (bind-in-require! (syntax-local-introduce (strip-context mod-id))
                          imports))

      (dict-set! imports (syntax-e local-id) import))))

(define (require-module-provide! lctx mod prov src-sym local-id)

  (when (current-debug)
    (displayln "require-module-provide!:")
    (println lctx)
    (println mod)
    (println prov)
    (println src-sym)
    (println local-id))

  (define binding
    (lookup-in (parsed-module-module-scope mod)
               (export-local-id prov)))

  (emplace-require! lctx mod local-id src-sym)

  (bind-in-module! local-id binding))

(define (require! lctx src-mod-path)

  (when (current-debug)
    (displayln "require!")
    (println src-mod-path))

  (require/prefix! lctx #f src-mod-path))

(define (require/prefix! lctx prefix src-mod-path)

  (when (current-debug)
    (displayln "require/prefix!")
    (println prefix)
    (println src-mod-path))

  (require/prefix-all-except! lctx prefix src-mod-path null))

(define (require/all-except! lctx src-mod-path exceptions)

  (when (current-debug)
    (displayln "require/all-except!")
    (println src-mod-path)
    (println exceptions))

  (require/prefix-all-except! lctx #f src-mod-path exceptions))

(define (require/prefix-all-except! lctx prefix src-mod-path exceptions)

  (when (current-debug)
    (displayln "require/prefix-all-except!")
    (println prefix)
    (println src-mod-path)
    (println exceptions))

  (define mod (resolve-module lctx src-mod-path))

  (define mod-id (parsed-module-ident mod))
  (define mod-id* (syntax-local-introduce (strip-context mod-id)))
  (let ([imports (lookup-in-require mod-id*)])
    (unless imports
      (bind-in-require! mod-id* (make-mutable-ddict))))

  (define provide-scope (parsed-module-provide-scope mod))

  (for ([prov-id (in-list (scope-idents provide-scope))]
        #:when (not (member prov-id exceptions datum=?)))

    (define local-id
      (datum->syntax src-mod-path
                     (syntax-e prov-id)
                     src-mod-path
                     src-mod-path))

    (define prefixed
      (if prefix
          (format-id local-id "~a~a" prefix local-id
                     #:source local-id
                     #:props local-id)
          local-id))

    (define src-sym (syntax-e local-id))

    (define prov (resolve-provide lctx mod src-sym prefixed))

    (require-module-provide! lctx
                             mod
                             prov
                             src-sym
                             prefixed)))

(define (require/only! lctx src-mod-path local-ids)

  (when (current-debug)
    (displayln "require/only!")
    (println src-mod-path)
    (println local-ids))

  (for ([local-id (in-list local-ids)])
    (require/rename! lctx src-mod-path local-id local-id)))

(define (require/rename! lctx
                         src-mod-path
                         src-id
                         local-id)

  (when (current-debug)
    (displayln "require/rename!")
    (println src-mod-path)
    (println src-id)
    (println local-id))

  (define src-sym (syntax-e src-id))

  (define mod
    (resolve-module lctx src-mod-path))

  (define prov
    (resolve-provide lctx mod src-sym local-id))

  (require-module-provide! lctx mod prov src-sym local-id))

(define (provide! lctx local-id)

  (when (current-debug)
    (displayln "provide!")
    (println local-id))

  (provide/rename! lctx local-id local-id))

(define (provide/all-from! lctx mod-id)

  (when (current-debug)
    (displayln "provide/all-from!")
    (println mod-id))

  (provide/all-from-except! lctx mod-id null))

(define (provide/all-from-except! lctx mod-id exceptions)

  (when (current-debug)
    (displayln "provide/all-from-except!")
    (println mod-id)
    (println exceptions))

  (define require-scope (current-require-scope))

  (define requires
    (for*/list ([(import-mod-id imports) (in-scope require-scope)]
                [(_ import)              (in-dict imports)]
                #:do [(define import-id (import-local-id import))]
                #:when (and (datum=? mod-id import-mod-id)
                            (not (member import-id exceptions
                                         datum=?))))
      import-id))

  (for ([req (in-list requires)])
    (provide! lctx req)))

(define (provide/all-defined! lctx)

  (when (current-debug)
    (displayln "provide/all-defined!"))

  (provide/all-defined-except! lctx null))

(define (provide/all-defined-except! lctx exceptions)

  (when (current-debug)
    (displayln "provide/all-defined-except!")
    (println exceptions))

  (provide/prefix-all-defined-except! lctx #f exceptions))

(define (provide/prefix-all-defined! lctx prefix)

  (when (current-debug)
    (displayln "provide/prefix-all-defined!")
    (println prefix))

  (provide/prefix-all-defined-except! lctx prefix null))

(define (provide/prefix-all-defined-except! lctx prefix exceptions)

  (when (current-debug)
    (displayln "provide/prefix-all-defined-except!")
    (println prefix)
    (println exceptions))

  (define module-scope (current-module-scope))
  (define require-scope (current-require-scope))

  (define requires
    (for*/list ([(_ imports) (in-scope require-scope)]
                [(_ import)  (in-dict imports)])
      (import-local-id import))) 

  (define definitions (filter (λ (def)
                                (and (not (member def requires
                                                  datum=?))
                                     (not (member def exceptions
                                                  datum=?))))
                              (scope-idents module-scope)))

  (for ([definition (in-list definitions)])

    (define prefixed
      (if prefix
          (format-id definition "~a~a" prefix definition
                     #:source definition
                     #:props definition)
          definition))

    (provide/rename! lctx definition prefixed)))

(define (provide/rename! lctx
                         local-id
                         out-id)

  (when (current-debug)
    (displayln "provide/rename!")
    (println local-id)
    (println out-id))

  (resolve-definition lctx local-id)

  (define out-id*
    (syntax-local-introduce (strip-context out-id)))

  (define provide-id
    (export local-id
            out-id*
            lctx))

  (bind-in-provide! out-id* provide-id)

  local-id)

(define require?
  (syntax-parser
    #:datum-literals [require]
    [(require _ ...) #t]
    [_ #f]))

(define provide?
  (syntax-parser
    #:datum-literals [provide]
    [(provide _ ...) #t]
    [_ #f]))

(define-splicing-syntax-class (extract-require lctx)
  #:datum-literals [require]
  (pattern
   (~and
    (~do (define deps null))
    (~seq
     (~or (~and mod:id
                (~do (set! deps (append deps (list #'mod)))))

          (~and (only only-mod:id _ ...)
                (~do (set! deps (append deps (list #'only-mod)))))

          (~and (prefix _:id prefix-mod:id)
                (~do (set! deps (append deps (list #'prefix-mod)))))

          (~and (all-except all-except-mod:id _...)
                (~do (set! deps
                           (append deps (list #'all-except-mod)))))

          (~and (prefix-all-except
                 _
                 prefix-all-except-mod:id
                 _ ...)
                (~do (set! deps
                           (append deps (list #'prefix-all-except-mod)))))

          (~and (rename rename-mod:id _ _)
                (~do (set! deps (append deps (list #'rename-mod))))))
     ...))

   #:attr dependencies deps))


(define-splicing-syntax-class (bind-require lctx)
  #:datum-literals [require
                    only
                    prefix
                    all-except
                    prefix-all-except
                    rename]
  (pattern
   (~seq (~or (~and mod:id
                    (~do (require! lctx #'mod)))

              (~and (only ~! only-mod:id req:id ...)
                    (~do (require/only! lctx #'only-mod
                                        (attribute req))))

              (~and (prefix ~! req-prefix:id prefix-mod:id)
                    (~do (require/prefix! lctx #'req-prefix
                                          #'prefix-mod)))

              (~and (all-except
                     ~!
                     all-except-mod:id
                     all-except-exception:id ...)

                    (~do (require/all-except!
                          lctx
                          #'all-except-mod
                          (attribute all-except-exception))))

              (~and (prefix-all-except
                     ~!
                     all-except-prefix:id
                     prefix-all-except-mod:id
                     prefix-all-except-exception:id ...)

                    (~do (require/all-except!
                          lctx
                          #'all-except-prefix
                          #'prefix-all-except-mod
                          (attribute prefix-all-except-exception))))

              (~and (rename ~! rename-mod:id req-from:id req-to:id)
                    (~do (require/rename! lctx
                                          #'rename-mod
                                          #'req-from
                                          #'req-to))))
         ...)))

(define-splicing-syntax-class (bind-provide lctx)
  #:datum-literals [provide
                    rename
                    all-from
                    all-from-except
                    all-defined
                    all-defined-except
                    prefix-all-defined
                    prefix-all-defined-except]
  (pattern
   (~seq (~or (~and prov:id
                    (~do (provide! lctx
                                   #'prov)))

              (~and (rename ~! prov-from:id prov-to:id)
                    (~do (provide/rename! lctx
                                          #'prov-from
                                          #'prov-to)))

              (~and (all-from mod:id)
                    (~do (provide/all-from! lctx #'mod)))

              (~and (all-from-except
                     ~!
                     except-mod:id
                     from-exception:id ...)
                    (~do (provide/all-from-except!
                          lctx
                          #'except-mod
                          (attribute from-exception))))

              (~and (all-defined)
                    (~do (provide/all-defined! lctx)))

              (~and (all-defined-except ~! ident:id ...)
                    (~do (provide/all-defined-except!
                          lctx
                          (attribute ident))))

              (~and (prefix-all-defined ~! prefix-id:id)
                    (~do (provide/prefix-all-defined! lctx
                                                      #'prefix-id)))

              (~and (prefix-all-defined-except
                     ~!
                     except-prefix:id
                     except-ident:id ...)
                    (~do (provide/prefix-all-defined-except!
                          lctx
                          #'except-prefix
                          (attribute except-ident)))))
         ...)))
