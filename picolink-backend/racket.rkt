#lang picolink/backend

(require racket/list
         racket/dict
         racket/function

         syntax/parse

         picolink/module
         picolink/ee-lib

         (for-template racket/base))

(provide (all-defined-out))

#:target   (const 'racket)
#:link     racket-link
#:run      racket-run
#:interact racket-interact

(define racket-introduce (make-interned-syntax-introducer 'racket))

(define (racket-link _lctx compiled)

  (define mods
    (for/list ([mod (in-list compiled)])

      (define mod-id (compiled-module-ident mod))
      (define require-scope (compiled-module-require-scope mod))
      (define module-scope (compiled-module-module-scope mod))
      (define provide-scope (compiled-module-provide-scope mod))
      (define stx (compiled-module-stx mod))

      (let* ([reqs
              (reverse
               (for*/list ([(_ imports) (in-scope require-scope)]
                           [(_ req)     (in-dict imports)])

                 (with-syntax ([local-id
                                (racket-introduce
                                 (splice-from-scope
                                  (import-local-id req)
                                  module-scope))]

                               [dots (if (equal? (syntax-e mod-id)
                                                 'entry-point)
                                         "."
                                         "..")]

                               [src-sym
                                (import-src-sym req)]
                               [src-mod-path
                                (import-src-mod-path req)])

                   #'(only-in (submod dots src-mod-path)
                              [src-sym local-id]))))]

             [provs
              (for/list ([(_ prov) (in-scope provide-scope)])

                (with-syntax ([local-id
                               (racket-introduce (splice-from-scope
                                       (export-local-id prov)
                                       module-scope))]
                              [out-id (export-out-id prov)])

                  #'[local-id out-id]))])

        (with-syntax ([(req ...)  reqs]
                      [(prov ...) provs]
                      [(body ...)
                       (syntax-parse stx
                         #:datum-literals [begin]
                         [(begin body ...)
                          #'(body ...)])])

          (cons mod-id
                #'(begin
                    (require req ...)
                    (provide (rename-out prov ...))
                    body ...))))))

  (define mods* (map (λ (pair)
                       (syntax-parse (cdr pair)
                         #:datum-literals [begin]
                         [(begin body ...)
                          (with-syntax ([ident (car pair)])
                            #'(module ident racket/base
                                body ...))]))
                     (remf (λ (a)
                             (equal? (syntax-e (car a))
                                     'entry-point))
                           mods)))

  (define entry-point
    (syntax-parse (cdr (assoc #'entry-point mods
                              (λ (a b)
                                (equal? (syntax-e a)
                                        (syntax-e b)))))
      #:datum-literals [begin]
      [(begin body ...)
       #'(body ...)]))

  (with-syntax ([(mod ...) (reverse mods*)]
                [(body ...) entry-point])

    (syntax-local-introduce
     #'(begin
         mod ...
         body ...))))

(define (racket-run _lctx linked)
  linked)

(define (racket-interact _lctx compiled)
  compiled)
