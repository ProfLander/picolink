#lang racket/base

(require racket/function
         racket/match
         racket/hash

         syntax/strip-context

         picolink/ee-lib
         picolink/syntax
         picolink/output
         picolink/program/scope
         picolink/module/syntax
         picolink/module/source
         picolink/module/parsed)

(provide (all-defined-out))

(define (topological-sort-program mod-idents)

  (define graph
    (for*/fold ([graph (for/hash ([ident (in-list mod-idents)])
                         (values (syntax-e ident) null))])
               ([ident (in-list mod-idents)]
                #:do [(define mod (syntax-local-module ident))
                      (define deps (source-module-dependencies mod))]
                [dep (in-list deps)])

      (let ([dep   (syntax-e dep)]
            [ident (syntax-e ident)])

        (hash-update graph dep (curry cons ident)))))

  (define in-degrees
    (for/hash ([ident (in-list mod-idents)])

      (let* ([mod  (syntax-local-module ident)]
             [deps (source-module-dependencies mod)])

        (values (syntax-e ident) (length deps)))))

  (define queue
    (for/list ([(mod-id in-degree) (in-hash in-degrees)]
               #:when (= 0 in-degree))
      mod-id))

  (define order
    (let loop ([order null]
               [queue queue]
               [in-degrees in-degrees])

      (if (= 0 (length queue))

          (for/list ([sym (in-list (reverse order))])
            (findf (λ (b)
                     (equal? sym (syntax-e b)))
                   mod-idents))

          (let ([u (car queue)])

            (let-values ([(queue in-degrees)
                          (for/fold ([queue (cdr queue)]
                                     [in-degrees in-degrees])
                                    ([dep (in-list (hash-ref graph u))])

                            (define in-degree
                              (- (hash-ref in-degrees dep) 1))

                            (values (if (= 0 in-degree)
                                        (append queue (list dep))
                                        queue)

                                    (hash-set in-degrees
                                              dep
                                              in-degree)))])

              (loop (cons u order)
                    queue
                    in-degrees))))))

  (unless (= (length order)
             (length mod-idents))

    (error "circular dependency"))

  order)

(define (parse-program mod-idents)

  (let ([mod-idents (topological-sort-program mod-idents)])

    (with-scope program

      (parameterize ([current-program-scope program])

        (for ([mod-ident (in-list mod-idents)])

          (let* ([ident  (syntax-local-introduce
                          (strip-context mod-ident))]

                 [mod    (syntax-local-module mod-ident)]
                 [parsed (parse-module ident mod)])

            (bind-in-program! ident parsed)))

        program))))

(define (compile-program program backend)

  (let* ([mod-ids (scope-idents program)])

    (for/list ([mod-id (in-list mod-ids)])
      (define mod
        (lookup-in-program mod-id))

      (compile-parsed-module mod-id backend mod))))

(define (parallel-compile-program program backend)

  (let* ([mod-ids (scope-idents program)]

         [pool
          (make-parallel-thread-pool)]

         [threads
          (for/list ([mod-id (in-list mod-ids)])
            (define mod
              (lookup-in-program mod-id))

            (define-values (out-port get-out-events)
              (make-deferred-output-port))

            (define-values (err-port get-err-events)
              (make-deferred-output-port))

            (parameterize ([current-output-port out-port]
                           [current-error-port err-port])
              (list mod-id
                    get-out-events
                    get-err-events
                    (thread
                     (thunk
                      (compile-parsed-module mod-id
                                              backend
                                              mod))
                     #:pool pool
                     #:keep 'results))))])

    (for/list ([parts (in-list threads)])
      (match parts
        [(list mod-id get-out-events get-err-events thrd)

         (define result
           (thread-wait thrd))

         (define out-events
           (get-out-events))

         (define err-events
           (get-err-events))

         (when (or (pair? out-events)
                   (pair? err-events))

           (printf "~a:\n" (syntax-e mod-id))

           (replay-deferred-output out-events (current-output-port))
           (replay-deferred-output err-events (current-error-port))

           (displayln ""))

         result]))))
