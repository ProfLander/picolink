#lang racket/base

(require racket/function
         racket/match

         syntax/strip-context

         picolink/ee-lib
         picolink/output
         picolink/program/scope
         picolink/module/syntax
         picolink/module/parsed)

(provide (all-defined-out))

(define (parse-program mod-idents)

  (with-scope program

    (parameterize ([current-program-scope program])

      (for ([mod-ident (in-list mod-idents)])

        (let* ([ident  (syntax-local-introduce
                        (strip-context mod-ident))]

               [mod    (syntax-local-module mod-ident)]
               [parsed (parse-module ident mod)])

          (bind-in-program! ident parsed)))

      program)))

(define (compile-program program backend)

  (let* ([mod-ids (scope-idents program)])

    (for/list ([mod-id (in-list mod-ids)])
      (define mod
        (lookup-in-program mod-id))

      (compile-parsed-module mod-id
                             backend
                             mod))))

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
