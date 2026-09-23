#lang racket/base

(require racket/generic
         racket/contract
         racket/hash
         racket/path

         syntax/id-set
         syntax/id-table

         picolink/path

         (prefix-in input: picolink/input)
         (prefix-in output: picolink/output))

(provide (all-defined-out))

(struct compile-ctx (root entry-point))
(struct link-ctx (root entry-point outputs requires provides))

(define-generics backend
  (output-name backend)

  (compile backend ctx)
  (link backend ctx)
  (run backend linked)

  #:fallbacks
  [(define/generic call-output-name output-name)
   (define/generic call-link link)
   (define/generic call-run run)

   (define (compile backend ctx)
     (define (collect-input inputs requires path)

       (let ([abs-path (build-path (compile-ctx-root ctx) path)])
         (if (hash-has-key? inputs path)

             (values inputs requires)

             (let* ([input (dynamic-require
                            (module-path->file-path abs-path)
                            'input)]
                    [mod-reqs (hash path
                                    (input:collect-requires input))])

               (for*/fold ([inputs (hash-set inputs path input)]
                           [requires (hash-union requires mod-reqs)])
                          ([(_ reqs) (in-hash mod-reqs)]
                           [(_ reqs) (in-hash reqs)]
                           [(mod _) (in-free-id-table reqs)])

                 (collect-input
                  inputs requires
                  (string->path
                   (symbol->string
                    (syntax-e mod)))))))))

     (let-values ([(inputs requires)
                   (collect-input (hash)
                                  (hash)
                                  (compile-ctx-entry-point ctx))])

       (let ([provides (for/hash ([(path input) (in-hash inputs)])
                         (values path (input:collect-provides input)))])

         (for* ([(_ reqs) (in-hash requires)]
                [(lctx reqs) (in-hash reqs)]
                [(target reqs) (in-free-id-table reqs)])

           (let* ([target (string->path
                           (symbol->string
                            (syntax-e target)))]
                  [provs (hash-ref provides target)])

             (for ([req (in-free-id-set reqs)])

               (unless (free-id-set-member? provs req)
                 (raise-syntax-error
                  'require
                  (format "~a does not provide ~a"
                          target (syntax-e req))
                  lctx
                  req)))))

         (let ([outputs
                (for/hash ([(path input) (in-hash inputs)])
                  (values path
                          (input:compile
                           input
                           (call-output-name backend))))]

               [requires
                (for*/fold ([acc
                             (for/hash ([(path _) (in-hash requires)])
                               (values
                                path
                                (make-immutable-free-id-table)))])
                           ([(path reqs) (in-hash requires)]
                            [(_ req-tbl) (in-hash reqs)]
                            [(mod req-set) (in-free-id-table req-tbl)])
                  (hash-update
                   acc path
                   (λ (tgt)
                     (free-id-table-update
                      tgt mod
                      (λ (tgt)
                        (free-id-set-union tgt req-set))
                      req-set))
                   req-tbl))])

           (call-run
            backend
            (call-link
             backend
             (link-ctx
              (compile-ctx-root ctx)
              (compile-ctx-entry-point ctx)
              outputs
              requires
              provides)))))))])

(define backend-registry
  (make-hash))

(define/contract (register-backend name in)
  (-> symbol?
      (-> (generic-instance/c
           gen:backend
           [output-name (-> backend? symbol?)]
           [compile (or/c (-> backend? compile-ctx? any/c) #f)]
           [link (-> backend? link-ctx? any/c)]
           [run (-> backend? any/c any/c)]))
      void)
  (hash-set! backend-registry name in))

(define/contract (backend name)
  (-> symbol?
      (generic-instance/c
       gen:backend
       [output-name (-> backend? symbol?)]
       [compile (or/c (-> backend? compile-ctx? any/c) #f)]
       [link (-> backend? link-ctx? any/c)]
       [run (-> backend? any/c any/c)]))

  (dynamic-require
   (string->symbol (format "picolink/backend/~a" name))
   'backend-inst))
