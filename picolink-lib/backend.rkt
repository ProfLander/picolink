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

(define-generics backend
  (output-name backend)
  (compile backend path)
  (link backend outputs)
  (run backend linked)

  #:fallbacks
  [(define/generic call-output-name output-name)
   (define/generic call-link link)

   (define (compile backend path)
     (let ([root (path-only path)]
           [path (file-name-from-path path)])

       (define (collect-input inputs requires path)
         (let ([abs-path (build-path root path)])
           (if (hash-has-key? inputs abs-path)

               (values inputs requires)

               (let* ([input (dynamic-require
                              (module-path->file-path abs-path)
                              'input)]
                      [mod-reqs (input:collect-requires input)])

                 (for*/fold ([inputs (hash-set inputs path input)]
                             [requires (hash-union requires mod-reqs)])
                            ([(_ reqs) (in-hash mod-reqs)]
                             [(mod _) (in-free-id-table reqs)])

                   (collect-input
                    inputs requires
                    (string->path
                     (symbol->string
                      (syntax-e mod)))))))))

       (let-values ([(inputs requires)
                     (collect-input (hash) (hash) path)])

         ; check requires
         (let ([provides (for/hash ([(path input) (in-hash inputs)])
                           (values path (input:collect-provides input)))])

           (for* ([(lctx reqs) (in-hash requires)]
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

           (let ([outputs (for/hash ([(path input) (in-hash inputs)])
                            (values path
                                    (input:compile
                                     input
                                     (call-output-name backend))))])

             (call-link backend outputs))))))])

(define backend-registry
  (make-hash))

(define/contract (register-backend name in)
  (-> symbol?
      (-> (generic-instance/c
           gen:backend

           [output-name (-> backend? symbol?)]

           [compile
            (or/c (-> backend? path? any/c) #f)]

           [link
            (-> backend? (hash/c path? output:output?) any/c)]

           [run
            (-> backend? any/c any/c)]))
      void)
  (hash-set! backend-registry name in))

(define/contract (backend name)
  (-> symbol? (-> backend?))
  (hash-ref backend-registry name))

(define/contract (make-backend name)
  (-> symbol? backend?)
  ((backend name)))
