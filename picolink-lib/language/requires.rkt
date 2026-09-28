#lang racket/base

(require racket/contract
         racket/list
         racket/hash
         racket/set
         racket/struct
         racket/sequence

         picolink/binding)

(provide (all-defined-out))

(struct module-require (lctx from to)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'module-require)
      (λ (self) (let ([from (module-require-from self)]
                      [to (module-require-to self)])
                  (if (equal? from to)
                      (list from)
                      (list from '-> to))))))]

  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (and (module-require? b)
          (recur (module-require-from a)
                 (module-require-from b))))

   (define (hash-proc a recur)
     (recur (module-require-from a)))

   (define (hash2-proc a recur)
     (recur (module-require-from a)))])

(define/contract (make-module-require lctx from [to from])
  (-> syntax?
      (or/c identifier? binding?)
      (or/c identifier? binding?)
      module-require?)
  (module-require lctx
                  (lift-binding from)
                  (lift-binding to)))

(struct module-requires (inner)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'module-requires)
      (λ (self) (hash-map (module-requires-inner self)
                          (λ (k v)
                            (cons k (set->list v)))))))])

(define/contract (make-module-requires [requires null])
  (->* []
       [(listof (cons/c (or/c identifier? binding?)
                        (listof module-require?)))]
       module-requires?)

  (let* ([requires
          (for/fold ([acc (hash)])
                    ([pair (in-list requires)])
            (let ([mod (car pair)]
                  [reqs (cdr pair)])
              (hash-union acc
                          (hash (lift-binding mod) reqs)
                          #:combine append)))]

         [requires
          (for/hash ([(mod reqs) (in-hash requires)])
            (let ([duplicate (check-duplicates reqs)])
              (when duplicate
                (raise-syntax-error
                 #f
                 "duplicate require:"
                 (module-require-lctx duplicate)
                 (binding-ident
                  (module-require-from duplicate)))))
            (values mod (list->set reqs)))])

    (module-requires requires)))

(define/contract (module-requires-ref
                  requires key
                  [failure-result
                   (λ ()
                     (raise (make-exn:fail:contract)))])
  (->* [module-requires? binding?]
       [failure-result/c]
       (set/c module-require?))
  (hash-ref (module-requires-inner requires) key failure-result))

(define/contract (in-module-requires requires)
  (-> module-requires? (sequence/c binding? (set/c module-require?)))
  (in-hash (module-requires-inner requires)))
