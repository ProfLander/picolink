#lang racket/base

(require racket/file
         racket/function
         racket/path

         picolink2/exception)

(provide (all-defined-out))

(define current-linker
  (make-parameter #f))

(define current-link-space
  (make-parameter #f))

(define (link [start-path #f])
  (for/list ([path (in-list (find-files
                             (λ (path)
                               (path-has-extension?
                                path ".rkt"))
                             start-path))]
             #:do
             [(define linked
                (with-handlers
                  ([exn:fail:dynamic-require:unknown-module?
                    (const #f)])
                  
                  (dynamic-require `(submod ,path link) '#%linked)))]

             #:when linked)

    linked))

(define (link-module racket-module-path
                     module-path
                     imports
                     sources
                     definitions
                     module-body)

  (printf "linking ~a...\n" module-path)

  (let ([linker (current-linker)]
        [space  (current-link-space)])

    (unless linker
      (raise-argument-error 'current-linker     "linker" linker))

    (unless space
      (raise-argument-error 'current-link-space "space"  space))

    (let*-values ([(export-key) (if space
                                         (cons 0 space)
                                         0)]

                       [(exports _)  (module->exports racket-module-path)]
                       [(exports)    (assoc export-key exports)]
                       [(exports)    (if exports
                                         (map car (cdr exports ))
                                         null)])

      (linker module-path
              imports
              sources
              exports
              definitions
              module-body))))
