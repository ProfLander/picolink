#lang racket/base

(require racket/file
         racket/function
         racket/path

         picolink2/compiler/exception)

(provide (all-defined-out))

(define current-linker
  (make-parameter #f))

(define current-link-space
  (make-parameter #f))

(define (link [start-path #f])
  (for*/fold ([acc (hash)])
             ([path (in-list (find-files
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

              #:when linked

              [artifact (in-list linked)])

    (hash-set acc (car artifact) (cdr artifact))))

(define (link-module module-path
                     imports
                     sources
                     exports
                     definitions
                     module-body)

  (printf "linking ~a...\n" module-path)

  (let ([linker (current-linker)]
        [space  (current-link-space)])

    (unless linker
      (raise-argument-error 'current-linker     "linker" linker))

    (unless space
      (raise-argument-error 'current-link-space "space"  space))

    (linker module-path
            imports
            sources
            exports
            definitions
            module-body)))
