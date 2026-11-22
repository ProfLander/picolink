#lang racket/base

(require picolink2/compiler/directory)

(provide (all-defined-out))

(define current-runner
  (make-parameter #f))

(define (run linked module-path)

  (parameterize ([current-directory
                  (build-path (current-directory)
                              (current-build-directory))])

    (let ([runner (current-runner)])
      (runner linked module-path))))
