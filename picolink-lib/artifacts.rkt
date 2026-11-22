#lang racket/base

(require racket/file
         racket/path

         picolink/config/parameters
         picolink/syntax)

(provide (all-defined-out))

(define-values (current-compile-artifacts)
  (make-parameter (make-hash)))

(struct/cross-phase artifact/string      [inner])
(struct/cross-phase artifact/bytes       [inner])
(struct/cross-phase artifact/copy-file   [inner])

(define (artifact? self)
  (or (artifact/string?    self)
      (artifact/bytes?     self)
      (artifact/copy-file? self)))

(define (check-path name path)
  (unless (path? path)
    (raise-argument-error name "path?" path)))

(define (check-artifact/string name artifact)
  (unless (artifact/string? artifact)
    (raise-argument-error name "artifact/string?" artifact)))

(define (check-artifact/bytes name artifact)
  (unless (artifact/bytes? artifact)
    (raise-argument-error name "artifact/bytes?" artifact)))

(define (check-artifact/copy-file name artifact)
  (unless (artifact/copy-file? artifact)
    (raise-argument-error name "artifact/copy-file?" artifact)))

(define (check-artifact name artifact)
  (unless (artifact? artifact)
    (raise-argument-error name "artifact?" artifact)))

(define (compile-artifact path)
  (check-path 'set-compile-artifact path)
  (hash-ref (current-compile-artifacts) path))

(define (%set-compile-artifact! path artifact)
  (hash-set! (current-compile-artifacts) path artifact))

(define (set-compile-artifact/string! path artifact)
  (check-path            'set-compile-artifact/string path)
  (check-artifact/string 'set-compile-artifact/string artifact)
  (%set-compile-artifact! path artifact))

(define (set-compile-artifact/bytes! path artifact)
  (check-path           'set-compile-artifact/bytes path)
  (check-artifact/bytes 'set-compile-artifact/bytes artifact)
  (%set-compile-artifact! path artifact))

(define (set-compile-artifact/copy-file! path artifact)
  (check-path               'set-compile-artifact/copy-file path)
  (check-artifact/copy-file 'set-compile-artifact/copy-file artifact)
  (%set-compile-artifact! path artifact))

(define (in-compile-artifacts)
  (in-hash (current-compile-artifacts)))

(define (write-compile-artifact! artifact path)
  (check-path 'write-compile-artifact! path)

  (printf "writing artifact: ~a\n" path)
  (println artifact)

  (cond
    [(artifact/string? artifact)
     (call-with-output-file path
       (λ (out)
         (write-string (artifact/string-inner artifact) out))
       #:exists 'replace)]

    [(artifact/bytes? artifact)
     (call-with-output-file path
       (λ (out)
         (write-bytes (artifact/bytes-inner artifact) out))
       #:exists 'replace)]

    [(artifact/copy-file? artifact)
     (copy-file (artifact/copy-file-inner artifact)
                path #:exists-ok? #t)]))

(define (write-compile-artifacts!)
  (let ([build-directory (build-path (path-only (current-config-path))
                                     (current-build-directory))])

    (delete-directory/files build-directory #:must-exist? #f)
    (make-directory* build-directory)

    (for ([(path artifact) (in-compile-artifacts)])
      (let ([path (build-path build-directory path)])
        (printf "making directory: ~a\n" (path-only path))
        (make-directory* (path-only path))
        (write-compile-artifact! artifact path)))))
