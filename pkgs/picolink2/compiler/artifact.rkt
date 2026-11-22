#lang racket/base

(require racket/file
         racket/path

         picolink2/compiler/directory)

(provide (all-defined-out))

(define-values (current-compile-artifacts)
  (make-parameter (make-hash)))

(struct artifact/string      [inner])
(struct artifact/bytes       [inner])
(struct artifact/copy-file   [inner])

(define (check-path name v)
  (unless (path-string? v)
    (raise-argument-error name "path?" v)))

(define (check-string name v)
  (unless (string? v)
    (raise-argument-error name "string?" v)))

(define (check-bytes name v)
  (unless (path? v)
    (raise-argument-error name "bytes?" v)))

(define (check-artifact/string name v)
  (unless (artifact/string? v)
    (raise-argument-error name "artifact/string?" v)))

(define (check-artifact/bytes name v)
  (unless (artifact/bytes? v)
    (raise-argument-error name "artifact/bytes?" v)))

(define (check-artifact/copy-file name v)
  (unless (artifact/copy-file? v)
    (raise-argument-error name "artifact/copy-file?" v)))

(define (compile-artifact path)
  (check-path 'set-compile-artifact path)
  (hash-ref (current-compile-artifacts) path))

(define (%set-compile-artifact! path artifact)
  (hash-set! (current-compile-artifacts) path artifact))

(define (set-compile-artifact/string! path string)
  (check-path   'set-compile-artifact/string path)
  (check-string 'set-compile-artifact/string string)
  (%set-compile-artifact! path (artifact/string string)))

(define (set-compile-artifact/bytes! path bytes)
  (check-path  'set-compile-artifact/bytes path)
  (check-bytes 'set-compile-artifact/bytes bytes)
  (%set-compile-artifact! path (artifact/bytes bytes)))

(define (set-compile-artifact/copy-file! path file-path)
  (check-path 'set-compile-artifact/copy-file path)
  (check-path 'set-compile-artifact/copy-file file-path)
  (%set-compile-artifact! path (artifact/copy-file file-path)))

(define (write-compile-artifact! artifact build-dir path)
  (check-path 'write-compile-artifact! build-dir)
  (check-path 'write-compile-artifact! path)

  (printf "writing ~a\n" path)

  (let ([path (build-path build-dir path)])
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
                  path
                  #:exists-ok? #t)])))

(define (write-compile-artifacts!)

  (let ([build-directory (current-build-directory)])

    (unless build-directory
      (raise-argument-error 'current-build-directory
                            "path?"
                            build-directory))

    (delete-directory/files build-directory #:must-exist? #f)

    (let ([compile-artifacts (current-compile-artifacts)])

      (when (not (hash-empty? compile-artifacts))
        (printf "\ninitializing build directory...\n")
        (make-directory* build-directory)

        (for ([(path artifact) (in-hash compile-artifacts)])
          (make-directory* (path-only (build-path build-directory path)))
          (write-compile-artifact! artifact build-directory path))))))
