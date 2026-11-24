#lang racket/base

(require racket/file
         racket/path

         picolink2/compiler/directory)

(provide (all-defined-out))

(struct artifact/string      [inner] #:prefab)
(struct artifact/bytes       [inner] #:prefab)
(struct artifact/copy-file   [inner] #:prefab)

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
                  #:exists-ok? #t)]

      [else (error 'write-compile-artifact!
                   "unrecognized artifact ~a"
                   artifact)])))

(define (write-compile-artifacts! artifacts)

  (let ([build-directory (current-build-directory)])

    (unless build-directory
      (raise-argument-error 'current-build-directory
                            "path?"
                            build-directory))

    (delete-directory/files build-directory #:must-exist? #f)

    (when (not (hash-empty? artifacts))
      (printf "\ninitializing build directory...\n\n")
      (make-directory* build-directory)

      (for ([(path artifact) (in-hash artifacts)])
        (make-directory* (path-only (build-path build-directory path)))
        (write-compile-artifact! artifact build-directory path)))))
