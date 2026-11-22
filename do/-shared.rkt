#lang racket/base

(require (only-in raco/invoke [raco %raco])
         (only-in pkg/lib pkg-directory))

(provide (all-defined-out))

(define main-package "picolink2")
(define doc-package  "picolink-doc")

(define (raco . args)
  (exit (apply %raco args)))

(define (packages)
  (for/list ([path (in-list (directory-list "../pkgs"))])
    (path->string path)))

(define (make-path target)
  (build-path ".."
              "pkgs"
              target))

(define (package-paths)
  (for/hash ([package (in-list (packages))])
    (values package (path->string (make-path package)))))

(define (build-package-path package . path)
  (apply build-path (hash-ref (package-paths) package path)
         path))

(define (build-package-path-string package . path)
  (path->string (apply build-package-path package path)))

(define installed? pkg-directory)

(define (installed-packages)
  (filter installed? (packages)))
