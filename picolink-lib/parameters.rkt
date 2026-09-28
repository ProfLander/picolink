#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define current-backend
  (make-parameter 'love))

(define/contract (set-current-backend backend)
  (-> string? void)
  (current-backend (string->symbol backend)))

(define current-build-directory
  (make-parameter (build-path "build")))

(define/contract (set-current-build-directory build-directory)
  (-> path? void)
  (current-build-directory (string->path build-directory)))

(define current-entry-point
  (make-parameter
   (string->path #;"../picolink-test/love-api/graphics-print"
                 "../picolink-test/dependency-tree/a"
                 #;"../picolink-test/lambda/lambda"
                 #;"../picolink-test/intrinsics/intrinsic-string")))

(define/contract (set-current-entry-point entry-point)
  (-> path? void)
  (current-entry-point entry-point))
