#lang racket/base

(require picolink/config
         picolink/backend/compile-ctx
         (prefix-in backend: picolink/backend))

(provide (all-defined-out)
         (all-from-out picolink/config))

; Procedure entrypoints

(define (compile config)
  (let* ([backend      (config-backend config)]
         [search-paths (append (backend:search-paths backend)
                               (list (config-root config)))]
         [compile-ctx  (make-compile-ctx config
                                         search-paths)])
    (backend:compile backend compile-ctx)))

(define (link config)
  (backend:link (config-backend config)
                (compile config)))

(define (run config)
  (backend:run (config-backend config)
               (link config)))

; REPL entrypoints

(module+ compile
  (compile))

(module+ link
  (link))

(module+ run
  (run))

; CLI entrypoint
(module+ main
  (require racket/cmdline)

  (define-values (command config)
    (command-line
     #:program "picolink"

     #:once-each
     [("-e" "--entry-point")
      entry-point
      "Set the entry-point module"
      (cons 'entry-point (string->path entry-point))]

     [("-d" "--build-directory")
      build-directory
      "Set the build output directory"
      (cons 'build-directory (string->path build-directory))]

     #:handlers
     (λ (flags command project-dir)

       (let* ([command (case command
                         [("compile") compile]
                         [("link") link]
                         [("run") run])]

              [config (find-config (string->path project-dir))]

              [flags (make-immutable-hash flags)]

              [entry-point (hash-ref flags 'entry-point #f)]
              [config (if entry-point
                          (config-update
                           config
                           #:entry-point entry-point)
                          config)]

              [build-directory (hash-ref flags 'build-directory #f)]
              [config (if build-directory
                          (config-update
                           config
                           #:build-directory build-directory)
                          config)])

         (values command config)))

     '("command" "project directory")))

  (command config))
