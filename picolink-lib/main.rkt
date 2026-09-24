#lang racket/base

(require racket/path

         picolink/config
         picolink/backend/compile-ctx
         (prefix-in backend: picolink/backend))

; Procedure entrypoint
(define (compile)
  (let* ([entry-point (current-entry-point)]
         [backend (backend:backend (current-backend))]
         [search-paths (append (backend:search-paths backend)
                               (list (path-only entry-point)))]
         [entry-point (file-name-from-path entry-point)]
         [compile-ctx (make-compile-ctx search-paths
                                        entry-point)]
         [link-ctx (backend:compile backend compile-ctx)]
         [linked (backend:link backend link-ctx)])

    (backend:run backend linked)))

; REPL entrypoint
(module+ run
  (compile))

; CLI entrypoint
(module+ main
  (require racket/cmdline)

  (command-line
   #:program "picolink"
   #:once-each
   [("-o" "--output") output
                      "Set the build output directory"
                      (set-current-build-directory output)]
   [("-b" "--backend") backend
                       "Set the language backend"
                       (set-current-backend backend)])

  (require (submod ".." run)))
