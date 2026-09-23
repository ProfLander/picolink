#lang racket/base

(require racket/path

         picolink/config
         (prefix-in backend: picolink/backend))

; Procedure entrypoint
(define (compile)
  (let* ([entry-point (current-entry-point)]
         [backend (backend:backend (current-backend))]
         [root (path-only entry-point)]
         [entry-point (file-name-from-path entry-point)]
         [ctx (backend:compile-ctx root entry-point)])

    (backend:compile backend ctx)))

; REPL entrypoint
(module+ run
  (compile))

; CLI entrypoint
(module+ main
  (require racket/cmdline)

  (command-line
   #:program "picolink"
   #:once-each
   [("-b" "--backend") backend
                       "Set the language backend"
                       (set-current-backend backend)])

  (require (submod ".." run)))
