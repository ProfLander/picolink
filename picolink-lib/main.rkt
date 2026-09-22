#lang racket/base

(module+ main
  (require racket/contract
           racket/cmdline

           picolink/backend
           picolink/backend/racket
           picolink/backend/lua
           picolink/output/s-lua/interface)

  (define current-backend
    (make-parameter 'lua))

  (define current-entry-point
    (make-parameter (string->path "../picolink-test/dependency-tree/a")))

  (define/contract (set-current-backend output)
    (-> string? void)
    (current-backend (string->symbol output)))

  (command-line
   #:program "picolink"
   #:once-each
   [("-b" "--backend") backend
                      "Set the language backend"
                      (set-current-backend backend)])

  (let ([backend (make-backend (current-backend))]
        [entry-point (current-entry-point)])
    (compile backend entry-point)))
