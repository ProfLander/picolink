#lang racket/base

; lua backend

(require racket/function
         racket/port
         racket/set
         racket/string

         picolink2/s-lua/name
         picolink2/compiler/artifact)

(provide (all-defined-out))

(define current-lua-compile-bytecode
  (make-parameter #f))

(define (lua-normalize-module-path mod-path)

  (let* ([mod-path
          (cond
            [(symbol? mod-path) (symbol->string mod-path)]

            [(string? mod-path) (path->string
                                 (path-replace-extension
                                  (string->path mod-path)
                                  ""))]

            [else (raise-result-error 'normalize-module-path
                                      "symbol? or string?"
                                      mod-path)])]

         [mod-path (string-replace mod-path "/" ".")])

    mod-path))

(define (lua-link/file module-path
                       imports
                       sources
                       exports
                       definitions
                       module-body)

  (define linked (lua-link module-path
                           imports
                           sources
                           exports
                           definitions
                           module-body))

  (define path (path-replace-extension (symbol->string module-path)
                                       ".lua"))

  (list (cons path (if (current-lua-compile-bytecode)
                       (artifact/bytes linked)
                       (artifact/string linked)))))

(define (lua-link module-path
                  imports
                  sources
                  exports
                  definitions
                  module-body)

  (define lua-module-path
    (format "local _NAME = \"~a\"" module-path))
  
  (define lua-imports
    (let ([import-decls
           (for*/list ([source (in-list sources)]
                       #:do [(define imports*
                               (hash-ref imports
                                         source
                                         #f))]
                       #:when imports*
                       [binding (in-set (hash-ref imports source))])

             (format "local ~a"
                     (rewrite-name
                      (symbol->string binding))))]

          [module-imports
           (for/list ([source (in-list sources)])
             (let* ([imports (hash-ref imports source #f)]
                    [binding-imports
                     (if imports

                         (for/list ([binding (in-set imports)])
                           (let ([binding
                                  (rewrite-name
                                   (symbol->string binding))])

                             (format "  ~a = ~a.~a"
                                     binding
                                     (rewrite-name source)
                                     binding)))

                         null)])

               (string-join

                (append (list (format "  ~a = require \"~a\""
                                      (rewrite-name source)
                                      source))

                        binding-imports)

                "\n")))])

      (if (pair? module-imports)

          (append import-decls
                  (list "do")
                  module-imports
                  (list "end"))

          null)))

  (define lua-definition-decls
    (for/list ([def (in-list definitions)])
      (format "local ~a" (rewrite-name (symbol->string def)))))

  (define lua-exports
    (let ([exports
           (for/list ([export (in-list exports)])
             (let ([export (rewrite-name
                            (symbol->string export))])
               (format "  ~a = ~a"
                       export
                       export)))])

      (if (pair? exports)

          (list "return {"
                (string-join exports ",\n")
                "}")

          null)))

  (let ([source (string-join (append (list lua-module-path)
                                     lua-imports
                                     lua-definition-decls
                                     (list (string-join module-body "\n"))
                                     lua-exports)
                             "\n")])

    (if (current-lua-compile-bytecode)
        (lua->bytecode module-path source)
        source)))

(define (lua->bytecode module-path source)

  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f
                (find-executable-path "luajit")
                "-b"
                "-s"
                "-X"
                "-d"
                "-n" (symbol->string module-path)
                "-e" source
                "-"))

  (subprocess-wait sp)

  (define (close-ports)
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr))

  (case (subprocess-status sp)

    [(0) (begin0 (port->bytes stdout)
           (close-ports))]

    [else (let ([msg (port->bytes stderr)])
            (close-ports)
            (error msg))]))

(define (lua-run/file _linked module-path)

  (define path (format "~a.lua" module-path))

  (call-with-values (thunk
                     (subprocess #f #f #f
                                 (find-executable-path "luajit")
                                 path))
                    lua-listen))

(define (lua-listen sp out in err)
  (define ((writer port buffer) n)
    (cond
      [(eof-object? n) (void)]
      [else (when (positive? n)
              (write-bytes buffer port 0 n)
              (flush-output port))]))

  (define thd
    (thread
     (thunk
      (let ([buffer-out (make-bytes 4096)]
            [buffer-err (make-bytes 4096)])

        (let loop ()
          (sync (choice-evt

                 (handle-evt
                  (read-bytes-avail!-evt buffer-out out)
                  (writer (current-output-port) buffer-out))

                 (handle-evt
                  (read-bytes-avail!-evt buffer-err err)
                  (writer (current-error-port) buffer-err))))

          (when (eq? 'running (subprocess-status sp))
            (loop)))

        (let flush-stdout ()
          (when (not (eq? (peek-byte out) eof))
            ((writer (current-output-port) buffer-out)
             (read-bytes-avail! buffer-out out))
            (flush-stdout)))

        (let flush-stderr ()
          (when (not (eq? (peek-byte err) eof))
            ((writer (current-error-port) buffer-err)
             (read-bytes-avail! buffer-err err))
            (flush-stderr)))

        (displayln (format "\nluajit exited with code ~a\n"
                           (subprocess-status sp)))

        (close-input-port out)
        (close-output-port in)
        (close-input-port err)))))

  (values thd out in err))
