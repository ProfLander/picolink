#lang picolink/backend

(require racket/function
         racket/string
         racket/list
         racket/hash
         racket/set
         racket/path
         racket/file
         racket/syntax

         (except-in syntax/parse expr)

         picolink/ee-lib
         picolink/config
         picolink/s-lua
         picolink/s-lua/lua
         picolink/module/compiled
         picolink/module/import

         (for-template racket/base
                       picolink/s-lua/ir
                       picolink/lua/run))

(provide (all-defined-out)
         (for-template (all-from-out picolink/lua/run)))

#:target    lua-target
#:modules   lua-modules
#:transform lua-transform
#:link      lua-link
#:run       lua-run
#:interact  lua-interact

(struct lua-module [name
                    path
                    ident-prefix
                    implicit]
  #:transparent)

(define (make-lua-module name
                         #:path path
                         #:ident-prefix [ident-prefix ""]
                         #:implicit [implicit #t])

  (lua-module name path ident-prefix implicit))

(define (lua-module-name=? self ident)
  (equal? (syntax-e (lua-module-name self))
          (syntax-e ident)))

(define current-lua-modules
  (make-parameter
   (list (make-lua-module
          #'lua
          #:path #'picolink/lua/modules/lua)

         (make-lua-module
          #'lua/coroutine
          #:path #'picolink/lua/modules/lua/coroutine
          #:ident-prefix "coroutine")

         (make-lua-module
          #'lua/debug
          #:path #'picolink/lua/modules/lua/debug
          #:ident-prefix "debug")

         (make-lua-module
          #'lua/io
          #:path #'picolink/lua/modules/lua/io
          #:ident-prefix "io")

         (make-lua-module
          #'lua/math
          #:path #'picolink/lua/modules/lua/math
          #:ident-prefix "math")

         (make-lua-module
          #'lua/os
          #:path #'picolink/lua/modules/lua/os
          #:ident-prefix "os")

         (make-lua-module
          #'lua/package
          #:path #'picolink/lua/modules/lua/package
          #:ident-prefix "package")

         (make-lua-module
          #'lua/string
          #:path #'picolink/lua/modules/lua/string
          #:ident-prefix "string")

         (make-lua-module
          #'lua/table
          #:path #'picolink/lua/modules/lua/table
          #:ident-prefix "table"))))

(define (subprocess-ident lctx)
  (datum->syntax lctx '#%subprocess))

(define (stdout-ident lctx)
  (datum->syntax lctx '#%stdout))

(define (stdin-ident lctx)
  (datum->syntax lctx '#%stdin))

(define (stderr-ident lctx)
  (datum->syntax lctx '#%stderr))

(define lua-target
  (make-parameter 's-lua))

(define (lua-modules)
  (for/list ([lua-mod (in-list (current-lua-modules))])
    (cons (lua-module-name lua-mod)
          (lua-module-path lua-mod))))

(define (lua-transform lctx prog)
  (let ([transform-mod (current-lua-transform-module)])
    (for/list ([mod (in-list prog)]
               #:when (compiled-module-stx mod))

      (transform-mod lctx prog mod))))

(define (lua-transform-module _lctx prog mod)
  (let* ([is-entry-point (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point)]
         [mod (rewrite-intrinsics prog mod)]
         [mod (splice-requires+provides mod)]
         [mod (if is-entry-point
                  (splice-unbuffered-stdio mod)
                  mod)]
         [mod (if is-entry-point
                  (splice-repl mod)
                  mod)])

    mod))

(define (lua-link lctx prog)

  (define linker (current-lua-linker))

  (unless linker
    (error "no linker specified"))

  (linker lctx prog))

(define (dotted-path->member-access path)
  (define segs (map string->symbol (string-split path ".")))

  (for/foldr ([acc (last segs)])
             ([seg (in-list (drop-right segs 1))])
    (with-syntax ([acc acc]
                  [seg seg])
      #`(#%member seg acc))))

(define (module-ident->lua-ident path)
  (string->symbol
   (string-replace
    (rewrite-name path)
    "/" "_")))

(define (module-ident->lua-string path)
  (string-replace
   (symbol->string (syntax-e path)) "/" "."))

(define (splice-requires+provides mod)

  (define require-scope (compiled-module-require-scope mod))

  (define reqs
    (for/list ([req (in-scope require-scope)]
               #:do [(define mod-path (import-src-mod-path req))
                     (define lua-mods (current-lua-modules))
                     (define lua-mod
                       (findf (curryr lua-module-name=? mod-path)
                              lua-mods))]
               #:unless (and lua-mod
                             (lua-module-implicit lua-mod)))
      req))

  (define req-mods (map import-src-mod-path reqs))

  (define req-mod-syms
    (set-map req-mods module-ident->lua-ident))

  (define req-mod-strs
    (set-map req-mods module-ident->lua-string))

  (define module-bindings
    (with-syntax ([(req-mod-sym ...) req-mod-syms]
                  [(req-mod-str ...) req-mod-strs])

      #'(#%local [req-mod-sym ...]
                 [(#%call require req-mod-str) ...])))

  (define mod-req-binds
    (for*/fold ([acc null])
               ([req (in-list reqs)])
      (let ([src-mod-path (import-src-mod-path req)]
            [from (import-src-sym req)]
            [to (import-local-id req)])
        (cons (list (module-ident->lua-ident src-mod-path)
                    from
                    to)
              acc))))

  (define member-bindings
    (with-syntax ([((req-bind-mod req-bind-from req-bind-to) ...)
                   mod-req-binds])

      #'(#%local [req-bind-to ...]
                 [(#%member req-bind-mod
                            req-bind-from) ...])))

  (define provide-scope
    (compiled-module-provide-scope mod))

  (define prov-idents
    (scope-idents provide-scope))

  (define provide-return
    (with-syntax ([(prov ...) prov-idents])
      #'(#%return (#%table [prov prov] ...))))

  (define stx
    (syntax-parse (compiled-module-stx mod)
      #:literal-sets [s-lua-ir-literals]

      [(#%chunk (#%block body ...))

       (with-syntax ([(body ...)
                      (append
                       (if (pair? reqs)
                           (list module-bindings
                                 member-bindings)
                           null)

                       (attribute body)

                       (if (pair? prov-idents)
                           (list provide-return)
                           null))])

         #'(#%chunk (#%block body ...)))]))

  (compiled-module-update mod #:stx stx))

(define (rewrite-intrinsics _prog mod)

  (define require-scope (compiled-module-require-scope mod))

  (define rewrites
    (for/fold ([acc (hash)])
              ([import (in-scope require-scope)]
               #:do [(define mod-path (import-src-mod-path import))
                     (define lua-mods (current-lua-modules))
                     (define lua-mod
                       (findf (curryr lua-module-name=? mod-path)
                              lua-mods))]
               #:when (and lua-mod
                           (lua-module-implicit lua-mod)))

      (define prefix (lua-module-ident-prefix lua-mod))

      (define ident (import-local-id import))

      (define sym (syntax-e ident))

      (define rewrite
        (dotted-path->member-access
         (string-append prefix "." (symbol->string sym))))

      (hash-set acc sym rewrite)))

  (define rewrite
    (syntax-parser
      [(form ...)
       (datum->syntax this-syntax (map rewrite (attribute form)))]
      [ident:id
       #:do [(define rewrite (hash-ref rewrites (syntax-e #'ident) #f))]
       #:when rewrite
       (datum->syntax this-syntax rewrite)]
      [_ this-syntax]))

  (let* ([stx (compiled-module-stx mod)]
         [stx (rewrite stx)])
    (compiled-module-update mod #:stx stx)))

(define (lift-package-preloader path stx)
  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]

    [(#%chunk (#%block body ...))
     (with-syntax ([path (symbol->string (syntax-e path))])
       #'(#%assign
          [(#%member (#%member package preload)
                     path)]
          [(#%function
            (#%vararg)
            (#%block
             body ...))]))]))

(define (s-lua-append a b)
  (syntax-parse a
    #:datum-literals [#%chunk #%block #%return]
    [(#%chunk (#%block (~and body-a (~not (#%return _))) ...
                       (~optional (#%return ret-a))))

     (syntax-parse b
       #:datum-literals [#%chunk #%block #%return]
       [(#%chunk (#%block (~and body-b (~not (#%return _))) ...
                          (~optional (#%return ret-b))))
        (when (and (attribute ret-a)
                   (attribute ret-b))
          (error "cannot append s-lua chunks with multiple returns"))

        (with-syntax ([(ret ...) (cond
                                   [(attribute ret-a)
                                    (list #'(#%return ret-a))]
                                   [(attribute ret-b)
                                    (list #'(#%return ret-b))]
                                   [else null])])
          #'(#%chunk
             (#%block
              body-a ...
              body-b ...
              ret ...)))])]))

(define stx-unbuffered-stdio
  #'(#%chunk
     (#%block

      (#%call (#%method (#%member io stdout)
                        setvbuf)
              "no")

      (#%call (#%method (#%member io stderr)
                        setvbuf)
              "no"))))

(define (splice-unbuffered-stdio mod)
  (compiled-module-update
   mod
   #:stx (s-lua-append (compiled-module-stx mod)
                       stx-unbuffered-stdio)))

(define stx-repl
  #'(#%chunk
     (#%block

      (#%while #t

               (#%block
                (#%local [input] [(#%call (#%member io read))])
                (#%local [res err] [(#%call loadstring
                                            input
                                            "repl")])
                (#%if res

                      (#%then
                       (#%block
                        (#%call res)))

                      (#%else
                       (#%block
                        (#%call
                         (#%method (#%member io stderr)
                                   write)
                         err)))))))))

(define (splice-repl mod)
  (compiled-module-update
   mod
   #:stx
   (s-lua-append (compiled-module-stx mod) stx-repl)))

(define (program-entry-point prog)
  (findf (λ (mod)
           (equal? 'entry-point
                   (syntax-e
                    (compiled-module-ident mod))))
         prog))

(define (lua-link/chunk _lctx prog)

  (let* ([entry-point (program-entry-point prog)]
         
         [dependencies
          (for/list ([mod (in-list prog)]
                     #:when (not (eq? mod entry-point)))

            (lift-package-preloader (compiled-module-ident mod)
                                    (compiled-module-stx mod)))]

         [stx
          (with-syntax ([(dependency ...) dependencies]
                        [(entry-point-body ...)
                         (syntax-parse (compiled-module-stx
                                        entry-point)
                           #:literal-sets [s-lua-ir-literals]
                           [(#%chunk (#%block body ...))
                            (attribute body)])])
            #'(#%chunk
               (#%block
                dependency ...
                entry-point-body ...)))]

         [_ (println stx)]

         [compile (s-lua-compiler 'lua)]
         [compiled (compile stx)])

    (displayln "lua:")
    (displayln compiled)
    (displayln "")

    compiled))

(define (lua-link/collection lctx prog)

  (define config (syntax-local-config lctx))

  (define compile (s-lua-compiler 'lua))

  (let ([modules (for/hash ([mod (in-list prog)])

                   (let* ([name (symbol->string
                                 (syntax-e
                                  (compiled-module-ident mod)))]
                          [rename (current-lua-entry-point-name)]
                          [name (if (and rename
                                         (equal? name "entry-point"))
                                    rename
                                    name)]
                          [path (string->path name)])
                     (values (path-replace-extension path ".lua")
                             (compile (compiled-module-stx mod)))))]

        [build-directory (build-path (config-build-directory config)
                                     (current-lua-build-subdir))])

    (delete-directory/files build-directory #:must-exist? #f)
    (make-directory* build-directory)

    (for ([(path mod) (in-hash modules)])
      (let ([path (build-path build-directory path)])
        (make-directory* (path-only path))
        (display-to-file mod path #:exists 'replace)
        (flush-output)))

    (symbol->string
     (syntax-e
      (compiled-module-ident
       (program-entry-point prog))))))

(define (lua-run lctx prog)

  (define exe-name (current-lua-executable))

  (define exe (find-executable-path exe-name) )

  (unless exe
    (error (format "unable to locate ~a executable" exe-name)))

  (with-syntax* ([#%subprocess (subprocess-ident lctx)]
                 [#%stdout (stdout-ident lctx)]
                 [#%stdin (stdin-ident lctx)]
                 [#%stderr (stderr-ident lctx)]
                 [exe-name exe-name]
                 [exe exe]
                 [prog prog]
                 [do-run
                  (let ([config (syntax-local-config lctx)])
                    (with-syntax ([runner (current-lua-runner)]
                                  [build-directory
                                   (build-path
                                    (config-build-directory config)
                                    (current-lua-build-subdir))])
                      #'(runner exe build-directory prog)))])
    #'(begin
        (define-values (#%subprocess #%stdout #%stdin #%stderr)
          do-run)

        (lua-listen exe-name #%subprocess #%stdout #%stdin #%stderr))))

(define (lua-interact lctx stx)

  (define compiler (s-lua-compiler 'lua))

  (let ([interaction (parameterize ([current-separator "; "])
                       (compiler stx))])

    (with-syntax ([#%stdin (stdin-ident lctx)]
                  [interaction interaction])

      #'(begin
         (when (port-closed? #%stdin)
           (raise-user-error
            (format "lua port is closed\n  port: ~a" #%stdin)))

         (displayln interaction #%stdin)
         (flush-output #%stdin)))))

; Parameters

(define current-lua-executable
  (make-parameter "lua"))

(define current-lua-build-subdir
  (make-parameter "lua"))

(define current-lua-linker
  (make-parameter #f))

(define current-lua-runner
  (make-parameter #f))

(define current-lua-entry-point-name
  (make-parameter #f))

(define current-lua-transform-module
  (make-parameter lua-transform-module))

(define (lua-chunk-mode)
  (current-lua-linker lua-link/chunk)
  (current-lua-runner #'lua-run/chunk))

(define (lua-collection-mode)
  (current-lua-linker lua-link/collection)
  (current-lua-runner #'lua-run/collection))

(lua-chunk-mode)
