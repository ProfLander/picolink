#lang racket/base

(require racket/contract
         racket/hash
         racket/port
         racket/match

         picolink/config
         picolink/backend/generic
         picolink/backend/link-ctx
         picolink/backend/run-ctx
         picolink/language/requires
         picolink/language/provides
         picolink/lua
         picolink/s-lua)

(provide (all-defined-out))

(define (love-output-name)
  (lua-output-name))

(define (love-search-paths)
  (append (list 'picolink/love/modules)
          (lua-search-paths)))

(define (love-link self ctx)

  (let* ([entry-point (link-ctx-entry-point ctx)]
         [modules     (link-ctx-modules ctx)]
         [requires    (link-ctx-requires ctx)]
         [provides    (link-ctx-provides ctx)]
         [main        (build-path "main")]

         [ctx
          (if (hash-ref modules main #f)

              ctx

              (link-ctx-update
               ctx

               #:modules
               (hash-union
                modules
                (hash main
                      (make-s-lua
                       #`(#%chunk
                          (#%block
                           (#%call (#%method (#%member io stdout)
                                             setvbuf)
                                   "no")
                           (#%call (#%method (#%member io stderr)
                                             setvbuf)
                                   "no")
                           (#%call require
                                   #,(path->string entry-point)))))))
               
               #:requires
               (hash-union
                requires
                (hash main (make-module-requires)))

               #:provides
               (hash-union
                provides
                (hash main (make-module-provides)))))])

    (lua-link (make-lua #:mode 'library
                        #:build-subdir (love-build-subdir self))
              ctx)

    (make-run-ctx ctx
                  (build-path (link-ctx-build-directory ctx)
                              (love-build-subdir self)))))

(define (love-run _self ctx)
  "Run PATH via the system Love executable."

  (define love (find-executable-path "love") )

  (unless love
    (error "unable to locate love executable"))

  (define-values (sp out in err)
    (subprocess #f #f #f love (run-ctx-artifact ctx)))

  (thread
   (lambda ()
     (let ([buffer (make-bytes 4096)])
       (let loop ()
         (let ([n (sync (choice-evt (read-bytes-avail!-evt buffer out)
                                    (read-bytes-avail!-evt buffer err)))])

           (match n
             [(? eof-object?) (void)]

             [n (when (positive? n)
                  (write-bytes buffer (current-output-port) 0 n)
                  (flush-output))])

           (when (eq? 'running (subprocess-status sp))
             (loop)))))))

  (sync sp)

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))

(struct love (build-subdir executable)
  #:transparent
  #:methods gen:backend

  [(define (output-name _self)
     (love-output-name))

   (define (search-paths _self)
     (love-search-paths))

   (define link love-link)
   (define run love-run)])

(define/contract (make-love #:build-subdir [build-subdir "love"]
                            #:executable [executable "love"])
  (->* []
       [#:build-subdir path-string?
        #:executable string?]
       love?)
  (love build-subdir executable))
