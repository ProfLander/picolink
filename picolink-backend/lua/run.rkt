#lang racket/base

(require racket/port
         picolink/config)

(provide (all-defined-out))

(define (lua-run/chunk exe _build-directory linked)
  (subprocess #f #f #f exe "-e" linked))

(define (lua-run/collection exe build-directory linked)
  (subprocess #f #f #f exe
              "-e"
              (format "package.path = \"~a/\" .. package.path"
                      build-directory)
              "-l"
              linked))

(define (lua-listen exe-name sp out in err)
  (thread
   (lambda ()
     
     (define ((writer port buffer) n)
       (cond
         [(eof-object? n) (void)]
         [else (when (positive? n)
                 (write-bytes buffer port 0 n)
                 (flush-output port))]))

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

       (displayln (format "~a exited with code ~a"
                          exe-name
                          (subprocess-status sp)))

       (close-input-port out)
       (close-output-port in)
       (close-input-port err)))))
