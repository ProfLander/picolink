#lang racket/base

(require racket/match
         racket/function)

(provide (all-defined-out))

; Thread-safe + REPL-friendly deferred output

(define (make-deferred-output-port)

  (define reversed-events null)

  (define (record! event)
    (set! reversed-events
          (cons event reversed-events)))

  (define out
    (make-output-port
     'deferred-output
     always-evt

     (λ (bs start end _non-block? _breakable?)
       (define len (- end start))

       (unless (zero? len)
         (record!
          (list 'bytes
                (bytes->immutable-bytes
                 (subbytes bs start end))))

       len))

     void))

  (port-print-handler
   out
   (λ (value _dest [quote-depth 0])
     (record! (list 'print value quote-depth))))

  (values out
          (thunk
           (reverse reversed-events))))


(define (replay-deferred-output events [dest (current-output-port)])
  (for ([event (in-list events)])
    (match event
      [(list 'bytes bs)
       (write-bytes bs dest)]

      [(list 'print value quote-depth)
       (print value dest quote-depth)])))
