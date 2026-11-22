#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/for       for for/fold for*)
         (only r-lua/prelude/format    format)
         (only r-lua/prelude/list      list cons null for/list in-list)
         (only r-lua/prelude/sequence  in-naturals in-range)

         r-lua/prelude/number
         r-lua/prelude/string)

(for ([i (in-naturals)]
      [j (in-list (list 3 2 1))])
  (print (format "~a: ~a" i j)))

(print "")

(for/fold ([acc null])
          ([i (in-range 1 6)]
           [j (in-range 5 12 2)])
  (cons (cons i j) acc))

(print "")

(for/list ([i (in-range 1 6)]
           [j (in-range 2 12 2)])
  (cons i j))

(print "")

(for* ([i (in-range 1 3)]
       [j (in-range 0 2)])
  (print (format "~a / ~a" i j)))
