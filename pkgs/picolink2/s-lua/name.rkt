#lang racket/base

(require racket/string)

(provide rewrite-name)

(define number->lua-name
  (hash
   "0"  "_zero_"
   "1"  "_one_"
   "2"  "_two_"
   "3"  "_three_"
   "4"  "_four_"
   "5"  "_five_"
   "6"  "_six_"
   "7"  "_seven_"
   "8"  "_eight_"
   "9"  "_nine_"))

(define reserved-symbol->lua-name
  (hash
   "="  "_eq_"
   "~"  "_tilde_"
   ":"  "_colon_"
   "::" "_2colon_"
   "+"  "_plus_"
   "-"  "_"
   "*"  "_star_"
   "/"  "_slash_"
   "^"  "_caret_"
   "%"  "_percent_"
   "#"  "_hash_"
   "."  "_dot_"
   ".." "_2dot_"
   "<"  "_lt_"
   "<=" "_le_"
   ">"  "_gt_"
   ">=" "_ge_"
   "==" "_2eq_"
   "~=" "_ne_"
   "?"  "_question_"
   "!"  "_exclamation_"
   "λ"  "_lambda_"))

(define reserved-identifier->lua-name
  (hash
   "break"    "_break_"
   "goto"     "_goto_"
   "do"       "_do_"
   "while"    "_while_"
   "repeat"   "_repeat_"
   "until"    "_until_"
   "if"       "_if_"
   "for"      "_for_"
   "function" "_function_"
   "local"    "_local_"
   "then"     "_then_"
   "elseif"   "_elseif_"
   "else"     "_else_"
   "return"   "_return_"
   "nil"      "_nil_"
   "false"    "_false_"
   "true"     "_true_"
   "and"      "_and_"
   "or"       "_or_"
   "not"      "_not_"
   "length"   "_length_"))

(define (rewrite-name name)
  (let* ([name (for/fold ([acc name])
                         ([(from to) (in-hash
                                      number->lua-name)])
                 (if (string-prefix? acc from)
                     (string-replace acc from to #:all #f)
                     acc))]

         [name (for/fold ([acc name])
                         ([(from to) (in-hash
                                      reserved-symbol->lua-name)])
                 (string-replace acc from to))]
         
         [name (for/fold ([acc name])
                         ([(from to) (in-hash
                                      reserved-identifier->lua-name)])
                 (if (equal? acc from)
                     to
                     acc))])
    name))
