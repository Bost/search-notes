#lang racket
(provide notes)
(require
 (for-syntax syntax/parse
             racket/function ;; curry
             racket/syntax
             syntax/id-set))
;; Also the `partial` from (require rackjure) can be used
#;(require (rename-in racket/function
                    (curry partial)))

(define-syntax (notes form)
  (syntax-parse form
    ((notes
      text:string
      case-sensitivity:string
      colorize-matches:boolean
      note:expr ...)
     (begin
       #`(begin
           '(#,@((compose
                 (curry map bytes->string/utf-8)
                 (curry filter
                        (curry regexp-match
                               #;#px"^.*labor.*" #;#rx"^.*labor.*"
                               ((compose
                                 regexp
                                 #;
                                 (lambda (s)
                                   (display (format "notes: ~a\n" s))
                                   s))
                                (format
                                 ;; match the whole line containing pattern
                                 "(?~a:^.*~a.*)"
                                 (syntax->datum #`case-sensitivity)
                                 (syntax->datum #`text)))))
                 (curry map syntax->datum))
                (syntax->list #`(note ...)))))))))
