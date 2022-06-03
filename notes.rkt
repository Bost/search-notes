#lang racket
(provide notes)
(require
 (for-syntax syntax/parse
             racket/function ;; curry
             racket/syntax
             syntax/id-set))

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
                               (regexp (format
                                        "(?~a:^.*~a.*)"
                                        (syntax->datum #`case-sensitivity)
                                        (syntax->datum #`text)))))
                 (curry map syntax->datum))
                (syntax->list #`(note ...)))))))))
