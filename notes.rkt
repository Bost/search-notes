#lang racket
(provide notes)
(require
 (for-syntax syntax/parse
             racket/function ;; curry
             racket/syntax
             syntax/id-set))

;; Filter out lines containing given string. E.g. 'labor' returns lines 2, 3, 6
#|
1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
2 incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
3 nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
4 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
5 eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
6 in culpa qui officia deserunt mollit anim id est laborum.
|#

(define-syntax (notes form)
  (syntax-parse form
    ((notes
      text:string
      case-sensitivity:string
      note:expr ...)
     (begin
       #;(printf "(syntax->list #`(note ...)): ~a\n" (syntax->list #`(note ...)))
       #;(printf "text: ~a\n" (syntax->datum #`text))
       #;(printf "filter: ~a\n" (regexp (format "^.*~a.*"
                                                (syntax->datum #`text))))
       #`(begin
           #;'(1 2 3 4)
           #;'(#,@((compose
                 (curry map number->string)
                 (curry filter (lambda (n) (odd? n))))
                '(1 2 3 4)))
           #;(define f (compose
                      (curry map bytes->string/utf-8)
                      #;(curry filter (curry regexp-match
                                             #;#px"^.*labor.*"
                                             ;; #rx"^.*labor.*"
                                             (regexp (format "^.*~a.*"
                                                             (syntax->datum #`text)))
                                             ))
                      (curry map syntax->datum)))
           '(#,@((compose
                 #;(lambda (r4) (printf "r4: list? ~s\n" (list? r4)) r4)
                 #;(lambda (r3) (printf "r3: ~s\n" r3) r3)
                 (curry map bytes->string/utf-8)
                 #;(lambda (r2) (printf "r2: ~s\n" r2) r2)
                 (curry filter (curry regexp-match
                                      #;#px"^.*labor.*" #;#rx"^.*labor.*"
                                      (regexp (format "(?~a:^.*~a.*)"
                                                      (syntax->datum #`case-sensitivity)
                                                      (syntax->datum #`text)))))
                 #;(lambda (r1) (printf "r1: ~s\n" r1) r1)
                 (curry map syntax->datum))
                (syntax->list #`(note ...)))))))))

#|
(define t1
  "aaa
bbb

ccc")

(define t2
  "aaa
bbb
ccc")

(define t3
  "aaa
bbb
ccc
")


(define t4
  "aaaa
abbb
accc


baaa
bbbb
bccc
")

(define t
  "1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
2 incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
3 nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

4 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
5 eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
6 in culpa qui officia deserunt mollit anim id est laborum.


aaa
bbb

ccc
")

(regexp-match #rx".*?(\n\n|\n$|$)" t1)
;; => '("aaa\nbbb\n\n" "\n\n")
(regexp-match #rx".*?(\n\n|\n$|$)" t2)
;; => '("aaa\nbbb\nccc" "")
(regexp-match #rx".*?(\n\n|\n$|$)" t3)
;; => '("aaa\nbbb\nccc\n" "\n")
(regexp-match #rx".*?(\n\n|\n$|$)" t4)
|#
