#lang racket

(module+ test
  (require
   ;; main
   rackunit
   racket/match
   "main.rkt"
   (submod "main.rkt" main)
   )

  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  ;; (test-case "Test for add function"
  ;;   (check-equal? (+ 2 2) 4))

  (test-case "Test diacritics"
    (define sdiacr "jkl \n abčd \n xyz \n 123 \n ábc \n 567")
    (define sd sdiacr)
    (define splain "jkl \n abcd \n xyz \n 123 \n abc \n 567")
    (define sp splain)
    (define rxs "abc")
    (define diarxs (create-regexp-split-match
                    "i"
                    (create-pattern-with-diacritics rxs)))

    (check-equal? (length (regexp-normalize-match* diarxs sdiacr))
                  (length (regexp-match* rxs splain)))
    (check-equal? (regexp-normalize-split diarxs sdiacr)
                  (regexp-split rxs splain))
    (check-equal? (regexp-normalize-split diarxs splain)
                  (regexp-split rxs splain))))
