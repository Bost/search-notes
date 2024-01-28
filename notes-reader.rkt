#lang racket
(provide (rename-out (notes-read-syntax read-syntax))
         parse-notes)
(require syntax/readerr)

(define (notes-read-syntax src in)
  (datum->syntax
   #f
   `(module notes racket
      (require "notes.rkt")
      (notes
       ,@(parse-notes src in)))))

;; `src` - context information about what file this code resides in. The value
;; is provided by the racket system
;; `in` - input-port - a reference to an open file "or something like that"[sic.]
;; (parse-notes src in)
(define (parse-notes src in)
  (define note (parse-note src in))
  (if (eof-object? note)
      '()
      (let [(notes (parse-notes src in))]
        ;; (printf "vvvvvvvvvvv note:\n~a\n^^^^^^^^^^^^^^^^^\n" note)
        ;; (printf "vvvvvvvvvvv notes:\n~a\n^^^^^^^^^^^^^^^^^\n\n" notes)
        (cons note notes))))

(define (parse-note src in)
  ;; Don't remove whitespace for better formatting (column alignment)
  ;; See also `next-token`
  #;(regexp-try-match #px"^\\s+" in)
  (if (eof-object? (peek-char in))
      eof
      (let ()
        (define note-content (parse-note-content src in))
        note-content)))

(define (skip-whitespace in)
  (regexp-try-match #px"^[ \t]+" in))

(define (complain src in msg)
  (define-values (line col pos) (port-next-location in))
  (raise-read-error msg src line col pos 1))

(define (last-index-of str substr)
  (let* ((positions (regexp-match-positions
                     (regexp (format "(~a).*" substr))
                     str))
         (end (if positions (cdar positions) -1))
         (lio (if positions (- end (string-length substr)) -1)))
    lio))

(define (next-token src in (peek? #f))
  ;; Don't remove whitespace for better formatting (column alignment)
  ;; See also `parse-note`
  #;(skip-whitespace in)
  (define match-fn (if peek? regexp-match-peek regexp-try-match))
  (cond
    ;; note is a block of lines
    ((match-fn #rx".*?(\n\n|\n$|$)" in)
     => (lambda (match-raw)
          (let* ((match (map bytes->string/utf-8 match-raw))
                 (fst (car match))
                 (snd (cadr match)))
            ((compose
              string->bytes/utf-8
              (lambda (substr-index)
                (if (positive-integer? substr-index)
                    (substring fst 0 substr-index)
                    fst)))
             (last-index-of fst snd)))))

    ((eof-object? (peek-char in))
     eof)
    ((equal? #\newline (peek-char in))
     (read-char in)
     eof)
    ((match-fn "^$" in)
     eof)
    (else
     (complain src in "unknown lexeme"))))

(define (parse-note-content src in)
  (define first-token (next-token src in))
  (when (eof-object? first-token)
    (error "eof detected"))
  (cond
    #;((eq? 'PRINT first-token)
     `(found ,@(parse-arguments src in)))
    (#t
     first-token)))
