#lang racket

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require
   ;; (prefix-in com: "common.rkt")
   "notes.rkt" ;; is used indeed
   "notes-reader.rkt"
   ansi-color)

  (define pattern (make-parameter ""))
  (define files-prm (make-parameter (list)))

  (define case-sensitive "i")
  (define case-insensitive "-i")

  (define case-sensitivity (make-parameter case-sensitive))

  (define csd (format "case-sensitive `~a`~a or case-insensitive `~a`~a search."
                      case-sensitive
                      (if (equal? case-sensitive (case-sensitivity)) " (default)" "")
                      case-insensitive
                      (if (equal? case-sensitive (case-sensitivity)) "" " (default)")))

  (command-line
   #:program "search-notes"

   #:usage-help
   "Search in note-file(s) for a pattern. Return note-block(s).
E.g.:
racket main.rkt \\
       -f '\\'(\"/home/bost/dev/notes/org-roam/20210805195404-guix.org\"
              \"/home/bost/dev/notes/org-roam/20210729234518-racket.org\")' \\
       -p title
"
   #:once-each
   ;; TODO parameterize displayed color
   ;; TODO check if the case-sensitivity value is allowed
   [("-f" "--files") fs
                     "A list of file to search through."
                     (files-prm fs)]
   [("-c" "--case-sensitivity") cs
                                (csd)
                                (case-sensitivity cs)]
   [("-p" "--pattern") NAME
                       "Search pattern"
                       (pattern NAME)]

   ;; no other arguments are accepted
   #;#;#;#:args () (void))

  (define-namespace-anchor a)
  ;; the expression must be evaluated in a namespace.
  ;; Thanks to https://stackoverflow.com/q/16266934 for a hint
  (define ns (namespace-anchor->namespace a))
  (define (files) (eval (call-with-input-string (files-prm) read) ns))
  #;(printf "~a\n" (files))

  (define add-src-location-info #f)

  ;; TODO implement interleave
  ;; (interleave (repeat "a") [1 2 3])
  ;; =>("a" 1 "a" 2 "a" 3)
  ;; (require racket/list)
  (define (interpose elem ls)
    ;; TODO implement tail-call version of `interpose`; see also string-join
    (if (or (empty? (cdr ls)) (empty? ls))
        ls
        (append (list (car ls) elem) (interpose elem (cdr ls)))))

  (define (colorize ls pattern)
    (match ls
      [(list) (color-display "")]
      [(list l) (color-display l)]
      [_
       (let ((txt (car ls)))
         (color-display txt)
         (with-colors 'red (lambda () (color-display pattern)))
         (colorize (cdr ls) pattern))]))

  ((compose
    (lambda (_) (display ""))
    (curry map
           (lambda (file-strs)
             (let ((strs (cdr file-strs)))
               (unless (empty? strs)
                 (let* ((s (string-join strs "\n"))
                        (ptrn (pattern))
                        ;; TODO use regexp-match* instead of regexp-split.
                        ;; e.g. (regexp-match* #rx"(?i:x*)" "12X4x6")
                        (lst (regexp-split (regexp
                                            (format "(?~a:~a)"
                                                    (case-sensitivity) ptrn))
                                           s)))
                   (with-colors 'white (lambda () (color-displayln (car file-strs))))
                   (colorize lst ptrn)
                   (printf "\n\n")))
               strs)))
    #;(lambda (p) (printf "~s\n" p) p)
    (curry map
           (lambda (f)
             (let ((strs (call-with-input-file f
                           (lambda (inf)
                             (define exp
                               `(notes
                                 ,@(cons
                                    (pattern)
                                    (cons
                                     (case-sensitivity)
                                     (parse-notes add-src-location-info inf)))))
                             (eval exp ns)))))
               (if (empty? strs)
                   (list f)
                   (list f (string-join strs "\n\n")))))))
   (files)))
