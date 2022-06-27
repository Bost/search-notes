#lang racket

(module+ test
  (require rackunit
           racket/match))

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
  (define matching-files (make-parameter ""))

  (define case-sensitive "i")
  (define case-insensitive "-i")
  ;; passed directly to the regexp as a flag for case-sensitivity flag
  (define case-sensitivity (make-parameter case-sensitive))
  (define case-sensitivity-help-text
    (format "case-sensitive `~a`~a or case-insensitive `~a`~a search."
            case-sensitive
            (if (equal? case-sensitive (case-sensitivity)) " (default)" "")
            case-insensitive
            (if (equal? case-sensitive (case-sensitivity)) "" " (default)")))

  (define colorize-matches (make-parameter #t))
  (define colorize-matches-help-text
    "If omitted the result is colorized")

  (command-line
   #:program "search-notes"

   #:usage-help
   "Search in note-file(s) for a pattern. Return note-block(s).
E.g.:
racket main.rkt -f shells -p title
racket main.rkt -fp shells title
racket main.rkt -n f shells -p title
racket main.rkt -nfp shells title
racket main.rkt -f \"shells|linux\" -p title
"
   #:once-each
   ;; TODO check if the case-sensitivity value is allowed
   ;; TODO crp: read /home/bost/dev/notes/org-roam/*utf8.org
   ;; see also .spacemacs definition
   [("-f" "--files") REGEXP
                     "Regexp matching a list of file-names in the org-roam
directory to search in."
                     (matching-files REGEXP)]
   [("-c" "--case-sensitivity") CS
                                (case-sensitivity-help-text)
                                (case-sensitivity CS)]
   [("-n" "--no-colors")
                         (colorize-matches-help-text)
                         (colorize-matches #f)]
   [("-p" "--pattern") NAME
                       "Search pattern"
                       (pattern NAME)]

   ;; no other arguments are accepted
   #;#;#;#:args () (void))

  (define-namespace-anchor a)
  ;; the expression must be evaluated in a namespace.
  ;; Thanks to https://stackoverflow.com/q/16266934 for a hint
  (define ns (namespace-anchor->namespace a))

  ;; this is the default location of the org-roam directory
  ;; ~/org-roam is a symbolic link at the moment:
  ;; $ ls -l ~/org-roam
  ;; [...] /home/bost/org-roam -> /home/bost/dev/notes/notes
  (define dir (format "~a/org-roam/" (getenv "HOME")))

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

  (define (colorize cm display-fn ls pattern)
    (match ls
      [(list) (display-fn "")]
      [(list l) (display-fn l)]
      [_
       (let ((txt (car ls)))
         (display-fn txt)
         (if cm
             (with-colors 'red (lambda () (color-display pattern)))
             (display pattern))
         (colorize cm display-fn (cdr ls) pattern))]))

  ((compose
    (lambda (_) (display ""))
    (curry map
           (lambda (file-strs)
             (let ((strs (cdr file-strs)))
               (unless (empty? strs)
                 (let* ((cm (colorize-matches))
                        (s (string-join strs "\n"))
                        (ptrn (pattern))
                        ;; TODO use regexp-match* instead of regexp-split.
                        ;; e.g. (regexp-match* #rx"(?i:x*)" "12X4x6")
                        (lst (regexp-split (regexp
                                            (format "(?~a:~a)"
                                                    (case-sensitivity) ptrn))
                                           s)))
                   (if cm
                       (with-colors 'white (lambda ()
                                             (color-displayln (car file-strs))))
                       (displayln (car file-strs)))
                   (colorize cm (if cm color-display display) lst ptrn)
                   (printf "\n\n")))
               strs)))
    (curry map
           (lambda (f)
             (let ((strs (call-with-input-file f
                           (lambda (inf)
                             (define exp
                               `(notes
                                 ,@((compose
                                     (curry cons (pattern))
                                     (curry cons (case-sensitivity))
                                     (curry cons (colorize-matches)))
                                    (parse-notes add-src-location-info inf))))
                             (eval exp ns)))))
               (if (empty? strs)
                   (list f)
                   (list f (string-join strs "\n\n"))))))
    (lambda (filter-fun)
      "Return a list of files to search through."
      (let ([all-files (for/list ([f (in-directory dir)])
                         (path->string f))])
        (filter filter-fun all-files)))
    (lambda (regexp)
      "Return a predicate function."
      (lambda (f) (regexp-match (format ".*(~a).*\\.(org|scrbl)" regexp) f))))
   (matching-files)))
