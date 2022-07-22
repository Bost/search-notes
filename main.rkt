#lang racket

;; TODO try #lang hacket - haskell + racket
;; https://lexi-lambda.github.io/hackett/index.html

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

  (define pattern-param (make-parameter ""))
  (define matching-files-param (make-parameter ""))

  (define case-sensitive "i")
  (define case-insensitive "-i")
  ;; passed directly to the regexp as a flag for case-sensitivity-params flag
  (define case-sensitivity-params (make-parameter case-sensitive))
  (define case-sensitivity-params-help-text
    (format "case-sensitive `~a`~a or case-insensitive `~a`~a search."
            case-sensitive
            (if (equal? case-sensitive (case-sensitivity-params))
                " (default)" "")
            case-insensitive
            (if (equal? case-sensitive (case-sensitivity-params))
                "" " (default)")))

  (define colorize-matches-param (make-parameter #t))
  (define colorize-matches-param-help-text
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
   ;; see also .spacemacs definition
   [("-f" "--files") REGEXP
                     "Regexp matching a list of file-names in the org-roam
directory to search in."
                     (matching-files-param REGEXP)]
   [("-c" "--case-sensitivity-params") CS
                                (case-sensitivity-params-help-text)
                                (case-sensitivity-params CS)]
   [("-n" "--no-colors")
                         (colorize-matches-param-help-text)
                         (colorize-matches-param #f)]
   [("-p" "--pattern") NAME
                       "Search pattern"
                       (pattern-param NAME)]

   ;; no other arguments are accepted
   #;#;#;#:args () (void))

  (define-namespace-anchor a)
  ;; the expression must be evaluated in a namespace.
  ;; Thanks to https://stackoverflow.com/q/16266934 for a hint
  (define namespace (namespace-anchor->namespace a))

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

  (define (colorize colorize-matches? display-fn matches patterns)
    (match matches
      [(list) (display-fn "")]
      [(list l) (display-fn l)]
      [_
       (let ((txt (car matches))
             (ptrn (car patterns)))
         (display-fn txt)
         (if colorize-matches?
             (with-colors 'red (lambda () (color-display ptrn)))
             (display ptrn))
         (colorize colorize-matches? display-fn
                   (cdr matches) (cdr patterns)))]))

  (define regexp-split-match
    (regexp (format "(?~a:~a)" (case-sensitivity-params) (pattern-param))))

  (define colorize-matches? (colorize-matches-param))

  (define display-fn (if colorize-matches? color-display display))

  ((compose
    (lambda (_) (display ""))
    (curry map
           (lambda (all-file-strings)
             (let ((relevant-file-strings (cdr all-file-strings)))
               (unless (empty? relevant-file-strings)
                 (let ((first-file-string (car all-file-strings))
                       (relevant-file-strings-joined
                        (string-join relevant-file-strings "\n")))
                   (if colorize-matches?
                       (with-colors 'white
                         (lambda ()
                           (color-displayln first-file-string)))
                       (displayln first-file-string))
                   (colorize colorize-matches?
                             display-fn
                             (regexp-split  regexp-split-match
                                            relevant-file-strings-joined)
                             (regexp-match* regexp-split-match
                                            relevant-file-strings-joined))
                   (printf "\n\n")))
               relevant-file-strings)))
    (curry map
           (lambda (f)
             (let ((strs (call-with-input-file f
                           (lambda (input-file)
                             (define expression
                               `(notes
                                 ,@((compose
                                     (curry cons (pattern-param))
                                     (curry cons (case-sensitivity-params))
                                     (curry cons (colorize-matches-param)))
                                    (parse-notes add-src-location-info
                                                 input-file))))
                             (eval expression namespace)))))
               (if (empty? strs)
                   (list f)
                   (list f (string-join strs "\n\n"))))))
    (lambda (predicate-filter-fun)
      "Return a list of files forming the search space."
      (let ([all-files (for/list ([f (in-directory dir)])
                         (path->string f))])
        (filter predicate-filter-fun all-files)))
    (lambda (regexp)
      "Return a predicate function."
      (lambda (f)
        "Always exclude 'notes.scrbl' from the search space."
        (if (equal? f (string-append dir "notes.scrbl"))
            #f
            (regexp-match (format ".*(~a).*\\.(org|scrbl)" regexp) f)))))
   (matching-files-param)))
