#lang racket

;; TODO try #lang hacket - haskell + racket
;; https://lexi-lambda.github.io/hackett/index.html

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

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (provide
   create-pattern-with-diacritics
   create-regexp-split-match
   regexp-normalize-match*
   regexp-normalize-split)
  
  (require
   ;; (prefix-in com: "common.rkt")
   "notes.rkt" ;; is used indeed
   "notes-reader.rkt"
   ansi-color

  ;; for string-replace
  racket/string
  )

  (define diacritic-map
    (hash "a" "[aáäàâæ]"
          "c" "[cčç]"
          "d" "[dď]"
          "e" "[eéèêë]"
          "i" "[iíîï]"
          "l" "[lĺľ]"
          "n" "[nň]"
          "o" "[oóôöœ]"
          "r" "[rŕř]"
          "s" "[sš]"
          "t" "[tť]"
          "u" "[uúûüù]"
          "y" "[yý]"
          "z" "[zž]"
          "A" "[AÁÄÀÂÆ]"
          "C" "[CČÇ]"
          "D" "[DĎ]"
          "E" "[EÉÈÊË]"
          "I" "[IÍÎÏ]"
          "L" "[LĹĽ]"
          "N" "[NŇ]"
          "O" "[OÓÔÖŒ]"
          "R" "[RŔŘ]"
          "S" "[SŠ]"
          "T" "[TŤ]"
          "U" "[UÚÛÜÙ]"
          "Y" "[YÝ]"
          "Z" "[ZŽ]"
          "ß" "ß")) ; German sharp S

  (define (string-normalize s)
    ;; Normalization Form C, Canonical Decomposition followed by Canonical
    ;; Composition:
    ;; Decompose characters and then recomposes them using canonical
    ;; equivalence. E.g., 'é' would first be split into 'e' and the combining
    ;; accent, and then recomposed back into 'é'.
    ;; Use this when you want to normalize characters to their composed forms
    ;; while still respecting canonical equivalence.
    (string-normalize-nfc s))

  (define (regexp-normalize-match* regex target-str)
    ;; (printf "[regexp-normalize-match*] regex: ~a\n" regex)
    ;; (printf "[regexp-normalize-match*] target-str : ~a\n" target-str)
    (let* ((normalized-target (string-normalize target-str)))
      ;; (printf "[regexp-normalize-match*] normalized-target: ~a\n" normalized-target)
      (regexp-match* regex normalized-target)))

  (define (regexp-normalize-split regex target-str)
    ;; (printf "[regexp-normalize-split] regex: ~a\n" regex)
    ;; (printf "[regexp-normalize-split] target-str : ~a\n" target-str)
    (let* ((normalized-target (string-normalize target-str)))
      ;; (printf "[regexp-normalize-split] normalized-target: ~a\n" normalized-target)
      (regexp-split regex normalized-target)))

  (define pattern-param (make-parameter ""))
  (define filepaths-param (make-parameter ""))

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
racket main.rkt -p title
racket main.rkt -n -p title
racket main.rkt -np title
racket main.rkt -p rackjure
racket main.rkt -e ./main.rkt -p \"\\bfile\\b\" # \\b match word boundaries
racket main.rkt -e ./main.rkt -p [[:blank:]]install([[:cntrl:]]|[[:blank:]])
racket main.rkt -e ./main.rkt -p \"[eeeee]\"
racket main.rkt -e ./main.rkt -p \"[eéèêë]\"
racket main.rkt -e \"/home/bost/der/search-notes/main.rkt /home/bost/der/search-notes/README.md\" -p subdir
"

   #:once-each
   ;; see also .spacemacs definition
   [("-e" "--exact-filepaths")
    FILEPATHS
    "List of exact filepaths on the file system."
    (filepaths-param FILEPATHS)]
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

  ;; This is the default location of the org-roam directory. See Makefile
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
             ;; b-red means 'bold and red'
             (with-colors 'b-red (lambda () (color-display ptrn)))
             (display ptrn))
         (colorize colorize-matches? display-fn
                   (cdr matches) (cdr patterns)))]))

  (define (create-pattern-with-diacritics pattern)
    (string-append*
     (map (lambda (char)
            (hash-ref diacritic-map (string char) (string char)))
          (string->list pattern))))

  (define pattern-with-diacritics (create-pattern-with-diacritics (pattern-param)))

  ;; - For regexp vs. pregexp - the same must be used also in notes.rkt;
  ;; - In contrary to the regexp-defining string in the notes.rkt no '.*' must
  ;;   be used.
  (define (create-regexp-split-match case-sensitivity diacritic-pattern)
    ;; (printf "case-sensitivity : ~a\n" case-sensitivity)
    ;; (printf "diacritic-pattern : ~a\n" diacritic-pattern)
    ;; (printf "pattern : ~a\n" pattern)
    (let* [
           (rgx (pregexp (format "(?~a:~a)"
                                 case-sensitivity diacritic-pattern)))]
      ;; (printf "diacritic-pattern : ~a\n" diacritic-pattern)
      ;; (printf "rgx : ~a\n" rgx)
      rgx))

  (define regexp-split-match (create-regexp-split-match
                              (case-sensitivity-params)
                              pattern-with-diacritics))

  (define colorize-matches? (colorize-matches-param))

  (define display-fn (if colorize-matches? color-display display))

  ((compose
    (lambda (_) (display ""))
    (curry map
           (lambda (all-file-strings)
             ;; (printf "all-file-strings : ~a\n" all-file-strings)
             (let [(relevant-file-strings (cdr all-file-strings))]
               ;; (printf "relevant-file-strings : ~a\n" relevant-file-strings)
               ;; (printf "(empty? relevant-file-strings) : ~a\n" (empty? relevant-file-strings))
               (unless (empty? relevant-file-strings)
                 (let ((first-file-string (car all-file-strings))
                       (relevant-file-strings-joined
                        (string-join relevant-file-strings "\n")))
                   (if colorize-matches?
                       (with-colors 'magenta
                         (lambda ()
                           (color-displayln first-file-string)))
                       (displayln first-file-string))
                   (colorize colorize-matches?
                             display-fn
                             (regexp-normalize-split
                              regexp-split-match relevant-file-strings-joined)
                             (regexp-normalize-match*
                              regexp-split-match relevant-file-strings-joined))
                   (printf "\n\n")))
               relevant-file-strings)))
    ;; (lambda (files) (printf "1. files:\n~a\n" files) files)
    (curry map
           (lambda (f)
             (let ((strs (call-with-input-file f
                           (lambda (input-file)
                             ;; (printf "1. input-file:\n~a\n" input-file)
                             (define expression
                               `(notes-syntax-parser
                                 ,@((compose
                                     ;; (lambda (p) (printf "13. ~a\n" p) p)
                                     (curry cons pattern-with-diacritics)
                                     ;; (lambda (p) (printf "12. ~a\n" p) p)
                                     (curry cons (case-sensitivity-params))
                                     ;; (lambda (p) (printf "11. ~a\n" p) p)
                                     (curry cons (colorize-matches-param))
                                     ;; (lambda (p) (printf "10. ~a\n" p) p)
                                     )
                                    (parse-notes add-src-location-info
                                                 input-file))))
                             ;; (printf "1. expression:\n~a\n" expression)
                             ;; (printf "1. namespace:\n~a\n" namespace)
                             (eval expression namespace)))))
               (if (empty? strs)
                   (list f)
                   (list f (string-join strs "\n\n"))))))
    ;; (lambda (files) (printf "0. files:\n~a\n" files) files)
    string-split)
   (filepaths-param)))
