search-notes
============
Installation:
```shell
# Fix the 'loading code: version mismatch' error
rm -rf ./compiled/ ./scribblings/compiled/
raco pkg install --auto ansi-color
raco exe -o ~/bin/search-notes main.rkt
```

See main.rkt -> command-line -> #:usage-help

TODO When inside source-block BEGIN\_SRC END\_SRC then the block is continuous text.
When outside of the source-block then the block is between two org-mode '* ...',
'* ...' sections

TODO Two search patterns means
- search for block(s)
- then search within the found block(s)

TODO Find org-mode racket parser / language: https://github.com/tgbugs/laundry

TODO search in subdirectories
