search-notes
============
Installation: run `make`. See also Makefile.
```bash
# Fix the 'loading code: version mismatch' error
rm -rf ./compiled/ ./scribblings/compiled/
raco pkg install --auto ansi-color
isGuix=$(command -v guix > /dev/null 2>&1 && echo t || echo f)
[ ${isGuix} = t ] && destDir=$dotf/bin || destDir=~/bin
[ ! -d ${destDir} ] && mkdir $destDir || :
raco exe -o $destDir/search-notes main.rkt
[ ${isGuix} = t ] && gxhre --cores=${cores} || :
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
