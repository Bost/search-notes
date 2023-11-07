# pipefail is not POSIX complaint

# Export these variables for all commands in this Makefile
# the 'export' seems not to be necessary
$(eval isGuix := $(shell command -v guix > /dev/null 2>&1 && echo t || echo f))
$(eval destDir := $(shell [ "${isGuix}" = t ] && echo $${dotf}/bin || echo ~/bin))
orgRoamLink := ${HOME}/org-roam

all: show-environment clean install-deps
	[ ! -L "${orgRoamLink}" ] && ln -s ${dev}/notes/notes "${orgRoamLink}" || :
	[ ! -d ${destDir} ] && mkdir ${destDir} || :
	raco exe -o ${destDir}/search-notes main.rkt
	[ "${isGuix}" = t ] && gxhre --cores=${cores} || :

	@# both can be used: ${...} also $(...)
	@# commands must be terminated by '; \'
	@# @if [ "$(isGuix)" = t ]; then \
	# 	@# echo '# => $$isGuix: ${isGuix}; true branch'; \
	# else \
	# 	@# echo '# => $$isGuix: ${isGuix}; false branch'; \
	# 	: ; \
	# fi

show-environment:
	@echo '### => $$isGuix: ${isGuix}'
	@echo '### => $$destDir: ${destDir}'

install-deps:
	@# without --skip-installed the this target fails.
	raco pkg install --skip-installed --auto ansi-color

clean:
	rm -rf ./compiled/ ./scribblings/compiled/

