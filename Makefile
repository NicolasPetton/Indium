SRCS = indium*.el

LOAD_PATH = -L .

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--funcall package-initialize

SILENCE-SOME-DEPRECATION  = --load subr-x
SILENCE-SOME-DEPRECATION += --eval "(put 'when-let 'byte-obsolete-info nil)"
SILENCE-SOME-DEPRECATION += --eval "(put 'if-let 'byte-obsolete-info nil)"

.PHONY: all clean ci-dependencies check test lint

all: check

clean:
	rm -f /tmp/undercover_coveralls_report

ci-dependencies:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--funcall package-refresh-contents \
	--eval "(package-install 'company)" \
	--eval "(package-install 'undercover)" \
	--eval "(package-install 'buttercup)" \
	--eval "(package-install 'js2-mode)" \
	--eval "(package-install 'assess)" \
	--eval "(package-install 'exec-path-from-shell)"

check: test lint

test:
	TRAVIS=true $(BATCH) $(SILENCE-SOME-DEPRECATION) \
	-l buttercup \
	-l test/test-helper.el \
	-f buttercup-run-discover

lint:
	# Byte compile all and stop on any warning or error
	$(BATCH) $(SILENCE-SOME-DEPRECATION) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile ${SRCS}
