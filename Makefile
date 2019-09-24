SRCS = indium*.el

LOAD_PATH = -L .

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--funcall package-initialize

.PHONY: all clean dependencies check test test-elisp test-javascript lint lint-elisp lint-javascript

all: check

dependencies: dependencies-elisp dependencies-javascript

dependencies-elisp:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--eval "(progn (setq package-check-signature nil) (package-refresh-contents))" \
	--eval "(package-install 'assess)" \
	--eval "(package-install 'company)" \
	--eval "(package-install 'undercover)" \
	--eval "(package-install 'buttercup)" \
	--eval "(package-install 'js2-mode)" \
	--eval "(package-install 'js2-refactor)" \
	--eval "(package-install 'json-process-client)" \
	--eval "(package-install 'exec-path-from-shell)"

dependencies-javascript:
	cd server && npm install

check: test lint

test: test-elisp test-javascript

test-elisp:
	TRAVIS=true $(BATCH) \
	-l buttercup \
	-l test/test-helper.el \
	-f buttercup-run-discover

test-javascript:
	cd server && ./node_modules/.bin/jasmine .

lint: lint-elisp lint-javascript

lint-elisp:
	# Byte compile all and stop on any warning or error
	$(BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile ${SRCS}

lint-javascript:
	cd server && ./node_modules/.bin/eslint .
