# Copyright (C) 2021-2023 Free Software Foundation, Inc.

# This file is NOT part of GNU Emacs.

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

PACKAGE_NAME := urgrep
PACKAGE_MAIN := $(PACKAGE_NAME).el
AUTOLOADS := $(PACKAGE_NAME)-autoloads.el
SRCS := $(filter-out $(AUTOLOADS), $(wildcard *.el))
OBJS := $(patsubst %.el,%.elc,$(SRCS))
TESTS := $(wildcard *-tests.el)

EMACS ?= emacs
export DEPS_DIR = $(shell realpath .deps)

define INSTALL_SCRIPT
(progn
  (setq package-user-dir (getenv "DEPS_DIR"))
  (package-refresh-contents)
  (if-let ((reqs (package-desc-reqs (package-buffer-info)))
           (transaction (package-compute-transaction nil reqs)))
      (progn
        (message "Installing %s..."
                 (mapconcat (quote package-desc-full-name) transaction ", "))
        (package-download-transaction transaction))
    (message "Nothing to install")))
endef
export INSTALL_SCRIPT

EMACS_DEPS := $(EMACS) \
  --eval '(setq package-user-dir (getenv "DEPS_DIR"))' \
  --eval '(package-activate-all)'

.PHONY: all
all: compile autoloads

.PHONY: compile
compile: $(OBJS)

.PHONY: autoloads
autoloads: $(AUTOLOADS)

.PHONY: install-deps
install-deps:
	@$(EMACS) -Q --batch $(PACKAGE_MAIN) --eval "$$INSTALL_SCRIPT"

$(AUTOLOADS): GENERATE_AUTOLOADS = '$\
  (package-generate-autoloads "$(PACKAGE_NAME)" default-directory)'
$(AUTOLOADS): $(SRCS)
	@echo AUTOLOAD $@
	@$(EMACS) -Q --batch \
	  --eval '(package-initialize)' \
	  --eval $(GENERATE_AUTOLOADS)

%.elc: %.el
	@echo ELC $@
	@$(EMACS_DEPS) -Q --batch \
	  $(if $(STRICT),--eval '(setq byte-compile-error-on-warn t)') \
	  -L . --funcall batch-byte-compile $<

.PHONY: run
run: all
	$(EMACS_DEPS) -Q -L . \
	  --eval '(load "$(AUTOLOADS)")'

.PHONY: lint
lint:
	@$(MAKE) --always-make STRICT=1 compile

.PHONY: check
check:
	@echo TEST $(patsubst %.el,%,$(TESTS))
	@$(EMACS_DEPS) -Q --batch \
	  -L . $(patsubst %.el,-l %,$(TESTS)) \
	  --eval '(ert-run-tests-batch-and-exit t)'

.PHONY: clean
clean:
	rm -f *.elc $(AUTOLOADS)
