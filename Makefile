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

EMACS_BATCH := $(EMACS) -Q --batch \
  --eval '(setq package-user-dir (getenv "DEPS_DIR"))' \
  --eval '(package-activate-all)'

OBJS := $(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all
all: $(OBJS)

.PHONY: install-deps
install-deps:
	@$(EMACS) -Q --batch urgrep.el --eval "$$INSTALL_SCRIPT"

%.elc: %.el
	@echo ELC $@
	@$(EMACS_BATCH) \
	  $(if $(STRICT),--eval '(setq byte-compile-error-on-warn t)') \
	  -L . --funcall batch-byte-compile $<

.PHONY: lint
lint:
	@$(MAKE) --always-make STRICT=1 all

.PHONY: check
check:
	$(EMACS) -Q --batch \
	  --eval '(setq package-user-dir (getenv "DEPS_DIR"))' \
	  --eval '(package-activate-all)' \
	  -L . -l urgrep-tests \
	  --eval '(ert-run-tests-batch-and-exit t)'

.PHONY: clean
clean:
	rm -f *.elc
