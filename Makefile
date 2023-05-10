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

.PHONY: install-deps
install-deps:
	@$(EMACS) -Q --batch urgrep.el --eval "$$INSTALL_SCRIPT"

.PHONY: check
check:
	$(EMACS) -Q --batch \
	  --eval '(setq package-user-dir (getenv "DEPS_DIR"))' \
	  --eval '(package-activate-all)' \
	  -L . -l urgrep -l urgrep-tests \
	  --eval '(ert-run-tests-batch-and-exit t)'
