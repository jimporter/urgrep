EMACS=emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l urgrep -l urgrep-tests \
	--eval '(ert-run-tests-batch-and-exit t)'
