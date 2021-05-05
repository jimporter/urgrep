EMACS=emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l urgrep -l urgrep-test \
	--eval '(ert-run-tests-batch-and-exit t)'
