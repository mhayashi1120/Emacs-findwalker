EMACS = emacs

check: compile
	$(EMACS) -q -batch -L . -l findwalker.el -l findwalker-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L . -l findwalker.elc -l findwalker-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile findwalker.el
