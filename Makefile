EMACS ?= emacs

test:
	EMACS_TEST_VERBOSE=t $(EMACS) \
		-batch \
		--load proc-hist.el \
		--load test.el \
		-f ert-run-tests-batch-and-exit
