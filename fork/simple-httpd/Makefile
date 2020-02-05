.POSIX:
EMACS = emacs
BATCH = $(EMACS) -batch -Q -L .

compile: simple-httpd.elc simple-httpd-test.elc

test: check
check: simple-httpd-test.elc
	$(BATCH) -l simple-httpd-test.elc -f ert-run-tests-batch

clean:
	rm -f simple-httpd.elc simple-httpd-test.elc

simple-httpd-test.elc: simple-httpd-test.el simple-httpd.elc

.SUFFIXES: .el .elc
.el.elc:
	$(BATCH) -f batch-byte-compile $<
