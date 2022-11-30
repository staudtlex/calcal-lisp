# This is the Makefile of calcal-lisp
LISP=sbcl --no-userinit --no-sysinit

all: asd

asd: calcal.asd calendar.lisp
	@LISP_BIN=`echo $(LISP) | \
		sed -E 's/(.*\/)?(.*)/\2/g' | sed -E 's/( -+.*)//g'`; \
	if [ "$$LISP_BIN" = "clisp" ]; then \
		$(LISP) \
			-x "(require 'asdf)" \
			-x "(setf asdf:*central-registry* (list (uiop:getcwd)))" \
			-x "(if (asdf:make :calcal) (quit))"; \
	else \
		$(LISP) \
			--eval "(require 'asdf)" \
			--eval "(setf asdf:*central-registry* (list (uiop:getcwd)))" \
			--eval "(if (asdf:make :calcal) (quit))"; \
	fi

calendar.lisp: calendar.l modify.patch
	cp calendar.l calendar.lisp && \
	patch calendar.lisp < modify.patch

test: calcal
	./$<

clean-sources:
	rm -rf calendar.lisp

clean-binary:
	rm -rf calcal

clean: clean-sources clean-binary