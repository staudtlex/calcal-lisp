# This is the Makefile of calcal-lisp
LISP=sbcl --no-userinit --no-sysinit

all: asd

asd: calcal.asd calendar.lisp
	$(LISP) \
		--eval "(require 'asdf)" \
		--eval "(setf asdf:*central-registry* (list (uiop:getcwd)))" \
		--eval "(if (asdf:make :calcal) (quit))"

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