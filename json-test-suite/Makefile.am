SUBDIRS = parsers

.PHONY: check
check:
	python3 ./run_tests.py

.PHONY: update-sources
update-sources:
	cat parsers.txt | while read dir app ; do \
	  cp -vp packages.lisp main.lisp parsers/test_$$dir/ ; \
	done

## Makefile.am ends here
