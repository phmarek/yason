SUBDIRS = parsers

.PHONY: check
check:
	python3 ./run_tests.py

.PHONY: update-sources
update-sources:
	cat parsers.txt | while read dir app ; do \
	  cp -vp packages.lisp main.lisp parsers/test_$$dir/ ; \
	done

.PHONY: list-parse.lisp
list-parse.lisp:
	cat parsers.txt | while read dir app ; do \
	  line=parsers/test_$$dir/test-$$app ; \
	  echo $$line | sed 's/./=/g' ; \
	  echo $$line ; \
	  echo $$line | sed 's/./=/g' ; \
	  cat parsers/test_$$dir/parse.lisp ; \
	done

## Makefile.am ends here
