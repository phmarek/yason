## GNUmakefile --- JSON test files

.PHONY: sync
sync:
	curl -o test.zip https://www.json.org/JSON_checker/test.zip
	rm -f *.json
	unzip -j test.zip
	chmod 644 *.json
	rm -f test.zip
	git status

## GNUmakefile ends here
