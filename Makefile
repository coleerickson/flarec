all: clean matchRegex

clean:
	jbuilder clean
	rm -rf bin

matchRegex:
	jbuilder build src/ounit_tests/match_regex/matchRegex.exe && \
	mkdir -p bin && \
	cp _build/default/src/ounit_tests/match_regex/matchRegex.exe bin/matchRegex.exe 

test:
	jbuilder build src/ounit_tests/ounitTests.exe && \
	_build/default/src/ounit_tests/ounitTests.exe
