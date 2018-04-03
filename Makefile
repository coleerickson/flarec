all: clean matchRegex

clean:
	jbuilder clean
	rm -rf bin

matchRegex:
	jbuilder build src/match_regex/matchRegex.exe && \
	mkdir -p bin && \
	cp _build/default/src/match_regex/matchRegex.exe bin/matchRegex.exe

flarec:
	jbuilder build src/flarec/flarec.exe && \
	mkdir -p bin && \
	cp _build/default/src/flarec/flarec.exe bin/flarec.exe

test:
	jbuilder build src/ounit_tests/ounitTests.exe && \
	_build/default/src/ounit_tests/ounitTests.exe
