all: clean matchRegex

clean:
	jbuilder clean

matchRegex:
	jbuilder build src/ounit_tests/match_regex/matchRegex.exe

test:
	jbuilder build src/ounit_tests/ounitTests.exe && \
	_build/default/src/ounit_tests/ounitTests.exe
