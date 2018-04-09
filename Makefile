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

a.out: flarec
	bin/flarec.exe /hey/ && \
    (cat a.ll | grep -v source_filename) > a.ll.tmp && \
	mv a.ll.tmp a.ll && \
	clang a.ll	

test:
	jbuilder build src/ounit_tests/ounitTests.exe && \
	_build/default/src/ounit_tests/ounitTests.exe