all: clean matchRegex

clean:
	jbuilder clean
	rm -rf bin
	(cd rust-flarec && \
	cargo clean)
	rm *.o

matchRegex:
	jbuilder build src/match_regex/matchRegex.exe && \
	mkdir -p bin && \
	cp _build/default/src/match_regex/matchRegex.exe bin/matchRegex.exe

flarec:
	jbuilder build src/flarec/flarec.exe && \
	mkdir -p bin && \
	cp _build/default/src/flarec/flarec.exe bin/flarec.exe

regexc:
	jbuilder build src/regexc/regexc.exe && \
	mkdir -p bin && \
	cp _build/default/src/regexc/regexc.exe bin/regexc.exe

test:
	jbuilder build src/ounit_tests/ounitTests.exe && \
	_build/default/src/ounit_tests/ounitTests.exe -verbose

regexmatcher.ll: regexc
	bin/regexc.exe "/(0|1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)/" regexmatcher.ll && \
	dot -Tpng 0-0-dfa.dot -o out-d.png && \
	dot -Tpng 0-0-nfa.dot -o out-n.png && \
    (cat regexmatcher.ll | grep -v source_filename) > regexmatcher.ll.tmp && \
	mv regexmatcher.ll.tmp regexmatcher.ll

regexmatcherbinary: regexmatcher.ll
	clang -o regexmatcherbinary regexmatcher.ll

a.o: a.ll
	clang -c a.ll

cmain.o: cmain.c
	clang -c cmain.c

rustlib:
	(cd rust-flarec && \
	cargo build)

sheetmatch: a.o cmain.o rustlib
	clang -o sheetmatch cmain.o a.o ./rust-flarec/target/debug/librustflareclib.so
