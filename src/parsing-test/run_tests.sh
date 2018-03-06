rm *nfa.dot
ocamlbuild -pkg oUnit -use-menhir -use-ocamlfind -tag thread -pkg core ounit_tests.native -- && \
./ounit_tests.native -verbose
