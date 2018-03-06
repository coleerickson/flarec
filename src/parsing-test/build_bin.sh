ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core main.native && \
./main.native test1.json
