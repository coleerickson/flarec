jbuilder utop src/flare -- -init test.ocamlinit
echo "Done with OCaml; creating image"
dot -Tpng 0-1-dfa.dot -o out.png

